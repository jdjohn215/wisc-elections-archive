rm(list = ls())

library(tidyverse)

# this script cleans election files from 2024

# match the output format from a previous year
target <- read_csv("processed-data/annual/2022.csv")
################################################################################

# This script cleans the official Wisconsin Elections Commission reporting unit file
#   for the November 2024 general election.
#   Each office is on a different sheet of the XLSX file, so this script
#     processes each sheet, and combines them into a single CSV file 
#     with a standardized format
#   The script then adds the state assembly, state senate, & US house district
#     assignments for each reporting unit

# the list of sheets. Sheet 1 is the document map (doesn't contain election results)
orig.sheets <- readxl::excel_sheets("original-data/Ward by Ward Report_November 5 2024 General Election_Federal and State Contests.xlsx")

# this function reads each sheet individually and converts it to long format
#   with columns for county, reporting unit, office, total votes, party,
#   candidate name, and candidate votes
read_sheet <- function(sheetindex){
  # read the original sheet in its entirety
  s1 <- readxl::read_excel("original-data/Ward by Ward Report_November 5 2024 General Election_Federal and State Contests.xlsx",
                           col_names = F, .name_repair = "unique_quiet",
                           sheet = sheetindex) |>
    # remove any columns with only NA values
    janitor::remove_empty(which = "cols")
  
  # the rows containing header information
  #   header info is spread across multiple rows, so we must combine them
  header.start <- which(s1$...3 == "Total Votes Cast")
  header.end <- min(which(!is.na(s1$...2))) - 1
  
  # the cell containing the office name
  office.name <- s1$...1[header.start-2]
  
  # combine the multi-row headers into a single row
  colnames <- s1 |>
    filter(row_number() %in% header.start:header.end) |>
    mutate(rownum = row_number()) |>
    pivot_longer(cols = -rownum) |>
    pivot_wider(names_from = rownum, values_from = value) |>
    # some munging because the county and reporting unit columns aren't labelled
    #   plus the scattering column lacks party info
    mutate(`1` = case_when(
      name == "...1" ~ "county",
      name == "...2" ~ "reporting_unit",
      name == "...3" ~ "total_votes",
      `2` == "SCATTERING" ~ "NP", # "NP" = "NO PARTY"
      TRUE ~ `1`
    )) |>
    unite("colname", `1`, `2`, na.rm = T, sep = "!!") |>
    mutate(colname = str_remove_all(colname, coll("\r\n")),
           colname = str_squish(colname))
  
  # just keep the parts of the sheet with the actual vote table
  s1 |>
    filter(row_number() > header.end) |> # keep everything after the header
    set_names(colnames$colname) |> # use the new column names (processed above)
    mutate(county = zoo::na.locf(county)) |> # fill the county values
    filter(str_detect(county, "Totals", negate = T), # remove the total rows
           str_detect(reporting_unit, "Totals", negate = T)) |>
    # convert to long format
    pivot_longer(cols = -c(county, reporting_unit, total_votes),
                 names_to = "party!!candidate", values_to = "votes") |>
    # create separate party and candidate fields
    separate(col = `party!!candidate`, into = c("party", "candidate"), sep = "!!") |>
    mutate(office = office.name) |> # add the office name as a field
    # reorder the values
    select(county, reporting_unit, office, total_votes, party, candidate, votes)
}

# read every sheet and combine the results into a single dataframe
#   the map() function supplied each value in .x to the function indicated in .f
all.sheets <- map(.x = 2:length(orig.sheets),
                  .f = read_sheet,
                  .progress = TRUE) |>
  list_rbind() |>
  type_convert() |>
  # remove district attorney races & special election for CD8
  filter(str_detect(office, "DISTRICT ATTORNEY", negate = T),
         str_detect(office, coll("Term ending Jan. 3, 2025"), negate = T))

################################################################################
# add the district codes for assembly (wsa), state senate (wss), and congress (ush)
wsa.districts <- all.sheets |>
  filter(str_detect(office, "ASSEMBLY")) |>
  mutate(wsa_dist = as.numeric(word(office, -1))) |>
  group_by(county, reporting_unit, wsa_dist) |>
  summarise(.groups = "drop") |>
  # add state senate districts (wss) following the nested district assignments
  #   i.e. wsa 1-3 = wss 1, wsa 4-6 = wss 2, etc.
  inner_join(
    tibble(wsa_dist = 1:99,
           wss_dist = rep(1:33, each = 3))
  )

ush.districts <- all.sheets |>
  filter(str_detect(office, "REPRESENTATIVE IN CONGRESS")) |>
  # extract the word following "DISTRICT" in the office string
  mutate(con_dist = as.numeric(str_extract(office, '(?<=DISTRICT\\s)\\w+'))) |>
  group_by(county, reporting_unit, con_dist) |>
  summarise(.groups = "drop")

all.2024 <- all.sheets |>
  inner_join(wsa.districts) |>
  inner_join(ush.districts) |>
  # some basic string cleaning
  mutate(across(where(is.character), str_to_upper), # upper case
         across(where(is.character), str_squish)) |> # remove double white spaces
  # add district
  mutate(district = case_when(
    str_detect(office, "REPRESENTATIVE IN CONGRESS") ~ con_dist,
    str_detect(office, "ASSEMBLY") ~ wsa_dist,
    str_detect(office, "STATE SENATOR") ~ wss_dist,
    TRUE ~ NA
  )) |>
  # reformat party
  mutate(party = case_when(
    party == "DEM" ~ "Democratic",
    party == "REP" ~ "Republican",
    party == "CON" ~ "Constitution",
    party == "LIB" ~ "Libertarian",
    party == "WGR" ~ "Wisconsin Green",
    party == "IND" ~ "Independent",
    candidate == "SCATTERING" ~ "Scattering",
    str_detect(candidate, "WRITE-IN") ~ "Write-in",
    party == "NP" ~ "Independent",
    TRUE ~ party
  ))

uniquely.name.party <- all.2024 |>
  group_by(office, district, candidate, party) |> 
  summarise(votes = sum(total_votes)) |> 
  arrange(desc(votes)) |> 
  group_by(office, district, party) |>
  mutate(party = if_else(row_number() > 1, 
                         paste(party, row_number()),
                         party)) |>
  ungroup() |>
  select(district, office, candidate, party)

# add unique party names
all.2024.2 <- all.2024 |>
  select(-party) |>
  inner_join(uniquely.name.party) |>
  # reformat office names
  mutate(office = case_when(
    str_detect(office, "PRESIDENT") ~ "president",
    str_detect(office, "UNITED STATES SENATOR") ~ "senate",
    str_detect(office, "STATE SENATOR") ~ "state senate",
    str_detect(office, "ASSEMBLY") ~ "state assembly",
    str_detect(office, "CONGRESS") ~ "congress",
    TRUE ~ office
  )) |>
  separate(reporting_unit, into = c("municipality", "reporting_unit"), sep = " (?=WARD)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit), "WARD 1", reporting_unit),
         ctv = str_sub(municipality, 1, 1),
         municipality = word(municipality, 3, -1),
         candidate = str_remove(candidate, coll( "()")),
         year = 2024)

write_csv(all.2024.2, "processed-data/annual/2024.csv")

all.2024.2 |>
  group_by(office, district) |>
  summarise(total = sum(votes))

