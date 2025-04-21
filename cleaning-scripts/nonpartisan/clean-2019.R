rm(list = ls())

library(tidyverse)

# this script cleans election files from 2024

# read the original sheet in its entirety
s1 <- readxl::read_excel("original-data/nonpartisan/Spring_20Election_204.2.19-WxW_20Report-Supreme_20Court_2019.xlsx",
                         col_names = F, .name_repair = "unique_quiet",
                         sheet = 2) |>
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
s2 <- s1 |>
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
  select(county, reporting_unit, office, total_votes, party, candidate, votes) |>
  mutate(party = case_when(
    candidate == "Lisa Neubauer" ~ "liberal",
    candidate == "Brian Hagedorn" ~ "conservative",
    TRUE ~ "Scattering"
  )) |>
  mutate(across(where(is.character), str_to_upper)) |>
  separate(reporting_unit, into = c("municipality", "reporting_unit"), sep = " (?=WARD)") |>
  mutate(reporting_unit = if_else(is.na(reporting_unit), "WARD 1", reporting_unit),
         ctv = str_sub(municipality, 1, 1),
         municipality = word(municipality, 3, -1),
         candidate = str_remove(candidate, coll( "()")),
         year = 2019)

write_csv(s2, "processed-data/nonpartisan/annual/2019.csv")
