rm(list = ls())

library(tidyverse)

# all results are in 1 excel workbook
sheets.2014 <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                                  sheet = 1) |>
  mutate(sheetno = row_number() + 1)

sheets.2014

legis.dists <- read_csv("processed-data/annual/2010-2014-rep-unit-dists.csv") |>
  filter(year == 2014) |>
  select(-year)

################################################################################
# governor 2014
gov.2014.colnames <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                                         sheet = 2, col_names = FALSE,
                                         skip = 9, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

gov.2014.orig <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                                     sheet = 2, skip = 11,
                                     col_names = gov.2014.colnames) |>
  rename(county = 1, rep_unit = 2) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(str_detect(rep_unit, "County Totals", negate = T),
         !is.na(rep_unit)) |>
  left_join(legis.dists)

gov.2014.clean <- gov.2014.orig |>
  pivot_longer(cols = -c(county, rep_unit, total_votes_cast_na, wsa_dist, wss_dist,
                         con_dist),
               values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "dem_mary_burke_john_lehman" ~ "Mary Burke / John Lehman",
      name == "rep_scott_walker_rebecca_kleefisch" ~ "Scott Walker / Rebecca Kleefisch",
      name == "ind_dennis_fehr_no_candidate" ~ "Dennis Fehr",
      name == "ind_robert_burke_joseph_m_brost" ~ "Robert Burke / Joseph M Brost",
      name == "ind_mary_jo_walters_write_in" ~ "Mary Jo Walters",
      name == "rep_steve_r_evans_write_in" ~ "Steve R Evans",
      name == "con_jumoka_a_johnson_write_in" ~ "Jumoka A Johnson",
      name == "ind_brett_d_hulsey_write_in" ~ "Brett D Hulsey",
      name == "ind_jessica_nicole_perry_write_in" ~ "Jessica Nicole Perry",
      name == "rep_susan_p_resch_write_in" ~ "Susan P Resch",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_mary_burke_john_lehman" ~ "Democratic",
      name == "rep_scott_walker_rebecca_kleefisch" ~ "Republican",
      name == "ind_dennis_fehr_no_candidate" ~ "Independent",
      name == "ind_robert_burke_joseph_m_brost" ~ "Independent 2",
      name == "ind_mary_jo_walters_write_in" ~ "Write-in",
      name == "rep_steve_r_evans_write_in" ~ "Write-in 2",
      name == "con_jumoka_a_johnson_write_in" ~ "Write-in 3",
      name == "ind_brett_d_hulsey_write_in" ~ "Write-in 4",
      name == "ind_jessica_nicole_perry_write_in" ~ "Write-in 5",
      name == "rep_susan_p_resch_write_in" ~ "Write-in 6",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  mutate(rep_unit = str_replace(rep_unit, "WARD WD", "WARD")) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))
gov.2014.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

gov.2014.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# congress 2014
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[7]
  dist.colnames <- tibble(x1 = as.character(dist[9,]),
                          x2 = as.character(dist[10,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                     sheet = sheet, skip = 10, col_names = dist.colnames) |>
    janitor::clean_names() |>
    janitor::remove_empty("cols") |>
    rename(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county)) |>
    filter(!is.na(rep_unit), 
           rep_unit != "County Totals:") |>
    pivot_longer(cols = -c(1:3), values_to = "votes") |>
    mutate(district = districtno)
}

all.dist.orig <- map_df(6:13, read_cong_dist)

con.2014.clean <- all.dist.orig |>
  mutate(
    candidate = case_when(
      name == "dem_rob_zerban" ~ "Rob Zerban",
      name == "ind_keith_r_deschler_write_in" ~ "Keith R Deschler",
      name == "rep_paul_ryan" ~ "Paul Ryan",
      name == "dem_mark_pocan" ~ "Mark Pocan",
      name == "rep_peter_theron" ~ "Peter Theron",
      name == "dem_ron_kind" ~ "Ron Kind",
      name == "ind_ken_van_doren_write_in" ~ "Ken Van Doren",
      name == "rep_tony_kurtz" ~ "Tony Kurtz",
      name == "dem_gwen_moore" ~ "Gwen Moore",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "rep_dan_sebring" ~ "Dan Sebring",
      name == "dem_chris_rockwood" ~ "Chris Rockwood",
      name == "rep_f_james_sensenbrenner_jr" ~ "F James Sensenbrenner, Jr",
      name == "dem_mark_l_harris" ~ "Mark L Harris",
      name == "ind_gus_fahrendorf" ~ "Gus Fahrendorf",
      name == "rep_glenn_grothman" ~ "Glenn Grothman",
      name == "dem_kelly_westlund" ~ "Kelly Westlund",
      name == "ind_lawrence_dale" ~ "Lawrence Dale",
      name == "ind_rob_taylor_write_in" ~ "Rob Taylor",
      name == "rep_john_schiess_write_in" ~ "John Schiess",
      name == "rep_sean_duffy" ~ "Sean Duffy",
      name == "dem_ron_gruett" ~ "Ron Gruett",
      name == "rep_reid_j_ribble" ~ "Reid J Ribble",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_rob_zerban" ~ "Democratic",
      name == "ind_keith_r_deschler_write_in" ~ "Write-in",
      name == "rep_paul_ryan" ~ "Republican",
      name == "dem_mark_pocan" ~ "Democratic",
      name == "rep_peter_theron" ~ "Republican",
      name == "dem_ron_kind" ~ "Democratic",
      name == "ind_ken_van_doren_write_in" ~ "Write-in",
      name == "rep_tony_kurtz" ~ "Republican",
      name == "dem_gwen_moore" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "rep_dan_sebring" ~ "Republican",
      name == "dem_chris_rockwood" ~ "Democratic",
      name == "rep_f_james_sensenbrenner_jr" ~ "Republican",
      name == "dem_mark_l_harris" ~ "Democratic",
      name == "ind_gus_fahrendorf" ~ "Independent",
      name == "rep_glenn_grothman" ~ "Republican",
      name == "dem_kelly_westlund" ~ "Democratic",
      name == "ind_lawrence_dale" ~ "Independent",
      name == "ind_rob_taylor_write_in" ~ "Write-in",
      name == "rep_john_schiess_write_in" ~ "Write-in 2",
      name == "rep_sean_duffy" ~ "Republican",
      name == "dem_ron_gruett" ~ "Democratic",
      name == "rep_reid_j_ribble" ~ "Republican",
      name == "na_scattering" ~ "Scattering")
  ) |>
  mutate(rep_unit = str_replace(rep_unit, "WARD WD", "WARD")) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

con.2014.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate, district) |>
  filter(n() > 1)

con.2014.clean |>
  group_by(candidate, party, district) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 31)

################################################################################
# state assembly 2014
read_wsa_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[7]
  dist.colnames <- tibble(x1 = as.character(dist[9,]),
                          x2 = as.character(dist[10,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                     sheet = sheet, skip = 10, col_names = dist.colnames) |>
    janitor::remove_empty("cols") |>
    rename(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county)) |>
    filter(!is.na(rep_unit), 
           rep_unit != "County Totals:") |>
    pivot_longer(cols = -c(1:3), values_to = "votes") |>
    mutate(district = districtno) |>
    separate(name, into = c("party","candidate"), sep = "_") |>
    mutate(
      party = case_when(
        str_detect(candidate, "write-in|wr-in|WRITE-IN") ~ "Write-in",
        candidate == "SCATTERING" ~ "Scattering",
        party == "DEM" ~ "Democratic",
        party == "REP" ~ "Republican",
        party == "LIB" ~ "Libertarian",
        party == "IND" ~ "Independent",
        party == "CON" ~ "Constitution",
        TRUE ~ party),
      candidate = str_replace(candidate, "SCATTERING", "Scattering"),
      candidate = str_remove(candidate, "(write-in)|(wr-in)|(WRITE-IN)"),
      across(where(is.character), str_squish)) |>
    janitor::clean_names()
}

all.wsa.dist.orig <- map_df(31:129, read_wsa_dist)
uniquely.name.wsa.party <- all.wsa.dist.orig |>
  group_by(district, candidate, party) |> 
  summarise(votes = sum(total_votes_cast_na)) |> 
  arrange(desc(votes)) |> 
  group_by(district, party) |>
  mutate(party = if_else(row_number() > 1, 
                         paste(party, row_number()),
                         party)) |>
  ungroup() |>
  select(district, candidate, party)

all.wsa.dist.orig.2 <- all.wsa.dist.orig |>
  select(-party) |>
  inner_join(uniquely.name.wsa.party) |>
  mutate(rep_unit = str_replace(rep_unit, "WARD WD", "WARD")) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

# ensure all candidates have a unique party label
all.wsa.dist.orig.2 |>
  group_by(district, party) |>
  summarise(candidate = n_distinct(candidate)) |>
  filter(candidate > 1)
unique(all.wsa.dist.orig.2$party)

################################################################################
# state senate 2014
read_wss_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[7]
  dist.colnames <- tibble(x1 = as.character(dist[9,]),
                          x2 = as.character(dist[10,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                     sheet = sheet, skip = 10, col_names = dist.colnames) |>
    janitor::remove_empty("cols") |>
    rename(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county)) |>
    filter(!is.na(rep_unit), 
           rep_unit != "County Totals:") |>
    pivot_longer(cols = -c(1:3), values_to = "votes") |>
    mutate(district = districtno) |>
    separate(name, into = c("party","candidate"), sep = "_") |>
    mutate(
      party = case_when(
        str_detect(candidate, "write-in|wr-in|WRITE-IN") ~ "Write-in",
        candidate == "SCATTERING" ~ "Scattering",
        party == "DEM" ~ "Democratic",
        party == "REP" ~ "Republican",
        party == "LIB" ~ "Libertarian",
        party == "IND" ~ "Independent",
        party == "CON" ~ "Constitution",
        TRUE ~ party),
      candidate = str_replace(candidate, "SCATTERING", "Scattering"),
      candidate = str_remove(candidate, "(write-in)|(wr-in)|(WRITE-IN)"),
      across(where(is.character), str_squish)) |>
    janitor::clean_names()
}

all.wss.dist.orig <- map_df(14:30, read_wss_dist)
uniquely.name.wss.party <- all.wss.dist.orig |>
  group_by(district, candidate, party) |> 
  summarise(votes = sum(total_votes_cast_na)) |> 
  arrange(desc(votes)) |> 
  group_by(district, party) |>
  mutate(party = if_else(row_number() > 1, 
                         paste(party, row_number()),
                         party)) |>
  ungroup() |>
  select(district, candidate, party)

all.wss.dist.orig.2 <- all.wss.dist.orig |>
  select(-party) |>
  inner_join(uniquely.name.wss.party) |>
  mutate(rep_unit = str_replace(rep_unit, "WARD WD", "WARD")) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

# ensure all candidates have a unique party label
all.wss.dist.orig.2 |>
  group_by(district, party) |>
  summarise(candidate = n_distinct(candidate)) |>
  filter(candidate > 1)
unique(all.wss.dist.orig.2$party)

################################################################################
# Combine
all.2014 <- bind_rows(
  gov.2014.clean |>
    mutate(office = "governor") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           con_dist, wss_dist, wsa_dist,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2014.clean |>
    mutate(office = "congress") |>
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes),
  all.wss.dist.orig.2 |>
    mutate(office = "state senate") |>
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes),
  all.wsa.dist.orig.2 |>
    mutate(office = "state assembly") |>
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) |>
  mutate(year = 2014,
         across(where(is.character), str_squish)) |>
  select(county, municipality, ctv, reporting_unit, year, office, district,
         con_dist, wss_dist, wsa_dist, party, candidate, votes) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(con_dist = unique(con_dist[office == "governor"]),
         wss_dist = unique(wss_dist[office == "governor"]),
         wsa_dist = unique(wsa_dist[office == "governor"])) |>
  ungroup()

write_csv(all.2014, "processed-data/annual/2014.csv")
