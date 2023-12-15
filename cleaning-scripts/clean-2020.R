rm(list = ls())

library(tidyverse)

# this script cleans election files from 2020

################################################################################
# president 2020
pres.2020.orig <- readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20PRESIDENT%20OF%20THE%20UNITED%20STATES%20by%20State%20Representive%20District%20-%20After%20Recount.xlsx") %>%
  janitor::clean_names()

glimpse(pres.2020.orig)

# party information from: https://elections.wi.gov/sites/default/files/legacy/County%2520by%2520County%2520Report%2520-%2520President%2520of%2520the%2520United%2520States%2520post%2520recount.pdf
pres.2020.clean <- pres.2020.orig %>%
  select(-contains("district")) %>%
  pivot_longer(cols = -c(county_name, municipality_name, reporting_unit),
               values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "joseph_r_biden_kamala_d_harris" ~ "Joseph R Biden / Kamala D Harris",
      name == "donald_j_trump_michael_r_pence" ~ "Donald J Trump / Michael R Pence",
      name == "don_blankenship_william_mohr" ~ "Don Blankenship / William Mohr",
      name == "jo_jorgensen_jeremy_spike_cohen" ~ "Jo Jorgensen / Jeremy Spike Cohen",
      name == "brian_carroll_amar_patel" ~ "Brian Carroll / Amar Patel",
      name == "kasey_wells_write_in" ~ "Kasey Wells",
      name == "jade_simmons_claudeliah_j_roze_write_in" ~ "Jade Simmons / Claudeliah J Roze",
      name == "president_r19_boddie_write_in" ~ "President R19 Boddie",
      name == "howie_hawkins_angela_walker_write_in" ~ "Howie Hawkins / Angela Walker",
      name == "gloria_la_riva_sunil_freeman_write_in" ~ "Glorida La Riva / Sunil Freeman",
      name == "kanye_west_michelle_tidball_write_in" ~ "Kanye West / Michelle Tidball",
      name == "mark_charles_adrian_wallace_write_in" ~ "Mark Chalres / Adrian Wallace",
      name == "scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "joseph_r_biden_kamala_d_harris" ~ "Democratic",
      name == "donald_j_trump_michael_r_pence" ~ "Republican",
      name == "don_blankenship_william_mohr" ~ "Constitution",
      name == "jo_jorgensen_jeremy_spike_cohen" ~ "Independent",
      name == "brian_carroll_amar_patel" ~ "Independent 2",
      name == "kasey_wells_write_in" ~ "Write-in",
      name == "jade_simmons_claudeliah_j_roze_write_in" ~ "Write-in 2",
      name == "president_r19_boddie_write_in" ~ "Write-in 3",
      name == "howie_hawkins_angela_walker_write_in" ~ "Write-in 4",
      name == "gloria_la_riva_sunil_freeman_write_in" ~ "Write-in 5",
      name == "kanye_west_michelle_tidball_write_in" ~ "Write-in 6",
      name == "mark_charles_adrian_wallace_write_in" ~ "Write-in 7",
      name == "scattering" ~ "Scattering"
    )
  ) %>%
  mutate(municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

pres.2020.clean %>%
  group_by(county = county_name, municipality_name, municipality_type, 
           reporting_unit_name = reporting_unit, candidate) %>%
  filter(n() > 1)

pres.2020.clean %>%
  group_by(candidate, party) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 28)

################################################################################
# congress 2020
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20-%20Representative%20in%20Congress.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[which(str_detect(dist$...1, "REPRESENTATIVE IN CONGRESS "))]
  colname.start <- which(str_detect(dist$...3, "Total Votes Cast"))
  dist.colnames <- tibble(x1 = as.character(dist[colname.start,]),
                          x2 = as.character(dist[colname.start + 1,])) %>%
    mutate(colname = paste(x1, x2, sep = "_")) %>%
    pull(colname)
  readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20-%20Representative%20in%20Congress.xlsx",
                     sheet = sheet, skip = (colname.start + 1), col_names = dist.colnames) %>%
    janitor::clean_names() %>%
    janitor::remove_empty("cols") %>%
    rename(county = 1, rep_unit = 2) %>%
    mutate(county = zoo::na.locf(county)) %>%
    filter(!is.na(rep_unit), 
           rep_unit != "County Totals:") %>%
    pivot_longer(cols = -c(1:3), values_to = "votes") %>%
    mutate(district = districtno)
}

all.dist.orig <- map_df(2:9, read_cong_dist)

con.2020.clean <- all.dist.orig %>%
  mutate(
    candidate = case_when(
      name == "dem_roger_polack" ~ "Roger Polack",
      name == "rep_bryan_steil" ~ "Bryan Steil",
      name == "dem_mark_pocan" ~ "Mark Pocan",
      name == "rep_peter_theron" ~ "Peter Theron",
      name == "dem_ron_kind" ~ "Ron Kind",
      name == "rep_derrick_van_orden" ~ "Derrick Van Orden",
      name == "dem_gwen_s_moore" ~ "Gwen S Moore",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "rep_tim_rogers" ~ "Tim Rogers",
      name == "dem_tom_palzewicz" ~ "Tom Palzewicz",
      name == "rep_scott_fitzgerald" ~ "Scott Fitzgerald",
      name == "dem_jessica_j_king" ~ "Jessica J King",
      name == "rep_glenn_grothman" ~ "Glenn Grothman",
      name == "dem_tricia_zunker" ~ "Tricia Zunker",
      name == "rep_tom_tiffany" ~ "Tom Tiffany",
      name == "dem_amanda_stuck" ~ "Amanda Stuck",
      name == "rep_mike_gallagher" ~ "Mike Gallagher",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_roger_polack" ~ "Democratic",
      name == "rep_bryan_steil" ~ "Republican",
      name == "dem_mark_pocan" ~ "Democratic",
      name == "rep_peter_theron" ~ "Republican",
      name == "dem_ron_kind" ~ "Democratic",
      name == "rep_derrick_van_orden" ~ "Republican",
      name == "dem_gwen_s_moore" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "rep_tim_rogers" ~ "Republican",
      name == "dem_tom_palzewicz" ~ "Democratic",
      name == "rep_scott_fitzgerald" ~ "Republican",
      name == "dem_jessica_j_king" ~ "Democratic",
      name == "rep_glenn_grothman" ~ "Republican",
      name == "dem_tricia_zunker" ~ "Democratic",
      name == "rep_tom_tiffany" ~ "Republican",
      name == "dem_amanda_stuck" ~ "Democratic",
      name == "rep_mike_gallagher" ~ "Republican",
      name == "na_scattering" ~ "Scattering")
  ) %>%
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward)") %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

con.2020.clean %>%
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate, district) %>%
  filter(n() > 1)

con.2020.clean %>%
  group_by(candidate, party, district) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 32)

################################################################################
# Combine
all.2020 <- bind_rows(
  pres.2020.clean %>%
    mutate(office = "president") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit, office, party, candidate, votes),
  con.2020.clean %>%
    mutate(office = "congress") %>%
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) %>%
  mutate(year = 2020,
         across(where(is.character), str_squish)) %>%
  select(county, municipality, ctv, reporting_unit, year, office, district,
         party, candidate, votes)

write_csv(all.2020, "processed-data/annual/2020.csv")
