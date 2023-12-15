rm(list = ls())

library(tidyverse)

# this script cleans election files from 2022

################################################################################
# govident 2022
gov.2022.orig <- readxl::read_excel("original-data/2022 Governor by reporting unit with Congressional, State Senate, Assembly district.xlsx") %>%
  janitor::clean_names()

glimpse(gov.2022.orig)

gov.2022.clean <- gov.2022.orig %>%
  select(-contains("district")) %>%
  pivot_longer(cols = -c(county_name, municipality_name, reporting_unit_text),
               values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "tony_evers_sara_rodriguez" ~ "Tony Evers / Sara Rodriguez",
      name == "tim_michels_roger_roth" ~ "Tim Michels / Roger Roth",
      name == "joan_ellis_beglinger" ~ "Joan Ellis Beglinger",
      name == "seth_haskin_write_in" ~ "Seth Haskin",
      name == "scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "tony_evers_sara_rodriguez" ~ "Democratic",
      name == "tim_michels_roger_roth" ~ "Republican",
      name == "joan_ellis_beglinger" ~ "Independent",
      name == "seth_haskin_write_in" ~ "Write-in",
      name == "scattering" ~ "Scattering"
    )
  ) %>%
  mutate(municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

gov.2022.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_text, candidate) %>%
  filter(n() > 1)

gov.2022.clean %>%
  group_by(candidate, party) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 28)

################################################################################
# senate 2022
sen.2022.colnames <- readxl::read_excel("original-data/Ward by Ward Report by Congressional District - US Senate.xlsx",
                                        sheet = 2, col_names = FALSE,
                                        skip = 8, n_max = 2) %>%
  mutate(rownum = row_number()) %>%
  pivot_longer(cols = -rownum) %>%
  pivot_wider(names_from = rownum, values_from = value) %>%
  mutate(colname = paste(`1`, `2`, sep = "_")) %>%
  pull(colname)

sen.2022.orig <- readxl::read_excel("original-data/Ward by Ward Report by Congressional District - US Senate.xlsx",
                                    sheet = 2, skip = 10, col_names = sen.2022.colnames) %>%
  select(-1) %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  rename(county = 1, reporting_unit_name = 2) %>%
  mutate(county = zoo::na.locf(county)) %>%
  filter(reporting_unit_name != "County Totals:",
         !is.na(reporting_unit_name),
         str_detect(reporting_unit_name, "County Subtotals", negate = T))

sen.2022.clean <- sen.2022.orig %>%
  select(-contains("district")) %>%
  pivot_longer(cols = -c(1:3), values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "dem_mandela_barnes" ~ "Mandela Barnes",
      name == "rep_ron_johnson" ~ "Ron Johnson",
      name == "ind_adam_paul_write_in" ~ "Adam Paul",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_mandela_barnes" ~ "Democratic",
      name == "rep_ron_johnson" ~ "Republican",
      name == "ind_adam_paul_write_in" ~ "Write-in",
      name == "na_scattering" ~ "Scattering"
    )
  ) %>%
  separate(reporting_unit_name, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward)") %>%
  mutate(municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

sen.2022.clean %>%
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

sen.2022.clean %>%
  group_by(candidate, party) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 28)

################################################################################
# congress 2022
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/Ward by Ward Report_Representative in Congress_0.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[which(str_detect(dist$...1, "REPRESENTATIVE IN CONGRESS "))]
  colname.start <- which(str_detect(dist$...3, "Total Votes Cast"))
  dist.colnames <- tibble(x1 = as.character(dist[colname.start,]),
                          x2 = as.character(dist[colname.start + 1,])) %>%
    mutate(colname = paste(x1, x2, sep = "_")) %>%
    pull(colname)
  readxl::read_excel("original-data/Ward by Ward Report_Representative in Congress_0.xlsx",
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

con.2022.clean <- all.dist.orig %>%
  mutate(
    candidate = case_when(
      name == "dem_ann_roe" ~ "Ann Roe",
      name == "ind_charles_e_barman" ~ "Charles E Barman",
      name == "rep_bryan_steil" ~ "Bryan Steil",
      name == "dem_mark_pocan" ~ "Mark Pocan",
      name == "ind_douglas_alexander" ~ "Douglas Alexander",
      name == "rep_erik_olsen" ~ "Erik Olsen",
      name == "dem_brad_pfaff" ~ "Brad Pfaff",
      name == "rep_derrick_van_orden" ~ "Derrick Van Orden",
      name == "dem_gwen_moore" ~ "Gwen Moore",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "rep_tim_rogers" ~ "Tim Rogers",
      name == "dem_mike_van_someren" ~ "Mike Van Someren",
      name == "rep_scott_fitzgerald" ~ "Scott Fitzgerald",
      name == "ind_tom_powell_write_in" ~ "Tom Powell",
      name == "rep_glenn_grothman" ~ "Glenn Grothman",
      name == "dem_richard_dick_ausman" ~ "Richard Dick Ausman",
      name == "rep_tom_tiffany" ~ "Tom Tiffany",
      name == "dem_julie_hancock_write_in" ~ "Julie Hancock",
      name == "dem_robbie_hoffman_write_in" ~ "Robbie Hoffman",
      name == "ind_paul_david_boucher" ~ "Paul David Boucher",
      name == "lib_jacob_j_vanden_plas" ~ "Jacob J VandenPlas",
      name == "rep_mike_gallagher" ~ "Mike Gallagher",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_ann_roe" ~ "Democratic",
      name == "ind_charles_e_barman" ~ "Independent",
      name == "rep_bryan_steil" ~ "Republican",
      name == "dem_mark_pocan" ~ "Democratic",
      name == "ind_douglas_alexander" ~ "Independent",
      name == "rep_erik_olsen" ~ "Republican",
      name == "dem_brad_pfaff" ~ "Democratic",
      name == "rep_derrick_van_orden" ~ "Republican",
      name == "dem_gwen_moore" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "rep_tim_rogers" ~ "Republican",
      name == "dem_mike_van_someren" ~ "Democratic",
      name == "rep_scott_fitzgerald" ~ "Republican",
      name == "ind_tom_powell_write_in" ~ "Write-in",
      name == "rep_glenn_grothman" ~ "Republican",
      name == "dem_richard_dick_ausman" ~ "Democratic",
      name == "rep_tom_tiffany" ~ "Republican",
      name == "dem_julie_hancock_write_in" ~ "Write-in",
      name == "dem_robbie_hoffman_write_in" ~ "Write-in 2",
      name == "ind_paul_david_boucher" ~ "Independent",
      name == "lib_jacob_j_vanden_plas" ~ "Libertarian",
      name == "rep_mike_gallagher" ~ "Republican",
      name == "na_scattering" ~ "Scattering")
  ) %>%
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward)") %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

con.2022.clean %>%
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate, district) %>%
  filter(n() > 1)

con.2022.clean %>%
  group_by(candidate, party, district) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 32)

################################################################################
# Combine
all.2022 <- bind_rows(
  gov.2022.clean %>%
    mutate(office = "governor") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_text, office, party, candidate, votes),
  sen.2022.clean %>%
    mutate(office = "senate") %>%
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2022.clean %>%
    mutate(office = "congress") %>%
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) %>%
  mutate(year = 2022,
         across(where(is.character), str_squish)) %>%
  select(county, municipality, ctv, reporting_unit, year, office, district,
         party, candidate, votes)

write_csv(all.2022, "processed-data/annual/2022.csv")

all.2022 %>%
  group_by(office, district) %>%
  summarise(total = sum(votes))
