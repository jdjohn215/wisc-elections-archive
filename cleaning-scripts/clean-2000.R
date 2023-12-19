rm(list = ls())

library(tidyverse)

# This script standardizes the elections from 2000

################################################################################
# PRESIDENT
pres.2000.orig <- readxl::read_excel("original-data/2000-11-07_PRES_SORT.xls",
                                     col_types = "text", n_max = 3597) %>%
  janitor::remove_empty("cols") %>%
  janitor::clean_names()

pres.2000.clean <- pres.2000.orig %>%
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         george_bush_dick_cheney_republican, al_gore_joe_lieberman_democratic,
         harry_browne_art_olivier_libertarian, h_phillips_j_c_frazier_constitution,
         ralph_nader_winona_la_duke_wisconsin_greens, m_moorehead_gloria_la_riva_independent,
         james_harris_m_trowe_independent, john_hagelin_nat_goldhaber_independent,
         pat_buchanan_ezola_foster_independent, scattering) %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) %>%
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "george_bush_dick_cheney_republican" ~ "George Bush / Dick Cheney",
      name == "al_gore_joe_lieberman_democratic" ~ "Al Gore / Joe Lieberman",
      name == "harry_browne_art_olivier_libertarian" ~ "Harry Browne / Art Olivier",
      name == "h_phillips_j_c_frazier_constitution" ~ "H Phillips / J C Frazier",
      name == "ralph_nader_winona_la_duke_wisconsin_greens" ~ "Ralph Nader / Winona la Duke",
      name == "m_moorehead_gloria_la_riva_independent" ~ "M Moorehead / Gloria la Riva",
      name == "james_harris_m_trowe_independent" ~ "James Harris / M Trowe",
      name == "john_hagelin_nat_goldhaber_independent" ~ "John Hagelin / Nat Goldhaber",
      name == "pat_buchanan_ezola_foster_independent" ~ "Pat Buchanan / Ezola Foster",
      name == "scattering" ~ "Scattering"),
    party = case_when(
      name == "george_bush_dick_cheney_republican" ~ "Republican",
      name == "al_gore_joe_lieberman_democratic" ~ "Democratic",
      name == "harry_browne_art_olivier_libertarian" ~ "Libertarian",
      name == "h_phillips_j_c_frazier_constitution" ~ "Constitution",
      name == "ralph_nader_winona_la_duke_wisconsin_greens" ~ "Wisconsin Green",
      name == "m_moorehead_gloria_la_riva_independent" ~ "Independent",
      name == "james_harris_m_trowe_independent" ~ "Independent 2",
      name == "john_hagelin_nat_goldhaber_independent" ~ "Independent 3",
      name == "pat_buchanan_ezola_foster_independent" ~ "Independent 4",
      name == "scattering" ~ "Scattering")
  )

pres.2000.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

pres.2000.clean %>%
  group_by(candidate) %>%
  summarise(count = n())

################################################################################
# Senate
sen.2000.orig <- readxl::read_excel("original-data/2000-11-07_US_SEN_SORT.xls",
                                    col_types = "text") %>%
  janitor::remove_empty("cols") %>%
  janitor::clean_names() %>%
  filter(!is.na(office_type))

sen.2000.clean <- sen.2000.orig %>%
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         john_gillespie_republican, herbert_h_kohl_democratic,
         tim_peterson_libertarian, robert_r_raymond_constitution,
         eugene_a_hem_independent, scattering) %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) %>%
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "john_gillespie_republican" ~ "John Gillespie",
      name == "herbert_h_kohl_democratic" ~ "Herbert H Kohl",
      name == "tim_peterson_libertarian" ~ "Tim Peterson",
      name == "robert_r_raymond_constitution" ~ "Robert R Raymond",
      name == "eugene_a_hem_independent" ~ "Eugene A Hem",
      name == "scattering" ~ "Scattering"),
    party = case_when(
      name == "john_gillespie_republican" ~ "Republican",
      name == "herbert_h_kohl_democratic" ~ "Democratic",
      name == "tim_peterson_libertarian" ~ "Libertarian",
      name == "robert_r_raymond_constitution" ~ "Constitution",
      name == "eugene_a_hem_independent" ~ "Independent",
      name == "scattering" ~ "Scattering")
  )
sen.2000.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

sen.2000.clean %>%
  group_by(candidate) %>%
  summarise(count = n())

################################################################################
# Congress
con.2000.orig <- readxl::read_excel("original-data/2000-11-07_US_CONG_SORT.xls",
                                    col_types = "text", col_names = FALSE) %>%
  janitor::clean_names()

dist.start <- which(con.2000.orig$x6 == "US CONGRESS")
dist.colnames <- con.2000.orig[sort(c(dist.start, dist.start+1)),] %>%
  mutate(id = rep(1:9, each = 2)) %>%
  group_by(id) %>%
  mutate(rownum = row_number()) %>%
  ungroup() %>%
  pivot_longer(cols = -c(id, rownum)) %>%
  pivot_wider(names_from = rownum, values_from = value) %>%
  mutate(colname = paste(`1`, `2`, sep = "_")) %>%
  select(id, name, colname)

read_cong_district <- function(district){
  readxl::read_excel("original-data/2000-11-07_US_CONG_SORT.xls",
                     col_types = "text", skip = (dist.start[district] + 1),
                     col_names = dist.colnames$colname[dist.colnames$id == district]) %>%
    filter(OFFICE_NAME == OFFICE_NAME[row_number() == 1]) %>%
    janitor::clean_names() %>%
    janitor::remove_empty("cols") %>%
    select(-c(election_date, election_type, election_subtype, office_type_keyword,
              office_name, state_senate_district, state_assembly_district, 
              court_of_appeals_district, county_number, hindi_number, order_number,
              municipality_number)) %>%
    mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) %>%
    pivot_longer(cols = -c(county_name, municipality_name, municipality_type,
                           reporting_unit_name, us_congress_district),
                 names_to = "name", values_to = "votes")
}

all.dists <- map_df(1:9, read_cong_district)

con.2000.clean <- all.dists %>%
  mutate(
    candidate = case_when(
      name == "jeffrey_c_thomas_democratic" ~ "Jeffrey C Thomas",
      name == "david_r_obey_democratic" ~ "David R Obey",
      name == "john_sharpless_republican" ~ "John Sharpless",
      name == "tammy_baldwin_democratic" ~ "Tammy Baldwin",
      name == "ron_kind_democratic" ~ "Ron Kind",
      name == "susan_tully_republican" ~ "Susan Tully",
      name == "jerry_kleczka_democratic" ~ "Jerry Kleczka",
      name == "nikola_rajnovic_libertarian" ~ "Nikola Rajnovic",
      name == "tim_riener_republican" ~ "Tim Riener",
      name == "jonathan_smith_republican" ~ "Jonathan Smith",
      name == "tom_barrett_democratic" ~ "Tom Barrett",
      name == "dan_flaherty_democratic" ~ "Dan Flaherty",
      name == "paul_d_ryan_republican" ~ "Paul D Ryan",
      name == "tom_petri_republican" ~ "Tom Petri",
      name == "sean_cronin_republican" ~ "Sean Cronin",
      name == "dean_reich_democratic" ~ "Dean Reich",
      name == "mark_green_republican" ~ "Mark Green",
      name == "f_james_sensenbrenner_jr_republican" ~ "F James Sensenbrenner, Jr",
      name == "mike_clawson_democratic" ~ "Mike Clawson",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      str_detect(name, "_democratic") ~ "Democratic",
      str_detect(name, "_republican") ~ "Republican",
      str_detect(name, "_libertarian") ~ "Libertarian",
      name == "scattering_na" ~ "Scattering")
  )

con.2000.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

con.2000.clean %>%
  group_by(candidate, party, us_congress_district) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 28)

################################################################################
# Combine
all.2000 <- bind_rows(
  pres.2000.clean %>%
    mutate(office = "president") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  sen.2000.clean %>%
    mutate(office = "senate") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2000.clean %>%
    mutate(office = "congress") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           district = us_congress_district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) %>%
  mutate(year = 2000) %>%
  select(county, municipality, ctv, reporting_unit, year, office, district,
         party, candidate, votes)

write_csv(all.2000, "processed-data/annual/2000.csv")
