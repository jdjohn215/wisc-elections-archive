rm(list = ls())

library(tidyverse)

# This script standardizes the elections from 2008

################################################################################
# President
pres.colnames <- readxl::read_excel("original-data/2008-11-04_FallElection_President_WardbyWard.xls",
                                   n_max = 2, col_names = F) %>%
  mutate(rownum = row_number()) %>%
  pivot_longer(cols = -rownum) %>%
  pivot_wider(names_from = rownum, values_from = value) %>%
  mutate(colname = paste(`1`, `2`, sep = "_")) %>%
  pull(colname)
pres.2008.orig <- readxl::read_excel("original-data/2008-11-04_FallElection_President_WardbyWard.xls",
                                    skip = 2, col_names = pres.colnames, col_types = "text") %>%
  janitor::clean_names() %>%
  filter(!is.na(election_type)) %>%
  janitor::remove_empty("cols")

pres.2008.clean <- pres.2008.orig %>%
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         joe_biden_barack_obama_democratic, sarah_palin_john_mc_cain_republican,
         rosa_clemente_cynthia_mc_kinney_wisconsin_green, wayne_a_root_bob_barr_libertarian,
         stewart_a_alexander_brian_moore_independent, robert_moses_gloria_la_riva_independent,
         matt_gonzalez_ralph_nader_independent, darrell_l_castle_chuck_baldwin_independent,
         david_j_klimisch_jeffrey_j_wamboldt_independent,
         scattering_na) %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) %>%
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "joe_biden_barack_obama_democratic" ~ "Barack Obama / Joe Biden",
      name == "sarah_palin_john_mc_cain_republican" ~ "John McCain / Sarah Palin",
      name == "rosa_clemente_cynthia_mc_kinney_wisconsin_green" ~ "Cynthia McKinney / Rosa Clemente",
      name == "wayne_a_root_bob_barr_libertarian" ~ "Bob Barr / Wayne A Root",
      name == "stewart_a_alexander_brian_moore_independent" ~ "Brian Moore / Steward A Alexander",
      name == "robert_moses_gloria_la_riva_independent" ~ "Gloria la Riva / Robert Moses",
      name == "matt_gonzalez_ralph_nader_independent" ~ "Ralph Nader / Matt Gonzalez",
      name == "darrell_l_castle_chuck_baldwin_independent" ~ "Chuck Baldwin / Darrell L Castle",
      name == "david_j_klimisch_jeffrey_j_wamboldt_independent" ~ "Jeffrey J Wamboldt / David J Klimisch",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "joe_biden_barack_obama_democratic" ~ "Democratic",
      name == "sarah_palin_john_mc_cain_republican" ~ "Republican",
      name == "rosa_clemente_cynthia_mc_kinney_wisconsin_green" ~ "Wisconsin Green",
      name == "wayne_a_root_bob_barr_libertarian" ~ "Libertarian",
      name == "stewart_a_alexander_brian_moore_independent" ~ "Independent",
      name == "robert_moses_gloria_la_riva_independent" ~ "Independent 2",
      name == "matt_gonzalez_ralph_nader_independent" ~ "Independent 3",
      name == "darrell_l_castle_chuck_baldwin_independent" ~ "Independent 4",
      name == "david_j_klimisch_jeffrey_j_wamboldt_independent" ~ "Independent 5",
      name == "scattering_na" ~ "Scattering")
  )

pres.2008.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

pres.2008.clean %>%
  group_by(candidate) %>%
  summarise(count = n())

################################################################################
# Congress
con.2008.orig <- readxl::read_excel("original-data/2008-11-04_FallElection_USCongress_WardbyWard.xls",
                                    col_types = "text", col_names = FALSE) %>%
  janitor::clean_names()

dist.start <- which(con.2008.orig$x6 == "US CONGRESS")
dist.colnames <- con.2008.orig[sort(c(dist.start, dist.start+1)),] %>%
  mutate(id = rep(1:8, each = 2)) %>%
  group_by(id) %>%
  mutate(rownum = row_number()) %>%
  ungroup() %>%
  pivot_longer(cols = -c(id, rownum)) %>%
  pivot_wider(names_from = rownum, values_from = value) %>%
  mutate(colname = paste(`1`, `2`, sep = "_")) %>%
  select(id, name, colname)

read_cong_district <- function(district){
  df <- readxl::read_excel("original-data/2008-11-04_FallElection_USCongress_WardbyWard.xls",
                           col_types = "text", skip = (dist.start[district] + 1),
                           col_names = FALSE)
  names(df) <- dist.colnames$colname[dist.colnames$id == district][1:ncol(df)]
  df %>%
    janitor::clean_names() %>%
    filter(office_name == office_name[row_number() == 1]) %>%
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

all.dists <- map_df(1:8, read_cong_district)

con.2008.clean <- all.dists %>%
  mutate(
    candidate = case_when(
      name == "dan_mielke_republican" ~ "Dan Mielke",
      name == "david_r_obey_democratic" ~ "David R Obey",
      name == "f_james_sensenbrenner_jr_republican" ~ "F James Sensenbrenner, Jr",
      name == "gwen_moore_democratic" ~ "Gwen Moore",
      name == "john_gard_republican" ~ "John Gard",
      name == "joseph_kexel_libertarian" ~ "Joseph Kexel",
      name == "kevin_barrett_libertarian" ~ "Kevin Barrett",
      name == "marge_krupp_democratic" ~ "Marge Krupp",
      name == "michael_d_la_forest_independent" ~ "Michael D LaForest",
      name == "paul_ryan_republican" ~ "Paul Ryan",
      name == "peter_theron_republican" ~ "Peter Theron",
      name == "robert_r_raymond_independent" ~ "Robert R Raymond",
      name == "roger_a_kittelson_democratic" ~ "Roger A Kittelson",
      name == "ron_kind_democratic" ~ "Ron Kind",
      name == "steve_kagen_democratic" ~ "Steve Kagen",
      name == "tammy_baldwin_democratic" ~ "Tammy Baldwin",
      name == "tom_petri_republican" ~ "Tom Petri",
      name == "paul_stark_republican" ~ "Paul Stark",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      str_detect(name, "_democratic") ~ "Democratic",
      str_detect(name, "_republican") ~ "Republican",
      str_detect(name, "_libertarian") ~ "Libertarian",
      str_detect(name, "_wisconsin_green") ~ "Wisconsin Green",
      str_detect(name, "_independent") ~ "Independent",
      name == "scattering_na" ~ "Scattering")
  )

con.2008.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

con.2008.clean %>%
  group_by(candidate, party, us_congress_district) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  print(n = 28)

################################################################################
# Combine
all.2008 <- bind_rows(
  pres.2008.clean %>%
    mutate(office = "president") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2008.clean %>%
    mutate(office = "congress") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           district = us_congress_district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) %>%
  mutate(year = 2008) %>%
  select(county, municipality, ctv, reporting_unit, year, office, district,
         party, candidate, votes)

write_csv(all.2008, "processed-data/annual/2008.csv")
