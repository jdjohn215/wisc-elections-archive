rm(list = ls())

library(tidyverse)

# This script standardizes the elections from 2004

################################################################################
# President
pres.colnames <- readxl::read_excel("original-data/2004-11-02_FallElection_President_WardbyWard.xls",
                                   n_max = 2, col_names = F) %>%
  mutate(rownum = row_number()) %>%
  pivot_longer(cols = -rownum) %>%
  pivot_wider(names_from = rownum, values_from = value) %>%
  mutate(colname = paste(`1`, `2`, sep = "_")) %>%
  pull(colname)
pres.2004.orig <- readxl::read_excel("original-data/2004-11-02_FallElection_President_WardbyWard.xls",
                                    skip = 2, col_names = pres.colnames, col_types = "text") %>%
  janitor::clean_names() %>%
  filter(!is.na(election_type)) %>%
  janitor::remove_empty("cols")

pres.2004.clean <- pres.2004.orig %>%
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         john_edwards_john_f_kerry_democratic, dick_cheney_george_w_bush_republican,
         richard_v_campagna_michael_badnarik_libertarian, patricia_la_marche_david_cobb_wisconsin_greens,
         peter_miguel_camejo_ralph_nader_independent, margaret_trowe_james_harris_independent,
         mary_alice_herbert_walter_f_brown_independent, scattering_na) %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) %>%
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "john_edwards_john_f_kerry_democratic" ~ "John F Kerry / John Edwards",
      name == "dick_cheney_george_w_bush_republican" ~ "George W Bush / Dick Cheney",
      name == "richard_v_campagna_michael_badnarik_libertarian" ~ "Michael Badnarik / Richard V Campagna",
      name == "patricia_la_marche_david_cobb_wisconsin_greens" ~ "David Cobb / Patricia la Marche",
      name == "peter_miguel_camejo_ralph_nader_independent" ~ "Ralph Nader / Peter Miguel Camejo",
      name == "margaret_trowe_james_harris_independent" ~ "James Harris / Margaret Trowe",
      name == "mary_alice_herbert_walter_f_brown_independent" ~ "Walter F Brown / Mary Alice Herbert",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "john_edwards_john_f_kerry_democratic" ~ "Democratic",
      name == "dick_cheney_george_w_bush_republican" ~ "Republican",
      name == "richard_v_campagna_michael_badnarik_libertarian" ~ "Libertarian",
      name == "patricia_la_marche_david_cobb_wisconsin_greens" ~ "Wisconsin Green",
      name == "peter_miguel_camejo_ralph_nader_independent" ~ "Independent",
      name == "margaret_trowe_james_harris_independent" ~ "Independent 2",
      name == "mary_alice_herbert_walter_f_brown_independent" ~ "Independent 3",
      name == "scattering_na" ~ "Scattering")
  )

pres.2004.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

pres.2004.clean %>%
  group_by(candidate) %>%
  summarise(count = n())

################################################################################
# Senate
sen.colnames <- readxl::read_excel("~/Dropbox/Projects/2023/December/wisc-elections-archive/original-data/2004-11-02_FallElection_USSenate_WardbyWard.xls",
                                    n_max = 2, col_names = F) %>%
  mutate(rownum = row_number()) %>%
  pivot_longer(cols = -rownum) %>%
  pivot_wider(names_from = rownum, values_from = value) %>%
  mutate(colname = paste(`1`, `2`, sep = "_")) %>%
  pull(colname)
sen.2004.orig <- readxl::read_excel("~/Dropbox/Projects/2023/December/wisc-elections-archive/original-data/2004-11-02_FallElection_USSenate_WardbyWard.xls",
                                     skip = 2, col_names = sen.colnames, col_types = "text") %>%
  janitor::clean_names() %>%
  filter(!is.na(election_type)) %>%
  janitor::remove_empty("cols")

sen.2004.clean <- sen.2004.orig %>%
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         russ_feingold_democratic, tim_michels_republican,
         arif_khan_libertarian, eugene_a_hem_independent, scattering_na) %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) %>%
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") %>%
  mutate(
    candidate = case_when(
      name == "russ_feingold_democratic" ~ "Russ Feingold",
      name == "tim_michels_republican" ~ "Tim Michels",
      name == "arif_khan_libertarian" ~ "Arif Khan",
      name == "eugene_a_hem_independent" ~ "Eugene A Hem",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "russ_feingold_democratic" ~ "Democratic",
      name == "tim_michels_republican" ~ "Republican",
      name == "arif_khan_libertarian" ~ "Libertarian",
      name == "eugene_a_hem_independent" ~ "Independent",
      name == "scattering_na" ~ "Scattering")
  )

sen.2004.clean %>%
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

sen.2004.clean %>%
  group_by(candidate) %>%
  summarise(count = n())

################################################################################
# Congress
#   this file requires special processing because it is a PDF
#   see cleaning-scripts/clean-2004-congress.R for details
con.2004.clean <- read_csv("processed-data/annual/2004-congress-only.csv",
                           col_types = cols(.default = "c"))


################################################################################
# Combine
all.2004 <- bind_rows(
  pres.2004.clean %>%
    mutate(office = "president") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2004.clean %>%
    mutate(office = "congress") %>%
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes),
  sen.2004.clean %>%
    mutate(office = "senate") %>%
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes)
) %>%
  mutate(year = 2004,
         votes = str_remove(votes, coll(","))) %>%
  select(county, municipality, ctv, reporting_unit, year, office, district,
         party, candidate, votes)

write_csv(all.2004, "processed-data/annual/2004.csv")


