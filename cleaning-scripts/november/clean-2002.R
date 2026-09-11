rm(list = ls())

library(tidyverse)

# This script standardizes the elections from 2002

################################################################################
# Governor
gov.colnames <- readxl::read_excel("~/Dropbox/Projects/2023/December/wisc-elections-archive/original-data/2002-11-05_GOV.xls",
                                   n_max = 2, col_names = F) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)
gov.2002.orig <- readxl::read_excel("~/Dropbox/Projects/2023/December/wisc-elections-archive/original-data/2002-11-05_GOV.xls",
                                    skip = 2, col_names = gov.colnames, col_types = "text") |>
  janitor::clean_names() |>
  filter(!is.na(election_type)) |>
  janitor::remove_empty("cols") |>
  # munge incorrect state senate number in Marquette Springfield T.
  mutate(state_senate_district = case_when(
    municipality_name == "Springfield" & county_name == "Marquette" ~ "24",
    TRUE ~ state_senate_district
  ))

gov.2002.clean <- gov.2002.orig |>
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         wsa_dist = state_assembly_district, wss_dist = state_senate_district, con_dist = us_congress_district,
         jim_doyle_barbara_lawton_democratic, scott_mc_callum_margaret_a_farrow_republican,
         jim_young_jeff_peterson_wisconsin_green, ed_thompson_m_reynolds_libertarian,
         alan_d_eisenberg_independent, ty_a_bollerud_independent, mike_mangan_independent,
         aneb_jah_rasta_independent, scattering_na) |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) |>
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name,
                         wsa_dist, wss_dist, con_dist),
               names_to = "name", values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "jim_doyle_barbara_lawton_democratic" ~ "Jim Doyle / Barbara Lawton",
      name == "scott_mc_callum_margaret_a_farrow_republican" ~ "Scott McCallum / Margaret A Farrow",
      name == "jim_young_jeff_peterson_wisconsin_green" ~ "Jim Young / Jeff Peterson",
      name == "ed_thompson_m_reynolds_libertarian" ~ "Ed thompson / M Reynolds",
      name == "alan_d_eisenberg_independent" ~ "Alan D Eisenberg",
      name == "ty_a_bollerud_independent" ~ "Ty A Bollerud",
      name == "mike_mangan_independent" ~ "Mike Mangan",
      name == "aneb_jah_rasta_independent" ~ "Aneb Jab Rasta",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "jim_doyle_barbara_lawton_democratic" ~ "Democratic",
      name == "scott_mc_callum_margaret_a_farrow_republican" ~ "Republican",
      name == "jim_young_jeff_peterson_wisconsin_green" ~ "Wisconsin Green",
      name == "ed_thompson_m_reynolds_libertarian" ~ "Libertarian",
      name == "alan_d_eisenberg_independent" ~ "Independent",
      name == "ty_a_bollerud_independent" ~ "Independent 2",
      name == "mike_mangan_independent" ~ "Independent 3",
      name == "aneb_jah_rasta_independent" ~ "Independent 4",
      name == "scattering_na" ~ "Scattering")
  )

gov.2002.clean |>
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

gov.2002.clean |>
  group_by(candidate) |>
  summarise(count = n())

################################################################################
# Congress
con.2002.orig <- readxl::read_excel("original-data/2002-11-05_USCONG.xls",
                                    col_types = "text", col_names = FALSE) |>
  janitor::clean_names()

dist.start <- which(con.2002.orig$x6 == "US CONGRESS")
dist.colnames <- con.2002.orig[sort(c(dist.start, dist.start+1)),] |>
  mutate(id = rep(1:8, each = 2)) |>
  group_by(id) |>
  mutate(rownum = row_number()) |>
  ungroup() |>
  pivot_longer(cols = -c(id, rownum)) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  select(id, name, colname)

read_cong_district <- function(district){
  readxl::read_excel("original-data/2002-11-05_USCONG.xls",
                     col_types = "text", skip = (dist.start[district] + 1),
                     col_names = dist.colnames$colname[dist.colnames$id == district]) |>
    filter(OFFICE_NAME == OFFICE_NAME[row_number() == 1]) |>
    janitor::clean_names() |>
    janitor::remove_empty("cols") |>
    select(-c(election_date, election_type, election_subtype, office_type_keyword,
              office_name, state_senate_district, state_assembly_district, 
              court_of_appeals_district, county_number, hindi_number, order_number,
              municipality_number)) |>
    mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) |>
    pivot_longer(cols = -c(county_name, municipality_name, municipality_type,
                           reporting_unit_name, us_congress_district),
                 names_to = "name", values_to = "votes")
}

all.dists <- map_df(1:8, read_cong_district)

con.2002.clean <- all.dists |>
  mutate(
    candidate = case_when(
      name == "george_meyers_libertarian" ~ "Goerge Meyers",
      name == "jeffrey_c_thomas_democratic" ~ "Jeffrey C Thomas",
      name == "paul_ryan_republican" ~ "Paul Ryan",
      name == "ron_greer_republican" ~ "Ron Greer",
      name == "tammy_baldwin_democratic" ~ "Tammy Baldwin",
      name == "bill_arndt_republican" ~ "Bill Arndt",
      name == "jeff_zastrow_libertarian" ~ "Jeff Zastrow",
      name == "ron_kind_democratic" ~ "Ron Kind",
      name == "brian_verdin_wisconsin_green" ~ "Brian Verdin",
      name == "jerry_kleczka_democratic" ~ "Jerry Kleczka",
      name == "f_james_sensenbrenner_jr_republican" ~ "F James Sensenbrenner, Jr",
      name == "robert_raymond_independent" ~ "Robert Raymond",
      name == "tom_petri_republican" ~ "Tom Petri",
      name == "david_r_obey_democratic" ~ "David R Obey",
      name == "joe_rothbauer_republican" ~ "Joe Rothbauer",
      name == "andrew_m_becker_democratic" ~ "Andrew M Becker",
      name == "dick_kaiser_wisconsin_green" ~ "Dick Kaiser",
      name == "mark_green_republican" ~ "Mark Green",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      str_detect(name, "_democratic") ~ "Democratic",
      str_detect(name, "_republican") ~ "Republican",
      str_detect(name, "_libertarian") ~ "Libertarian",
      str_detect(name, "_wisconsin_green") ~ "Wisconsin Green",
      str_detect(name, "_independent") ~ "Independent",
      name == "scattering_na" ~ "Scattering")
  )

con.2002.clean |>
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

con.2002.clean |>
  group_by(candidate, party, us_congress_district) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# Combine
all.2002 <- bind_rows(
  gov.2002.clean |>
    mutate(office = "governor") |>
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           wsa_dist, wss_dist, con_dist,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2002.clean |>
    mutate(office = "congress") |>
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           district = us_congress_district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) |>
  mutate(year = 2002) |>
  select(county, municipality, ctv, reporting_unit, year, office, district,
         wsa_dist, wss_dist, con_dist, party, candidate, votes) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  arrange(wsa_dist) |>
  mutate(wsa_dist = first(wsa_dist[office == "governor"]),
         wss_dist = first(wss_dist[office == "governor"]),
         con_dist = first(con_dist[office == "governor"])) |>
  ungroup()

write_csv(all.2002, "processed-data/annual/2002.csv")
