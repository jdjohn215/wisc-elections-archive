rm(list = ls())

library(tidyverse)

# This script standardizes the elections from 2006


################################################################################
# Governor
gov.colnames <- readxl::read_excel("original-data/2006-11-07_FallElection_Governor_WardbyWard.xls",
                                    n_max = 2, col_names = F) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)
gov.2006.orig <- readxl::read_excel("original-data/2006-11-07_FallElection_Governor_WardbyWard.xls",
                                     skip = 2, col_names = gov.colnames, col_types = "text") |>
  janitor::clean_names() |>
  filter(!is.na(election_type)) |>
  janitor::remove_empty("cols")

gov.2006.clean <- gov.2006.orig |>
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         wsa_dist = state_assembly_district, wss_dist = state_senate_district, con_dist = us_congress_district,
         jim_doyle_barbara_c_lawton_democratic, mark_green_jean_hundertmark_republican,
         nelson_eisman_leon_todd_wisconsin_green, scattering_na) |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) |>
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name,
                         wsa_dist, wss_dist, con_dist),
               names_to = "name", values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "jim_doyle_barbara_c_lawton_democratic" ~ "Jim Doyle / Barbara C Lawton",
      name == "mark_green_jean_hundertmark_republican" ~ "Mark Green / Jean Hundertmark",
      name == "nelson_eisman_leon_todd_wisconsin_green" ~ "Nelson Eisman / Leon Todd",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "jim_doyle_barbara_c_lawton_democratic" ~ "Democratic",
      name == "mark_green_jean_hundertmark_republican" ~ "Republican",
      name == "nelson_eisman_leon_todd_wisconsin_green" ~ "Wisconsin Green",
      name == "scattering_na" ~ "Scattering")
  )

gov.2006.clean |>
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

gov.2006.clean |>
  group_by(candidate) |>
  summarise(count = n())

################################################################################
# Senate
sen.colnames <- readxl::read_excel("original-data/2006-11-07_FallElection_USSenate_WardbyWard.xls",
                                   n_max = 2, col_names = F) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)
sen.2006.orig <- readxl::read_excel("original-data/2006-11-07_FallElection_USSenate_WardbyWard.xls",
                                    skip = 2, col_names = sen.colnames, col_types = "text") |>
  janitor::clean_names() |>
  filter(!is.na(election_type)) |>
  janitor::remove_empty("cols")

sen.2006.clean <- sen.2006.orig |>
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         herb_kohl_democratic, robert_gerald_lorge_republican,
         rae_vogeler_wisconsin_green, ben_j_glatzel_independent, scattering_na) |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) |>
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "herb_kohl_democratic" ~ "Herb Kohl",
      name == "robert_gerald_lorge_republican" ~ "Robert Gerald Lorge",
      name == "rae_vogeler_wisconsin_green" ~ "Rae Vogeler",
      name == "ben_j_glatzel_independent" ~ "Ben J Glatzel",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "herb_kohl_democratic" ~ "Democratic",
      name == "robert_gerald_lorge_republican" ~ "Republican",
      name == "rae_vogeler_wisconsin_green" ~ "Wisconsin Green",
      name == "ben_j_glatzel_independent" ~ "Independent",
      name == "scattering_na" ~ "Scattering")
  )

sen.2006.clean |>
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

sen.2006.clean |>
  group_by(candidate) |>
  summarise(count = n())

################################################################################
# Congress
con.2006.orig <- readxl::read_excel("original-data/2006-11-07_FallElection_USCongress_WardbyWard.xls",
                                    col_types = "text", col_names = FALSE) |>
  janitor::clean_names()

dist.start <- which(con.2006.orig$x6 == "US CONGRESS")
dist.colnames <- con.2006.orig[sort(c(dist.start, dist.start+1)),] |>
  mutate(id = rep(1:8, each = 2)) |>
  group_by(id) |>
  mutate(rownum = row_number()) |>
  ungroup() |>
  pivot_longer(cols = -c(id, rownum)) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  select(id, name, colname)

read_cong_district <- function(district){
  df <- readxl::read_excel("original-data/2006-11-07_FallElection_USCongress_WardbyWard.xls",
                     col_types = "text", skip = (dist.start[district] + 1),
                     col_names = FALSE)
  names(df) <- dist.colnames$colname[dist.colnames$id == district][1:ncol(df)]
  df |>
    janitor::clean_names() |>
    filter(office_name == office_name[row_number() == 1]) |>
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

con.2006.clean <- all.dists |>
  mutate(
    candidate = case_when(
      name == "jeffrey_c_thomas_democratic" ~ "Jeffrey C Thomas",
      name == "paul_ryan_republican" ~ "Paul Ryan",
      name == "dave_magnum_republican" ~ "Dave Magnum",
      name == "tammy_baldwin_democratic" ~ "Tammy Baldwin",
      name == "paul_r_nelson_republican" ~ "Paul R Nelson",
      name == "ron_kind_democratic" ~ "Ron Kind",
      name == "gwen_moore_democratic" ~ "Gwen Moore",
      name == "perfecto_rivera_republican" ~ "Perfecto Rivera",
      name == "bob_levis_wisconsin_green" ~ "Bob Levis",
      name == "bryan_kennedy_democratic" ~ "Bryan Kennedy",
      name == "f_james_sensenbrenner_jr_republican" ~ "F James Sensenbrenner, Jr",
      name == "robert_r_raymond_independent" ~ "Robert R Raymond",
      name == "tom_petri_republican" ~ "Tom Petri",
      name == "david_r_obey_democratic" ~ "David R Obey",
      name == "mike_miles_wisconsin_green" ~ "Mike Miles",
      name == "nick_reid_republican" ~ "Nick Reig",
      name == "john_gard_republican" ~ "John Gard",
      name == "steven_l_kagen_democratic" ~ "Steven L Kagen",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      str_detect(name, "_democratic") ~ "Democratic",
      str_detect(name, "_republican") ~ "Republican",
      str_detect(name, "_libertarian") ~ "Libertarian",
      str_detect(name, "_wisconsin_green") ~ "Wisconsin Green",
      str_detect(name, "_independent") ~ "Independent",
      name == "scattering_na" ~ "Scattering")
  )

con.2006.clean |>
  group_by(county_name, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

con.2006.clean |>
  group_by(candidate, party, us_congress_district) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# Combine
all.2006 <- bind_rows(
  gov.2006.clean |>
    mutate(office = "governor") |>
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           wss_dist, wsa_dist, con_dist,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  sen.2006.clean |>
    mutate(office = "senate") |>
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2006.clean |>
    mutate(office = "congress") |>
    select(county = county_name, municipality = municipality_name, ctv = municipality_type,
           district = us_congress_district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) |>
  mutate(year = 2006) |>
  select(county, municipality, ctv, reporting_unit, year, office, district,
         wss_dist, wsa_dist, con_dist, party, candidate, votes) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(wsa_dist = first(wsa_dist[office == "governor"]),
         wss_dist = first(wss_dist[office == "governor"]),
         con_dist = first(con_dist[office == "governor"])) |>
  ungroup()

write_csv(all.2006, "processed-data/annual/2006.csv")
