rm(list = ls())

library(tidyverse)

# this script cleans election files from 2018

################################################################################
# govident 2018
gov.2018.orig <- readxl::read_excel("original-data/2018%20General%20Election%20Governor%20Contest%20Candidates%20with%20Districts.xlsx") |>
  janitor::clean_names()

glimpse(gov.2018.orig)

gov.2018.clean <- gov.2018.orig |>
  rename(wss_dist = senate_district, wsa_dist = assembly_district, con_dist = congressional_district) |>
  pivot_longer(cols = -c(county, muni, reporting_unit_name,
                         wss_dist, wsa_dist, con_dist),
               values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "scott_walker_rebecca_kleefisch_rep" ~ "Scott Walker / Rebecca Kleefisch",
      name == "tony_evers_mandela_barnes_dem" ~ "Tony Evers / Mandela Barnes",
      name == "phillip_anderson_patrick_baird_lib" ~ "Phillip Anderson / Patrick Baird",
      name == "michael_j_white_tiffany_anderson_wgr" ~ "Michael J White / Tiffany Anderson",
      name == "maggie_turnbull_wil_losch_ind" ~ "Maggie Turnbull / Wil Losch",
      name == "arnie_enz_no_candidate_ind" ~ "Arnie Enz",
      name == "ryan_cason_write_in_no_candidate_rep" ~ "Ryan Cason",
      name == "paul_boucher_write_in_no_candidate_dem" ~ "Paul Boucher",
      name == "mark_s_grimek_write_in_no_candidate_con" ~ "Mark S Grimek",
      name == "jared_william_landry_write_in_no_candidate_dem" ~ "Jared William Landry",
      name == "robbie_hoffman_write_in_no_candidate_ind" ~ "Robbie Hoffman",
      name == "richard_michael_turtenwald_write_in_no_candidate_ind" ~ "Richard Michael Turtenwald",
      name == "no_candidate_corban_gehler_write_in_dem" ~ "Corhan Gehler",
      name == "no_candidate_william_henry_davis_iii_write_in_dem" ~ "William Henry David III",
      name == "scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "scott_walker_rebecca_kleefisch_rep" ~ "Republican",
      name == "tony_evers_mandela_barnes_dem" ~ "Democratic",
      name == "phillip_anderson_patrick_baird_lib" ~ "Libertarian",
      name == "michael_j_white_tiffany_anderson_wgr" ~ "Wisconsin Green",
      name == "maggie_turnbull_wil_losch_ind" ~ "Independent",
      name == "arnie_enz_no_candidate_ind" ~ "Independent 2",
      name == "ryan_cason_write_in_no_candidate_rep" ~ "Write-in",
      name == "paul_boucher_write_in_no_candidate_dem" ~ "Write-in 2",
      name == "mark_s_grimek_write_in_no_candidate_con" ~ "Write-in 3",
      name == "jared_william_landry_write_in_no_candidate_dem" ~ "Write-in 4",
      name == "robbie_hoffman_write_in_no_candidate_ind" ~ "Write-in 5",
      name == "richard_michael_turtenwald_write_in_no_candidate_ind" ~ "Write-in 6",
      name == "no_candidate_corban_gehler_write_in_dem" ~ "Write-in 7",
      name == "no_candidate_william_henry_davis_iii_write_in_dem" ~ "Write-in 8",
      name == "scattering" ~ "Scattering"
    )
  ) |>
  separate(reporting_unit_name, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward|WARD)") |>
  mutate(municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

gov.2018.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

gov.2018.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# senate 2018
sen.2018.orig <- readxl::read_excel("original-data/US%20Senator_WardByWard_withDistricts%202018%20General_0.xlsx") |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(reporting_unit_name != "County Totals:",
         !is.na(reporting_unit_name))

sen.2018.clean <- sen.2018.orig |>
  select(-contains("district")) |>
  pivot_longer(cols = -c(1:3), values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "leah_vukmir_rep" ~ "Leah Vukmir",
      name == "tammy_baldwin_dem" ~ "Tammy Baldwin",
      name == "mary_jo_walters_write_in_ind" ~ "Mary Jo Walters",
      name == "john_schiess_write_in_ind" ~ "John Schiess",
      name == "scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "leah_vukmir_rep" ~ "Republican",
      name == "tammy_baldwin_dem" ~ "Democratic",
      name == "mary_jo_walters_write_in_ind" ~ "Write-in",
      name == "john_schiess_write_in_ind" ~ "Write-in 2",
      name == "scattering" ~ "Scattering"
    )
  ) |>
  separate(reporting_unit_name, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward|WARD)") |>
  mutate(municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

sen.2018.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

sen.2018.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# congress 2018
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/Ward%20by%20Ward%20Report-Gen%20Election-Congress_0.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[which(str_detect(dist$...1, "REPRESENTATIVE IN CONGRESS "))]
  colname.start <- which(str_detect(dist$...3, "Total Votes Cast"))
  dist.colnames <- tibble(x1 = as.character(dist[colname.start,]),
                          x2 = as.character(dist[colname.start + 1,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/Ward%20by%20Ward%20Report-Gen%20Election-Congress_0.xlsx",
                     sheet = sheet, skip = (colname.start + 1), col_names = dist.colnames) |>
    janitor::clean_names() |>
    janitor::remove_empty("cols") |>
    rename(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county)) |>
    filter(!is.na(rep_unit), 
           rep_unit != "County Totals:") |>
    pivot_longer(cols = -c(1:3), values_to = "votes") |>
    mutate(district = districtno)
}

all.dist.orig <- map_df(2:9, read_cong_dist)

con.2018.clean <- all.dist.orig |>
  mutate(
    candidate = case_when(
      name == "dem_randy_bryce" ~ "Randy Bryce",
      name == "ind_joseph_kexel_write_in" ~ "Joseph Kexel",
      name == "ind_ken_yorgan" ~ "Ken Yorgan",
      name == "rep_bryan_steil" ~ "Bryan Steil",
      name == "dem_bradley_jason_burt_write_in" ~ "Bradley Jason Burt",
      name == "dem_mark_pocan" ~ "Mark Pocan",
      name == "ind_rick_cruz_write_in" ~ "Rick Cruz",
      name == "rep_joey_wayne_reed_write_in" ~ "Joey Wayne Reed",
      name == "dem_ron_kind" ~ "Ron Kind",
      name == "rep_steve_toft" ~ "Steve Toft",
      name == "dem_gwen_s_moore" ~ "Gwen S Moore",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "rep_tim_rogers" ~ "Tim Rogers",
      name == "dem_ramon_garcia_write_in" ~ "Ramon Garcia",
      name == "dem_tom_palzewicz" ~ "Tom Palzewicz",
      name == "rep_f_james_sensenbrenner_jr" ~ "F James Sensenbrenner, Jr",
      name == "dem_dan_kohl" ~ "Dan Kohl",
      name == "rep_glenn_grothman" ~ "Glenn Grothman",
      name == "dem_bob_look_write_in" ~ "Bob Look",
      name == "dem_margaret_engebretson" ~ "Margaret Engebretson",
      name == "ind_ken_driessen" ~ "Ken Driessen",
      name == "rep_sean_p_duffy" ~ "Sean P Duffy",
      name == "dem_beau_liegeois" ~ "Beau Liegeois",
      name == "rep_mike_gallagher" ~ "Mike Gallagher",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_randy_bryce" ~ "Democratic",
      name == "ind_joseph_kexel_write_in" ~ "Write-in",
      name == "ind_ken_yorgan" ~ "Independent",
      name == "rep_bryan_steil" ~ "Republican",
      name == "dem_bradley_jason_burt_write_in" ~ "Write-in",
      name == "dem_mark_pocan" ~ "Democratic",
      name == "ind_rick_cruz_write_in" ~ "Write-in 2",
      name == "rep_joey_wayne_reed_write_in" ~ "Write-in 3",
      name == "dem_ron_kind" ~ "Democratic",
      name == "rep_steve_toft" ~ "Republican",
      name == "dem_gwen_s_moore" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "rep_tim_rogers" ~ "Republican",
      name == "dem_ramon_garcia_write_in" ~ "Write-in",
      name == "dem_tom_palzewicz" ~ "Democratic",
      name == "rep_f_james_sensenbrenner_jr" ~ "Republican",
      name == "dem_dan_kohl" ~ "Democratic",
      name == "rep_glenn_grothman" ~ "Republican",
      name == "dem_bob_look_write_in" ~ "Write-in",
      name == "dem_margaret_engebretson" ~ "Democratic",
      name == "ind_ken_driessen" ~ "Independent",
      name == "rep_sean_p_duffy" ~ "Republican",
      name == "dem_beau_liegeois" ~ "Democratic",
      name == "rep_mike_gallagher" ~ "Republican",
      name == "na_scattering" ~ "Scattering")
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

con.2018.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate, district) |>
  filter(n() > 1)

con.2018.clean |>
  group_by(candidate, party, district) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 32)

################################################################################
# Combine
all.2018 <- bind_rows(
  gov.2018.clean |>
    mutate(office = "governor") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes,
           wss_dist, wsa_dist, con_dist),
  sen.2018.clean |>
    mutate(office = "senate") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2018.clean |>
    mutate(office = "congress") |>
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) |>
  mutate(year = 2018,
         across(where(is.character), str_squish)) |>
  select(county, municipality, ctv, reporting_unit, year, office, district,
         wss_dist, wsa_dist, con_dist, party, candidate, votes) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(wsa_dist = first(wsa_dist[office == "governor"]),
         wss_dist = first(wss_dist[office == "governor"]),
         con_dist = first(con_dist[office == "governor"])) |>
  ungroup() |>
  mutate(wsa_dist = as.numeric(str_sub(wsa_dist, -2, -1)),
         wss_dist = as.numeric(str_sub(wss_dist, -2, -1)),
         con_dist = as.numeric(str_sub(con_dist, -2, -1)))

write_csv(all.2018, "processed-data/annual/2018.csv")
