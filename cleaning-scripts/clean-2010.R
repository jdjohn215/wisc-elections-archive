rm(list = ls())

library(tidyverse)

# all results are in 1 excel workbook
sheets.2010 <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                                  sheet = 1) |>
  mutate(sheetno = row_number() + 1)

sheets.2010

legis.dists <- read_csv("processed-data/annual/2010-2014-rep-unit-dists.csv") |>
  filter(year == 2010) |>
  select(-year)

################################################################################
# governor 2010
gov.2010.colnames <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                                        sheet = 2, col_names = FALSE,
                                        skip = 8, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

gov.2010.orig <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                                    sheet = 2, skip = 11,
                                    col_names = c("county", "rep_unit", gov.2010.colnames)) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(str_detect(rep_unit, "County Totals", negate = T),
         !is.na(rep_unit)) |>
  left_join(legis.dists)

gov.2010.clean <- gov.2010.orig |>
  pivot_longer(cols = -c(county, rep_unit, total_votes_cast_na, wsa_dist, wss_dist,
                         con_dist),
               values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "dem_tom_barrett_tom_nelson" ~ "Tom Barrett / Tom Nelson",
      name == "rep_scott_walker_rebecca_kleefisch" ~ "Scott Walker / Rebecca Kleefisch",
      name == "ind_hari_trivedi_no_candidate_write_in" ~ "Hari Trivedi",
      name == "ind_jim_langer_no_candidate" ~ "Jim Langer",
      name == "ind_leslie_ervin_smetak_david_myron_smetak_write_in" ~ "Leslie Ervin Smetak / David Myron Smetak",
      name == "ind_patricia_messicci_no_candidate_write_in" ~ "Patricia Messicci",
      name == "lib_no_candidate_terry_virgil" ~ "Terry Virgil",
      name == "na_james_james_no_candidate" ~ "James James",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_tom_barrett_tom_nelson" ~ "Democratic",
      name == "rep_scott_walker_rebecca_kleefisch" ~ "Republican",
      name == "ind_hari_trivedi_no_candidate_write_in" ~ "Write-in",
      name == "ind_jim_langer_no_candidate" ~ "Independent 2",
      name == "ind_leslie_ervin_smetak_david_myron_smetak_write_in" ~ "Write-in 2 ",
      name == "ind_patricia_messicci_no_candidate_write_in" ~ "Write-in 3",
      name == "lib_no_candidate_terry_virgil" ~ "Libertarian",
      name == "na_james_james_no_candidate" ~ "Independent",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))
gov.2010.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

gov.2010.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# senator 2010
sen.2010.colnames <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                                        sheet = 6, col_names = FALSE,
                                        skip = 8, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

sen.2010.orig <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                                    sheet = 6, skip = 11,
                                    col_names = c("county", "rep_unit", sen.2010.colnames)) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(str_detect(rep_unit, "County Totals", negate = T),
         !is.na(rep_unit))

sen.2010.clean <- sen.2010.orig |>
  pivot_longer(cols = -c(1:3), values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "dem_russ_feingold" ~ "Russ Feingold",
      name == "rep_ron_johnson" ~ "Ron Johnson",
      name == "na_rob_taylor" ~ "Rob Taylor",
      name == "ind_michael_d_laforest_write_in" ~ "Michael D LaForest",
      name == "rep_ernest_j_pagels_jr_write_in" ~ "Errnest J Pagels, Jr",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_russ_feingold" ~ "Democratic",
      name == "rep_ron_johnson" ~ "Republican",
      name == "na_rob_taylor" ~ "Independent",
      name == "rep_ernest_j_pagels_jr_write_in" ~ "Write-in",
      name == "ind_michael_d_laforest_write_in" ~ "Write-in 2",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))
sen.2010.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

sen.2010.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# congress 2010
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[8]
  dist.colnames <- tibble(x1 = as.character(dist[10,]),
                          x2 = as.character(dist[11,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                     sheet = sheet, skip = 11, col_names = dist.colnames) |>
    janitor::clean_names() |>
    janitor::remove_empty("cols") |>
    rename(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county)) |>
    filter(!is.na(rep_unit), 
           rep_unit != "County Totals:") |>
    pivot_longer(cols = -c(1:3), values_to = "votes") |>
    mutate(district = districtno)
}

all.dist.orig <- map_df(7:14, read_cong_dist)

con.2010.clean <- all.dist.orig |>
  mutate(
    candidate = case_when(
      name == "dem_john_heckenlively" ~ "John Heckenlively",
      name == "lib_joseph_kexel" ~ "Joseph Kexel",
      name == "rep_paul_ryan" ~ "Paul Ryan",
      name == "dem_tammy_baldwin" ~ "Tammy Baldwin",
      name == "rep_chad_lee" ~ "Chad Lee",
      name == "dem_ron_kind" ~ "Ron Kind",
      name == "na_michael_krsiean" ~ "Michael Krsiean",
      name == "rep_dan_kapanke" ~ "Dan Kapanke",
      name == "dem_gwen_moore" ~ "Gwen Moore",
      name == "na_eddie_ahmad_ayyash" ~ "Eddie Ahmad Ayyash",
      name == "rep_dan_sebring" ~ "Dan Sebring",
      name == "dem_todd_p_kolosso" ~ "Todd P Kolosso",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "rep_f_james_sensenbrenner_jr" ~ "F James Sensenbrenner, Jr",
      name == "dem_joseph_c_kallas" ~ "Joseph C Kallas",
      name == "rep_tom_petri" ~ "Tom Petri",
      name == "dem_julie_m_lassa" ~ "Julie M Lassa",
      name == "na_gary_kauther" ~ "Gary Kauther",
      name == "rep_sean_duffy" ~ "Sean Duffy",
      name == "dem_steven_l_kagen" ~ "Steven L Kagen",
      name == "rep_reid_j_ribble" ~ "Reid J Ribble",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_john_heckenlively" ~ "Democratic",
      name == "lib_joseph_kexel" ~ "Libertarian",
      name == "rep_paul_ryan" ~ "Republican",
      name == "dem_tammy_baldwin" ~ "Democratic",
      name == "rep_chad_lee" ~ "Republican",
      name == "dem_ron_kind" ~ "Democratic",
      name == "na_michael_krsiean" ~ "Independent",
      name == "rep_dan_kapanke" ~ "Republican",
      name == "dem_gwen_moore" ~ "Democratic",
      name == "na_eddie_ahmad_ayyash" ~ "Independent",
      name == "rep_dan_sebring" ~ "Republican",
      name == "dem_todd_p_kolosso" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "rep_f_james_sensenbrenner_jr" ~ "Republican",
      name == "dem_joseph_c_kallas" ~ "Democratic",
      name == "rep_tom_petri" ~ "Republican",
      name == "dem_julie_m_lassa" ~ "Democratic",
      name == "na_gary_kauther" ~ "Independent",
      name == "rep_sean_duffy" ~ "Republican",
      name == "dem_steven_l_kagen" ~ "Democratic",
      name == "rep_reid_j_ribble" ~ "Republican",
      name == "na_scattering" ~ "Scattering")
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward|WARD)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

  con.2010.clean |>
    group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
    filter(n() > 1)
  
  con.2010.clean |>
    group_by(candidate, party, district) |>
    summarise(count = n()) |>
    arrange(count) |>
    print(n = 29)
  
  ################################################################################
  # Combine
  all.2010 <- bind_rows(
    gov.2010.clean |>
      mutate(office = "governor") |>
      select(county = county, municipality = municipality_name, ctv = municipality_type,
             con_dist, wss_dist, wsa_dist,
             reporting_unit = reporting_unit_name, office, party, candidate, votes),
    con.2010.clean |>
      mutate(office = "congress") |>
      select(county, municipality = municipality_name, ctv = municipality_type,
             district, reporting_unit = reporting_unit_name,
             office, party, candidate, votes),
    sen.2010.clean |>
      mutate(office = "senate") |>
      select(county = county, municipality = municipality_name, ctv = municipality_type,
             reporting_unit = reporting_unit_name, office, party, candidate, votes)
  ) |>
    mutate(year = 2010,
           across(where(is.character), str_squish)) |>
    select(county, municipality, ctv, reporting_unit, year, office, district,
           con_dist, wss_dist, wsa_dist, party, candidate, votes) |>
    group_by(county, municipality, ctv, reporting_unit) |>
    mutate(con_dist = unique(con_dist[office == "governor"]),
           wss_dist = unique(wss_dist[office == "governor"]),
           wsa_dist = unique(wsa_dist[office == "governor"])) |>
    ungroup()
  
write_csv(all.2010, "processed-data/annual/2010.csv")
  