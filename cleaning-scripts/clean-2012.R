rm(list = ls())

library(tidyverse)

# all results are in 1 excel workbook
sheets.2012 <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                                  sheet = 1) |>
  mutate(sheetno = row_number() + 1)

sheets.2012

legis.dists <- read_csv("processed-data/annual/2010-2014-rep-unit-dists.csv") |>
  filter(year == 2012) |>
  select(-year)

################################################################################
# president 2012
pres.2012.colnames <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                                        sheet = 2, col_names = FALSE,
                                        skip = 8, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

pres.2012.orig <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                                    sheet = 2, skip = 11,
                                    col_names = c("county", "rep_unit", pres.2012.colnames)) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(str_detect(rep_unit, "County Totals", negate = T),
         !is.na(rep_unit)) |>
  left_join(legis.dists)

pres.2012.clean <- pres.2012.orig |>
  pivot_longer(cols = -c(county, rep_unit, total_votes_cast_na, wsa_dist, wss_dist,
                         con_dist),
               values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "rep_mitt_romney_paul_ryan" ~ "Mitt Romney / Paul Ryan",
      name == "dem_barack_obama_joe_biden" ~ "Barack Obama / Joe Biden",
      name == "con_virgil_goode_jim_clymer" ~ "Virgil Goode / Jim Clymer",
      name == "ind_gary_johnson_james_p_gray" ~ "Gary Johnson / James P Gray",
      name == "ind_gloria_la_riva_filberto_ramirez_jr" ~ "Gloria la Riva / Filberto Ramirez, Jr",
      name == "ind_jerry_white_phyllis_scherrer" ~ "Jerry White / Phyllis Scherrer",
      name == "ind_jill_stein_ben_manski" ~ "Jill Stein / Ben Manski",
      name == "ind_ross_c_rocky_anderson_luis_j_rodriguez_write_in" ~ "Ross C Rocky Anderson / Luis J Rodriguez",
      name == "ind_roseanne_barr_cindy_lee_sheehan_write_in" ~ "Roseanne Barr / Cindy Lee Sheehan",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "rep_mitt_romney_paul_ryan" ~ "Republican",
      name == "dem_barack_obama_joe_biden" ~ "Democratic",
      name == "con_virgil_goode_jim_clymer" ~ "Constitution",
      name == "ind_gary_johnson_james_p_gray" ~ "Independent",
      name == "ind_gloria_la_riva_filberto_ramirez_jr" ~ "Independent 2",
      name == "ind_jerry_white_phyllis_scherrer" ~ "Independent 3",
      name == "ind_jill_stein_ben_manski" ~ "Independent 4",
      name == "ind_ross_c_rocky_anderson_luis_j_rodriguez_write_in" ~ "Write-in",
      name == "ind_roseanne_barr_cindy_lee_sheehan_write_in" ~ "Write-in 2",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))
pres.2012.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

pres.2012.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# senator 2012
sen.2012.colnames <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                                        sheet = 3, col_names = FALSE,
                                        skip = 8, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

sen.2012.orig <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                                    sheet = 3, skip = 11,
                                    col_names = c("county", "rep_unit", sen.2012.colnames)) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(str_detect(rep_unit, "County Totals", negate = T),
         !is.na(rep_unit))

sen.2012.clean <- sen.2012.orig |>
  pivot_longer(cols = -c(1:3), values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "dem_tammy_baldwin" ~ "Tammy Baldwin",
      name == "rep_tommy_g_thompson" ~ "Tommy G Thompson",
      name == "ind_joseph_kexel" ~ "Joseph Kexel",
      name == "ind_nimrod_y_u_allen_iii" ~ "Nimrod Y U Allen, III",
      name == "con_riley_j_hood_write_in" ~ "Riley J Hood",
      name == "ind_diane_e_lorbiecki_write_in" ~ "Diane E Lorbiecki",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_tammy_baldwin" ~ "Democratic",
      name == "rep_tommy_g_thompson" ~ "Republican",
      name == "ind_joseph_kexel" ~ "Independent",
      name == "ind_nimrod_y_u_allen_iii" ~ "Independent 2",
      name == "con_riley_j_hood_write_in" ~ "Write-in",
      name == "ind_diane_e_lorbiecki_write_in" ~ "Write-in 2",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))
sen.2012.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

sen.2012.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# congress 2012
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[8]
  dist.colnames <- tibble(x1 = as.character(dist[10,]),
                          x2 = as.character(dist[11,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
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

all.dist.orig <- map_df(4:11, read_cong_dist)

con.2012.clean <- all.dist.orig |>
  mutate(
    candidate = case_when(
      name == "dem_rob_zerban" ~ "Rob Zerban",
      name == "ind_keith_deschler" ~ "Keith Deschler",
      name == "rep_paul_ryan" ~ "Paul Ryan",
      name == "dem_mark_pocan" ~ "Mark Pocan",
      name == "ind_joe_kopsick_write_in" ~ "Joe Kopsick",
      name == "rep_chad_lee" ~ "Chad Lee",
      name == "dem_ron_kind" ~ "Ron Kind",
      name == "rep_ray_boland" ~ "Ray Boland",
      name == "dem_gwen_moore" ~ "Gwen Moore",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "rep_dan_sebring" ~ "Dan Sebring",
      name == "dem_dave_heaster" ~ "Dave Heaster",
      name == "rep_f_james_sensenbrenner_jr" ~ "F James Sensenbrenner, Jr",
      name == "dem_joe_kallas" ~ "Joe Kallas",
      name == "rep_tom_petri" ~ "Tom Petri",
      name == "dem_pat_kreitlow" ~ "Pat Kreitlow",
      name == "ind_dale_c_lehner_write_in" ~ "Dale C Lehner",
      name == "rep_sean_duffy" ~ "Sean Duffy",
      name == "dem_jamie_wall" ~ "Jamie Wall",
      name == "rep_reid_j_ribble" ~ "Reid J Ribble",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_rob_zerban" ~ "Democratic",
      name == "ind_keith_deschler" ~ "Independent",
      name == "rep_paul_ryan" ~ "Republican",
      name == "dem_mark_pocan" ~ "Democratic",
      name == "ind_joe_kopsick_write_in" ~ "Write-in",
      name == "rep_chad_lee" ~ "Republican",
      name == "dem_ron_kind" ~ "Democratic",
      name == "rep_ray_boland" ~ "Republican",
      name == "dem_gwen_moore" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "rep_dan_sebring" ~ "Republican",
      name == "dem_dave_heaster" ~ "Democratic",
      name == "rep_f_james_sensenbrenner_jr" ~ "Republican",
      name == "dem_joe_kallas" ~ "Democratic",
      name == "rep_tom_petri" ~ "Republican",
      name == "dem_pat_kreitlow" ~ "Democratic",
      name == "ind_dale_c_lehner_write_in" ~ "Write-in",
      name == "rep_sean_duffy" ~ "Republican",
      name == "dem_jamie_wall" ~ "Democratic",
      name == "rep_reid_j_ribble" ~ "Republican",
      name == "na_scattering" ~ "Scattering")
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

con.2012.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate, district) |>
  filter(n() > 1)

con.2012.clean |>
  group_by(candidate, party, district) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 29)

################################################################################
# Combine
all.2012 <- bind_rows(
  pres.2012.clean |>
    mutate(office = "president") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           con_dist, wss_dist, wsa_dist,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2012.clean |>
    mutate(office = "congress") |>
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes),
  sen.2012.clean |>
    mutate(office = "senate") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes)
) |>
  mutate(year = 2012,
         across(where(is.character), str_squish)) |>
  select(county, municipality, ctv, reporting_unit, year, office, district,
         con_dist, wss_dist, wsa_dist, party, candidate, votes) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(con_dist = unique(con_dist[office == "president"]),
         wss_dist = unique(wss_dist[office == "president"]),
         wsa_dist = unique(wsa_dist[office == "president"])) |>
  ungroup() |>
  # add 2012 gubernatorial recall (legislative districts not available)
  bind_rows(read_csv("processed-data/annual/2012-gov.csv",
                     col_types = "ccccnccccn"))

write_csv(all.2012, "processed-data/annual/2012.csv")
