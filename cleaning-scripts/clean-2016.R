rm(list = ls())

library(tidyverse)

# this script cleans election files from 2016

################################################################################
# president 2016
pres.2016.orig <- readxl::read_excel("original-data/PresidentContest_RecountResult_WardByWard_withDistricts.xlsx") |>
  janitor::clean_names()

glimpse(pres.2016.orig)

# party information from: https://elections.wi.gov/sites/default/files/legacy/Statewide%2520Results%2520All%2520Offices%2520%2528post-Presidential%2520recount%2529.pdf
pres.2016.clean <- pres.2016.orig |>
  rename(con_dist = congressional_district, wss_dist = senate_district, wsa_dist = assembly_district) |>
  pivot_longer(cols = -c(county_name, municipality_name, reporting_unit_text,
                         con_dist, wss_dist, wsa_dist),
               values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "donald_j_trump_michael_r_pence" ~ "Donald Trump / Mike Pence",
      name == "hillary_clinton_tim_kaine" ~ "Hillary Clinton / Tim Kaine",
      name == "darrell_l_castle_scott_n_bradley" ~ "Darrell L Caste / Scott N Bradley",
      name == "gary_johnson_bill_weld" ~ "Gary Johnson / Bill Weld",
      name == "jill_stein_ajamu_baraka" ~ "Jill Stein / Ajamu Baraka",
      name == "monica_moorehead_lamont_lilly" ~ "Monica Moorehead / Lamont Lilly",
      name == "rocky_roque_de_la_fuente_michael_steinberg" ~ "Rocky Roque De La Fuente / Michael Steinberg",
      name == "cherunda_fox_roger_kushner_write_in" ~ "Cherunda Fox / Roger Kushner",
      name == "evan_mc_mullin_nathan_johnson_write_in" ~ "Evan McMullin / Nathan Johnson",
      name == "michael_a_maturen_juan_munoz_write_in" ~ "Michael A Maturen / Juan Munoz",
      name == "marshall_schoenke_james_creighton_mitchell_jr_write_in" ~ "Marshall Schoenke / James Creighton Mitchell, Jr",
      name == "chris_keniston_deacon_taylor_write_in" ~ "Chris Keniston / Deacon Taylor",
      name == "laurence_kotlikoff_edward_e_leamer_write_in" ~ "Laurence Kotlikoff / Edward E Leamer",
      name == "tom_hoefling_steve_schulin_write_in" ~ "Tom Hoefling / Steve Schulin",
      name == "joseph_maldonado_write_in" ~ "Joseph Maldonado",
      name == "emidio_soltysik_angela_nicole_walker_write_in" ~ "Emidio Soltysik / Angela Nicole Walker",
      name == "scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "donald_j_trump_michael_r_pence" ~ "Republican",
      name == "hillary_clinton_tim_kaine" ~ "Democratic",
      name == "darrell_l_castle_scott_n_bradley" ~ "Constitution",
      name == "gary_johnson_bill_weld" ~ "Libertarian",
      name == "jill_stein_ajamu_baraka" ~ "Wisconsin Green",
      name == "monica_moorehead_lamont_lilly" ~ "Workers World Party",
      name == "rocky_roque_de_la_fuente_michael_steinberg" ~ "American Delta Party",
      name == "cherunda_fox_roger_kushner_write_in" ~ "Write-in",
      name == "evan_mc_mullin_nathan_johnson_write_in" ~ "Write-in 2",
      name == "michael_a_maturen_juan_munoz_write_in" ~ "Write-in 3",
      name == "marshall_schoenke_james_creighton_mitchell_jr_write_in" ~ "Write-in 4",
      name == "chris_keniston_deacon_taylor_write_in" ~ "Write-in 5",
      name == "laurence_kotlikoff_edward_e_leamer_write_in" ~ "Write-in 6",
      name == "tom_hoefling_steve_schulin_write_in" ~ "Write-in 7",
      name == "joseph_maldonado_write_in" ~ "Write-in 8",
      name == "emidio_soltysik_angela_nicole_walker_write_in" ~ "Write-in 9",
      name == "scattering" ~ "Scattering"
    )
  ) |>
  separate(reporting_unit_text, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward|WARD)") |>
  mutate(municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1)) |>
  rename(county = county_name)

pres.2016.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

pres.2016.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# senate 2016
sen.2016.colnames <- readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20-US%20Senator.xlsx",
                                        sheet = 2, col_names = FALSE,
                                        skip = 9, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

sen.2016.orig <- readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20-US%20Senator.xlsx",
                                    sheet = 2, skip = 11,
                                    col_names = sen.2016.colnames) |>
  rename(county = 1, rep_unit = 2) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(rep_unit != "County Totals:",
         !is.na(rep_unit))

sen.2016.clean <- sen.2016.orig |>
  pivot_longer(cols = -c(1:3), values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "rep_ron_johnson" ~ "Ron Johnson",
      name == "dem_russ_feingold" ~ "Russ Feingold",
      name == "lib_phillip_n_anderson" ~ "Phillip N Anderson",
      name == "rep_john_schiess_write_in" ~ "John Schiess",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "rep_ron_johnson" ~ "Republican",
      name == "dem_russ_feingold" ~ "Democratic",
      name == "lib_phillip_n_anderson" ~ "Libertarian",
      name == "rep_john_schiess_write_in" ~ "Write-in",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

sen.2016.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

sen.2016.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

################################################################################
# congress 2016
read_cong_dist <- function(sheet){
  dist <- readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20-Congress.xlsx",
                             sheet = sheet, col_names = F)
  
  districtno <- dist$...1[which(str_detect(dist$...1, "REPRESENTATIVE IN CONGRESS"))]
  colname.start <- which(str_detect(dist$...3, "Total Votes Cast"))
  dist.colnames <- tibble(x1 = as.character(dist[colname.start,]),
                          x2 = as.character(dist[colname.start + 1,])) |>
    mutate(colname = paste(x1, x2, sep = "_")) |>
    pull(colname)
  readxl::read_excel("original-data/Ward%20by%20Ward%20Report%20-Congress.xlsx",
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

con.2016.clean <- all.dist.orig |>
  mutate(
    candidate = case_when(
      name == "dem_ryan_solen" ~ "Ryan Solen",
      name == "ind_spencer_zimmerman" ~ "Spencer Zimmerman",
      name == "lib_jason_lebeck" ~ "Jason Lebeck",
      name == "rep_paul_ryan" ~ "Paul Ryan",
      name == "dem_mark_pocan" ~ "Mark Pocan",
      name == "rep_peter_theron" ~ "Peter Theron",
      name == "dem_ron_kind" ~ "Ron Kind",
      name == "rep_ryan_peterson_write_in" ~ "Ryan Peterson",
      name == "dem_gwen_s_moore" ~ "Gwen S Moore",
      name == "ind_robert_r_raymond" ~ "Robert R Raymond",
      name == "lib_andy_craig" ~ "Andy Craig",
      name == "dem_khary_penebaker" ~ "Khary Penebaker",
      name == "lib_john_arndt" ~ "John Arndt",
      name == "rep_f_james_sensenbrenner_jr" ~ "F James Sensenbrenner, Jr",
      name == "dem_sarah_lloyd" ~ "Sarah Lloyd",
      name == "ind_jeff_dahlke" ~ "Jeff Dahlke",
      name == "dem_tom_nelson" ~ "Tom Nelson",
      name == "rep_glenn_grothman" ~ "Glenn Grothman",
      name == "dem_mary_hoeft" ~ "Mary Hoeft",
      name == "rep_sean_duffy" ~ "Sean Duffy",
      name == "dem_jerry_kobishop_wr_in" ~ "Jerry Kobishop",
      name == "rep_mike_gallagher" ~ "Mike Gallagher",
      name == "wgr_wendy_gribben_write_in" ~ "Wendy Gribben",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_ryan_solen" ~ "Democratic",
      name == "ind_spencer_zimmerman" ~ "Independent",
      name == "lib_jason_lebeck" ~ "Libertarian",
      name == "rep_paul_ryan" ~ "Republican",
      name == "dem_mark_pocan" ~ "Democratic",
      name == "rep_peter_theron" ~ "Republican",
      name == "dem_ron_kind" ~ "Democratic",
      name == "rep_ryan_peterson_write_in" ~ "Write-in",
      name == "dem_gwen_s_moore" ~ "Democratic",
      name == "ind_robert_r_raymond" ~ "Independent",
      name == "lib_andy_craig" ~ "Libertarian",
      name == "dem_khary_penebaker" ~ "Democratic",
      name == "lib_john_arndt" ~ "Libertarian",
      name == "rep_f_james_sensenbrenner_jr" ~ "Republican",
      name == "dem_sarah_lloyd" ~ "Democratic",
      name == "ind_jeff_dahlke" ~ "Independent",
      name == "dem_tom_nelson" ~ "Democratic",
      name == "rep_glenn_grothman" ~ "Republican",
      name == "dem_mary_hoeft" ~ "Democratic",
      name == "rep_sean_duffy" ~ "Republican",
      name == "dem_jerry_kobishop_wr_in" ~ "Write-in",
      name == "rep_mike_gallagher" ~ "Republican",
      name == "wgr_wendy_gribben_write_in" ~ "Write-in 2",
      name == "na_scattering" ~ "Scattering")
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"), sep = " (?=Ward)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1),
         district = word(district, -1))

con.2016.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate, district) |>
  filter(n() > 1)

con.2016.clean |>
  group_by(candidate, party, district) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 31)

################################################################################
# Combine
all.2016 <- bind_rows(
  pres.2016.clean |>
    mutate(office = "president") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes,
           wss_dist, con_dist, wsa_dist),
  sen.2016.clean |>
    mutate(office = "senate") |>
    select(county = county, municipality = municipality_name, ctv = municipality_type,
           reporting_unit = reporting_unit_name, office, party, candidate, votes),
  con.2016.clean |>
    mutate(office = "congress") |>
    select(county, municipality = municipality_name, ctv = municipality_type,
           district, reporting_unit = reporting_unit_name,
           office, party, candidate, votes)
) |>
  mutate(year = 2016,
         across(where(is.character), str_squish)) |>
  select(county, municipality, ctv, reporting_unit, year, office, district,
         wss_dist, con_dist, wsa_dist, party, candidate, votes) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(wsa_dist = first(wsa_dist[office == "president"]),
         wss_dist = first(wss_dist[office == "president"]),
         con_dist = first(con_dist[office == "president"])) |>
  ungroup() |>
  mutate(wsa_dist = as.numeric(str_sub(wsa_dist, -2, -1)),
         wss_dist = as.numeric(str_sub(wss_dist, -2, -1)),
         con_dist = as.numeric(str_sub(con_dist, -2, -1)))

write_csv(all.2016, "processed-data/annual/2016.csv")
