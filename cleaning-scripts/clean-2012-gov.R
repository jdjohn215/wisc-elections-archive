rm(list = ls())

library(tidyverse)

gov.2012.colnames <- readxl::read_excel("original-data/Ward Results_6.5.12 Recall Election_all offices_post sen 21 recount.xls",
                                    sheet = 2, col_names = FALSE,
                                    skip = 9, n_max = 2) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)

gov.2012.orig <- readxl::read_excel("original-data/Ward Results_6.5.12 Recall Election_all offices_post sen 21 recount.xls",
                                    sheet = 2, skip = 11,
                                    col_names = c("county", "rep_unit", gov.2012.colnames)) |>
  rename(county = 1, rep_unit = 2) |>
  mutate(county = zoo::na.locf(county)) |>
  janitor::clean_names() |>
  janitor::remove_empty("cols") |>
  filter(str_detect(rep_unit, "County Totals", negate = T),
         !is.na(rep_unit))


gov.2012.clean <- gov.2012.orig |>
  pivot_longer(cols = -c(1:3), values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "dem_tom_barrett" ~ "Tom Barrett",
      name == "rep_scott_walker" ~ "Scott Walker",
      name == "ind_hari_trivedi" ~ "Hari Trivedi",
      name == "na_scattering" ~ "Scattering"
    ),
    party = case_when(
      name == "dem_tom_barrett" ~ "Democratic",
      name == "rep_scott_walker" ~ "Republican",
      name == "ind_hari_trivedi" ~ "Independent",
      name == "na_scattering" ~ "Scattering"
    )
  ) |>
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward|WARD|ward|\\bWD\\b|\\bWDS\\b)") |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(municipality_name, 1, 1),
         municipality_name = word(municipality_name, 3, -1))

gov.2012.clean |>
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) |>
  filter(n() > 1)

gov.2012.clean |>
  group_by(candidate, party) |>
  summarise(count = n()) |>
  arrange(count) |>
  print(n = 28)

gov.2012.final <- gov.2012.clean |>
  mutate(office = "governor") %>%
  select(county = county, municipality = municipality_name, ctv = municipality_type,
         reporting_unit = reporting_unit_name, office, party, candidate, votes) |>
  mutate(year = 2012,
         district = 0,
         across(where(is.character), str_squish)) %>%
  select(county, municipality, ctv, reporting_unit, year, office, district,
         party, candidate, votes)


write_csv(gov.2012.final, "processed-data/annual/2012-gov.csv")
