rm(list = ls())

library(tidyverse)

# read the original sheet in its entirety
scowis.colnames <- readxl::read_excel("original-data/nonpartisan/2008-04-01_SpringElection_SupCt_WardbyWard_2008.xls",
                                    n_max = 2, col_names = F) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)
scowis.2008.orig <- readxl::read_excel("original-data/nonpartisan/2008-04-01_SpringElection_SupCt_WardbyWard_2008.xls",
                                     skip = 2, col_names = scowis.colnames, col_types = "text") |>
  janitor::clean_names() |>
  filter(!is.na(election_type)) |>
  janitor::remove_empty("cols")

scowis.2008.clean <- scowis.2008.orig |>
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         mike_gableman_non_partisan, louis_butler_non_partisan,
         scattering_na) |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) |>
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "mike_gableman_non_partisan" ~ "Mike Gableman",
      name == "louis_butler_non_partisan" ~ "Louis Butler",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "mike_gableman_non_partisan" ~ "Conservative",
      name == "louis_butler_non_partisan" ~ "Liberal",
      name == "scattering_na" ~ "Scattering")
  ) |>
  rename(municipality = municipality_name, ctv = municipality_type,
         reporting_unit = reporting_unit_name, county = county_name) |>
  select(-name) |>
  mutate(year = 2008) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(total_votes = sum(as.numeric(votes)))

scowis.2008.clean |>
  group_by(candidate) |>
  summarise(count = n())

write_csv(scowis.2008.clean, "processed-data/nonpartisan/annual/2008.csv")
