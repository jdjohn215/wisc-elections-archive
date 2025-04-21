rm(list = ls())

library(tidyverse)

# read the original sheet in its entirety
scowis.colnames <- readxl::read_excel("original-data/nonpartisan/2005-04-05_SpringElection_SupCt_WardbyWard_2005.xls",
                                    n_max = 2, col_names = F) |>
  mutate(rownum = row_number()) |>
  pivot_longer(cols = -rownum) |>
  pivot_wider(names_from = rownum, values_from = value) |>
  mutate(colname = paste(`1`, `2`, sep = "_")) |>
  pull(colname)
scowis.2005.orig <- readxl::read_excel("original-data/nonpartisan/2005-04-05_SpringElection_SupCt_WardbyWard_2005.xls",
                                     skip = 2, col_names = scowis.colnames, col_types = "text") |>
  janitor::clean_names() |>
  filter(!is.na(election_type)) |>
  janitor::remove_empty("cols")

scowis.2005.clean <- scowis.2005.orig |>
  select(county_name, municipality_name, municipality_type, reporting_unit_name,
         ann_w_bradley_non_partisan,
         scattering_na) |>
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name)) |>
  pivot_longer(cols = -c(county_name, municipality_name, municipality_type, reporting_unit_name),
               names_to = "name", values_to = "votes") |>
  mutate(
    candidate = case_when(
      name == "ann_w_bradley_non_partisan" ~ "Ann W Bradley",
      name == "scattering_na" ~ "Scattering"),
    party = case_when(
      name == "ann_w_bradley_non_partisan" ~ "Liberal",
      name == "scattering_na" ~ "Scattering")
  ) |>
  rename(municipality = municipality_name, ctv = municipality_type,
         reporting_unit = reporting_unit_name, county = county_name) |>
  select(-name) |>
  mutate(year = 2005) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(total_votes = sum(as.numeric(votes)))


scowis.2005.clean |>
  group_by(candidate) |>
  summarise(count = n())

write_csv(scowis.2005.clean, "processed-data/nonpartisan/annual/2005.csv")
