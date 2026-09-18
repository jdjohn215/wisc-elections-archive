rm(list = ls())

library(tidyverse)
library(arrow)

################################################################################
# the complete files for each election type
february <- read_csv("processed-data/february/AllElections_ReportingUnit.csv") |>
  mutate(month = "FEBRUARY")
april <- read_csv("processed-data/april/AllElections_ReportingUnit.csv") |>
  mutate(month = "APRIL")
august <- read_csv("processed-data/august/AllElections_ReportingUnit.csv") |>
  mutate(month = "AUGUST")
november <- read_csv("processed-data/november/AllElections_ReportingUnit.csv") |>
  # set the month of the 2012 gubernatorial recall to June
  mutate(month = case_when(
    year != 2012 ~ "NOVEMBER",
    office == "GOVERNOR" ~ "JUNE",
    TRUE ~ "NOVEMBER"
  ))

all.results <- bind_rows(february, april, august, november) |>
  # final edits
  mutate(
    district = case_when(
      office == "REPRESENTATIVE IN CONGRESS DISTRICT 8 (SPECIAL)" ~ 8,
      office %in% c("STATE ASSEMBLY", "STATE SENATE", "CONGRESS") ~ district,
      TRUE ~ 0
    ),
    office = case_when(
      office == "STATE SUPERINTENDENT OF PUBLIC INSTRUCTION" ~ "STATE SUPERINTENDENT",
      office == "JUSTICE OF THE SUPREME COURT" ~ "SUPREME COURT",
      TRUE ~ office
    ),
    muni_fips = paste0("55", str_sub(mcd_fips, -5, -1))) |>
  select(county_fips, muni_fips, mcd_fips, county, municipality, ctv, reporting_unit, everything())

# confirm no duplicates
all.results |> 
  group_by(mcd_fips, reporting_unit, year, month, office, district, party, candidate) |>
  filter(n() > 1)
################################################################################
# check place name consistency
n_distinct(all.results$county)
n_distinct(all.results$municipality)
n_distinct(all.results$mcd_fips)
all.results |> filter(is.na(mcd_fips))

# these are MCD_FIPS codes with more than 1 name. This can legitimately happen
# when a municipality incorporates (e.g. from a town to a village) or changes its name
different.names <- all.results |> distinct(mcd_fips, county, ctv, municipality) |> group_by(mcd_fips) |> filter(n() > 1) |> arrange(mcd_fips)
################################################################################
# coverage summary
election.totals <- all.results |>
  group_by(year, month, office, district) |>
  summarise(votes = sum(votes))

election.offices <- election.totals |>
  distinct(year, month, office) |>
  pivot_wider(names_from = month, values_from = office,
              values_fn = ~paste(.x, collapse = ", ")) |>
  select(year, FEBRUARY, APRIL, AUGUST, NOVEMBER)
################################################################################

write_csv(all.results, "processed-data/AllResults_ReportingUnit.csv.gz")

################################################################################
# write hive-partioned parquet dataset
all.results |>
  group_by(year, month, office) |>
  write_dataset(path = "processed-data/parquet/", format = "parquet",
                existing_data_behavior = "overwrite")
