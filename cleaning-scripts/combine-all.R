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

all.results <- bind_rows(february, april, august, november)

# confirm no duplicates
all.results |> 
  group_by(mcd_fips, reporting_unit, year, month, office, district, party, candidate) |>
  filter(n() > 1)

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
  write_dataset(path = "processed-data/parquet/", format = "parquet")
