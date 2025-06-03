rm(list = ls())

library(tidyverse)

# This script does the following:
#   - combines each year's election results into a single dataframe
#   - performs municipality and county name standardization
#   - verifies that election totals match those collected by Dave Leip's Atlas
#   - matches each minor civil division to its census FIPS code

################################################################################
# combine election files
all.files <- map_df(list.files("processed-data/nonpartisan/annual", full.names = T, pattern = "[0-9].csv"),
                    read_csv) |>
  mutate(office = "SUPREME COURT",
         ctv = str_sub(ctv, 1, 1))


################################################################################
# standardize fields
clean.results <- all.files |>
  mutate(across(where(is.character), str_to_upper)) |>
  # formatting to clean up inconsistent name formats
  mutate(county = str_remove(str_to_upper(county), " COUNTY$"),
         county = replace(county, county == "LACROSSE", "LA CROSSE"),
         municipality = replace(municipality, municipality == "MERRIMAC MERRIMAC TOWN", "MERRIMAC"),
         municipality = str_remove_all(municipality, "[.]"),
         ctv = str_to_upper(str_sub(ctv, 1, 1)),
         municipality = case_when(
           municipality == "GRANDVIEW" ~ "GRAND VIEW",
           municipality == "MT STERLING" ~ "MOUNT STERLING",
           municipality == "LAND O-LAKES" ~ "LAND O'LAKES",
           municipality == "LAVALLE" ~ "LA VALLE",
           municipality == "POYSIPPI" ~ "POY SIPPI",
           municipality == "DE FOREST" ~ "DEFOREST",
           municipality == "CLAYBANKS" ~ "CLAY BANKS",
           municipality == "FONTANA" ~ "FONTANA-ON-GENEVA LAKE",
           municipality == "SAINT LAWRENCE" ~ "ST LAWRENCE",
           municipality == "BUFFALO" & ctv == "C" ~ "BUFFALO CITY",
           TRUE ~ municipality
         ))

# confirm that fields uniquely identify rows
multiples <- clean.results |>
  group_by(county, municipality, ctv, year, reporting_unit, office, party) |>
  summarise(count = n()) |>
  filter(count > 1)
nrow(multiples) == 0

###############################################################################
# compare each race's total votes with the total from the original file
race.totals <- readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx")

compare.totals <- left_join(
  clean.results |>
    group_by(year, office) |>
    summarise(my_total = sum(votes)),
  race.totals |>
    pivot_longer(cols = -c(year), values_to = "official_total")
) |>
  mutate(my_total = if_else(is.na(my_total), 0, my_total),
         match = my_total == official_total,
         diff = my_total - official_total)

mismatch <- compare.totals |> filter(match == FALSE | is.na(match))
mismatch |> group_by(year, office) |> summarise(count = n())

###############################################################################
# Add minor civil division FIPS code
mcd.codes <- sf::st_read("processed-data/mcd-boundaries-2000_2009-2025.geojson") |>
  sf::st_drop_geometry() |>
  tibble() |>
  group_by(mcd_fips, county, municipality, ctv) |>
  summarise() |>
  ungroup()

# demonstrate that each minor civil division is uniquely identified by mcd_fips
mcd.codes |>
  group_by(county, municipality, ctv) |>
  filter(n() > 1)

# add MCD FIPS to election results
clean.results.with.fips <- clean.results |>
  left_join(mcd.codes)

clean.results.with.fips |> filter(is.na(mcd_fips))

clean.results.with.fips.valid <- clean.results.with.fips |>
  filter(!is.na(mcd_fips)) |>
  mutate(county_fips = str_sub(mcd_fips, 1, 5)) %>%
  select(mcd_fips, county_fips, county, municipality, ctv, reporting_unit, year,
         office, party, candidate, votes) |>
  # add total column
  group_by(mcd_fips, county, municipality, ctv, reporting_unit, year, office) |>
  mutate(total_votes = sum(votes)) |>
  ungroup() |>
  mutate(office = if_else(office == "SENATE", "US SENATE", office))

###############################################################################
# Save results by reporting unit, minor civil division, municipality, and county
#   by reporting unit
write_csv(clean.results.with.fips.valid, "processed-data/nonpartisan/AllElections_ReportingUnit.csv")
write_csv(clean.results.with.fips.valid, "processed-data/nonpartisan/AllElections_ReportingUnit.csv.gz")

#   by minor civil division
mcd.totals <- clean.results.with.fips.valid |>
  group_by(county, county_fips, mcd_fips, municipality, ctv, year, office, party, candidate) |>
  summarise(votes = sum(votes)) |>
  group_by(mcd_fips, municipality, ctv, year, office) |>
  mutate(total_votes = sum(votes))
write_csv(mcd.totals, "processed-data/nonpartisan/AllElections_MinorCivilDivisions.csv")

# by municipality
municipality.totals <- clean.results.with.fips.valid |>
  mutate(
    muni_label = case_when(
      ctv == "T" ~ paste(ctv, municipality, paste0("(", county, ")")),
      TRUE ~ paste(ctv, municipality)
    ),
    muni_fips = paste0("55", str_sub(mcd_fips, -5, -1))) |>
  group_by(muni_fips, municipality, ctv, muni_label, year, office, party, candidate) |>
  summarise(votes = sum(votes)) |>
  group_by(muni_fips, municipality, ctv, muni_label, year, office) |>
  mutate(total_votes = sum(votes)) |>
  ungroup()
write_csv(municipality.totals, "processed-data/nonpartisan/AllElections_Municipalities.csv")

# by county
county.totals <- clean.results.with.fips.valid |>
  group_by(county_fips, county, year, office, party, candidate) |>
  summarise(votes = sum(votes)) |>
  group_by(county_fips, county, year, office) |>
  mutate(total_votes = sum(votes)) |>
  ungroup()
write_csv(county.totals, "processed-data/nonpartisan/AllElections_Counties.csv")
write_csv(county.totals, "processed-data/nonpartisan/AllElections_Counties.csv.gz")

