rm(list = ls())

library(tidyverse)

# This script does the following:
#   - combines each year's election results into a single dataframe
#   - performs municipality and county name standardization
#   - matches each minor civil division to its census FIPS code

################################################################################
# combine election files
all.files <- map_df(list.files("processed-data/february/annual", full.names = T, pattern = "[0-9].csv"),
                    read_csv)


################################################################################
# standardize fields
clean.results <- all.files |>
  separate(reporting_unit, into = c("municipality", "reporting_unit"), sep = " (?=WARD)") |>
  mutate(reporting_unit = if_else(is.na(reporting_unit), "WARD 1", reporting_unit),
         ctv = str_sub(municipality, 1, 1),
         municipality = word(municipality, 3, -1)) |>
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
  group_by(county, municipality, ctv, year, reporting_unit, office, candidate) |>
  summarise(count = n()) |>
  filter(count > 1)
nrow(multiples) == 0

###############################################################################
# Add minor civil division FIPS code
mcd.codes <- sf::st_read("processed-data/mcd-boundaries-2000_2009-2024.geojson") |>
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

clean.results.with.fips |> filter(is.na(mcd_fips)) |>
  distinct(county, municipality, ctv, reporting_unit, year)

clean.results.with.fips.valid <- clean.results.with.fips |>
  filter(!is.na(mcd_fips)) |>
  mutate(county_fips = str_sub(mcd_fips, 1, 5)) %>%
  select(mcd_fips, county_fips, county, municipality, ctv, reporting_unit, year, office,
         party, candidate, votes) |>
  # add total column
  group_by(mcd_fips, county, municipality, ctv, reporting_unit, year, office) |>
  mutate(total_votes = sum(votes)) |>
  ungroup() |>
  mutate(office = if_else(office == "SENATE", "US SENATE", office))

###############################################################################
# Save results by reporting unit, minor civil division, municipality, and county
#   by reporting unit
write_csv(clean.results.with.fips.valid, "processed-data/february/AllElections_ReportingUnit.csv")
