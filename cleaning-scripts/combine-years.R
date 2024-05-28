rm(list = ls())

library(tidyverse)

# This script does the following:
#   - combines each year's election results into a single dataframe
#   - performs municipality and county name standardization
#   - verifies that election totals match those collected by Dave Leip's Atlas
#   - matches each minor civil division to its census FIPS code

################################################################################
# combine election files
all.files <- map_df(list.files("processed-data/annual", full.names = T, pattern = "[0-9].csv"),
                    read_csv)


################################################################################
# standardize fields
clean.results <- all.files |>
  mutate(across(where(is.character), str_to_upper),
         district = replace(district, ! office %in% c("CONGRESS", "STATE ASSEMBLY", "STATE SENATE"), 0)) |>
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
  group_by(county, municipality, ctv, year, reporting_unit, office, district, party) |>
  summarise(count = n()) |>
  filter(count > 1)
nrow(multiples) == 0

###############################################################################
# compare each race's total votes, dem votes, and rep votes with the Leip Atlas
leip.totals <- read_csv("processed-data/leip-wi-totals.csv") |>
  mutate(office = str_to_upper(office))

compare.with.leip <- full_join(
  clean.results |>
    filter(! office %in% c("STATE ASSEMBLY", "STATE SENATE")) |>
    group_by(year, office, district, party) |>
    summarise(votes = sum(votes)) |>
    group_by(year, office, district) |>
    mutate(total_vote = sum(votes)) |>
    pivot_wider(names_from = party, values_from = votes) |>
    select(year, office, district, total_vote, democratic = DEMOCRATIC, republican = REPUBLICAN) |>
    pivot_longer(cols = -c(year, office, district), values_to = "my_total"),
  leip.totals |>
    pivot_longer(cols = -c(year, office, district), values_to = "leip_total")
) |>
  mutate(my_total = if_else(is.na(my_total), 0, my_total),
         match = my_total == leip_total,
         diff = my_total - leip_total)

mismatch <- compare.with.leip |> filter(match == FALSE | is.na(match))
mismatch |> group_by(year, office, district) |> summarise(count = n())

# 2016 CD 3 and 2018 CD 2 both have a few Republican votes in Leip because he
#   classified a write-in "Republican" as the Republican candidate. In our data
#   we leave these candidates as true write-ins, so there is no real difference here.

###############################################################################
# verify that state assembly and senate totals are sensible
by.state.legis <- clean.results |>
  filter(str_detect(office, "STATE")) |>
  group_by(office, year, district) |>
  summarise(total_votes = sum(votes))
###############################################################################
# Add minor civil division FIPS code
mcd.codes <- sf::st_read("processed-data/mcd-boundaries-2000_2009-2022.geojson") |>
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
# The only unjoined MCD is Mukwonago V in Racine County
#   This ward only exists in the 2004 election results
#   It has 0 votes in any race
#   I can find no evidence of V. Mukwonago ever having land in Racine County
#   This record may be in error, and, in any event, does not effect election totals at all
#   I remove it from the records.

clean.results.with.fips.valid <- clean.results.with.fips |>
  filter(!is.na(mcd_fips)) |>
  mutate(county_fips = str_sub(mcd_fips, 1, 5)) %>%
  select(mcd_fips, county_fips, county, municipality, ctv, reporting_unit, year, office,
         district, con_dist, wss_dist, wsa_dist, party, candidate, votes) |>
  # add total column
  group_by(mcd_fips, county, municipality, ctv, reporting_unit, year, office,
           district) |>
  mutate(total_votes = sum(votes)) |>
  ungroup() |>
  mutate(office = if_else(office == "SENATE", "US SENATE", office))

# check that senate districts match assembly districts appropriately
#   The two instances where they don't have 0 votes, so I won't worry about it
clean.results.with.fips.valid |>
  filter(!is.na(wss_dist)) |>
  anti_join(tibble(wsa_dist = 1:99,
                   wss_dist = rep(1:33, each = 3))) |>
  group_by(county, municipality, ctv, reporting_unit, year, con_dist, wss_dist, wsa_dist) |>
  summarise()

###############################################################################
# Save results by reporting unit, minor civil division, and municipality
#   by reporting unit
write_csv(clean.results.with.fips.valid, "processed-data/AllElections_ReportingUnit.csv")

#   by minor civil division
mcd.totals <- clean.results.with.fips.valid |>
  group_by(county, county_fips, mcd_fips, municipality, ctv, year, office, district, party, candidate) |>
  summarise(votes = sum(votes)) |>
  group_by(mcd_fips, municipality, ctv, year, office, district) |>
  mutate(total_votes = sum(votes))
write_csv(mcd.totals, "processed-data/AllElections_MinorCivilDivisions.csv")

# by municipality
municipality.totals <- clean.results.with.fips.valid |>
  mutate(muni_fips = paste0("55", str_sub(mcd_fips, -5, -1))) |>
  group_by(muni_fips, municipality, ctv, year, office, district, party, candidate) |>
  summarise(votes = sum(votes)) |>
  group_by(muni_fips, municipality, ctv, year, office, district) |>
  mutate(total_votes = sum(votes))
write_csv(municipality.totals, "processed-data/AllElections_Municipalities.csv")

