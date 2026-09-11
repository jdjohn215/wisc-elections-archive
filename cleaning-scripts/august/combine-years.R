rm(list = ls())

library(tidyverse)

# This script does the following:
#   - combines each year's election results into a single dataframe
#   - performs municipality and county name standardization
#   - matches each minor civil division to its census FIPS code

################################################################################
# combine election files
all.files <- map_df(list.files("processed-data/august/annual", full.names = T, pattern = "[0-9].csv"),
                    read_csv)


################################################################################
# standardize fields
clean.results <- all.files |>
  mutate(across(where(is.character), str_to_upper),
         district = str_extract(office, "(?<=DISTRICT )\\d+"),
         office = case_when(
           str_detect(office, "SPECIAL") ~ office,
           str_detect(office, "ASSEMBLY") ~ "STATE ASSEMBLY",
           str_detect(office, "STATE SENATOR") ~ "STATE SENATE",
           str_detect(office, "CONGRESS") ~ "CONGRESS",
           str_detect(office, "UNITED STATES SENATOR") ~ "US SENATE",
           TRUE ~ office
         ),
         district = replace(district, ! office %in% c("CONGRESS", "STATE ASSEMBLY", "STATE SENATE"), 0)) |>
  separate(reporting_unit, into = c("municipality", "reporting_unit"), 
           sep = " (?=WARD)| (?=WD)", extra = "merge") |>
  mutate(reporting_unit = if_else(is.na(reporting_unit), "WARD 1", reporting_unit),
         across(where(is.character), str_to_upper),
         ctv = case_when(
           word(municipality, 1) %in% c("CITY", "TOWN", "VILLAGE") ~ str_sub(municipality, 1, 1),
           str_detect(municipality, "\\bTOWN\\b") ~ "T",
           str_detect(municipality, "\\bVILLAGE\\b") ~ "V",
           str_detect(municipality, "\\bCITY\\b") ~ "C"
         ),
         # munge CTV
         ctv = case_when(
           municipality == "CITY POINT TOWN" ~ "T",
           TRUE ~ ctv
         ),
         municipality = str_remove(municipality, "VILLAGE OF |CITY OF |TOWN OF |"),
         municipality = case_when(
           municipality %in% c("CUBA CITY", "FOUNTAIN CITY", "CITY POINT",
                               "GENOA CITY", "MARATHON CITY", "BAY CITY",
                               "PLUM CITY", "JUNCTION CITY", "SAUK CITY",
                               "GLENWOOD CITY") ~ municipality,
           TRUE ~ str_remove(municipality, " CITY$| VILLAGE$| TOWN$")
         )) |>
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
         ),
         district = as.numeric(district))

# confirm that fields uniquely identify rows
multiples <- clean.results |>
  group_by(county, municipality, ctv, year, reporting_unit, office, district, party, candidate) |>
  summarise(count = n()) |>
  filter(count > 1)
nrow(multiples) == 0

###############################################################################
# add legis district codes to all rows
rep.unit.dist.assignments <- clean.results |>
  filter(office == "STATE ASSEMBLY") |>
  distinct(year, county, ctv, municipality, reporting_unit, wsa_dist = district) |>
  left_join(tibble(wsa_dist = 1:99, wss_dist = rep(1:33, each = 3))) |>
  left_join(clean.results |>
              filter(office == "CONGRESS") |>
              distinct(year, county, ctv, municipality, reporting_unit, con_dist = district)) |>
  # for some reason a 0-vote reporting unit lacks a congressional district assingment, so I do it manually
  mutate(con_dist = if_else(year == 2016 & county == "DANE" & ctv == "V" & municipality == "BROOKLYN" & reporting_unit == "WARD 3",
                            2, con_dist))

# verify that every reporting unit is uniquely assigned to a single district
nrow(rep.unit.dist.assignments) == nrow(distinct(rep.unit.dist.assignments, year, county, ctv, municipality, reporting_unit))

clean.results.with.districts <- clean.results |> inner_join(rep.unit.dist.assignments)
sum(clean.results.with.districts$votes) == sum(clean.results$votes)

###############################################################################
# verify that state assembly and senate totals are sensible
by.state.legis <- clean.results.with.districts |>
  filter(str_detect(office, "STATE")) |>
  group_by(office, year, district) |>
  summarise(total_votes = sum(votes))
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
clean.results.with.fips <- clean.results.with.districts |>
  left_join(mcd.codes) |>
  # add recently incorporated French Island Village, which was previously the Town of Campbell
  #   this is subject to ongoing litigation, so it may not stand
  mutate(mcd_fips = if_else(county == "LA CROSSE" & municipality == "FRENCH ISLAND",
                            "5506312300", mcd_fips))

clean.results.with.fips |> filter(is.na(mcd_fips)) |>
  distinct(county, municipality, ctv, reporting_unit, year)

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
# confirm that all votes are preserved
sum(clean.results.with.fips.valid$votes) == sum(all.files$votes)

###############################################################################
# Save results by reporting unit, minor civil division, municipality, and county
#   by reporting unit
write_csv(clean.results.with.fips.valid, "processed-data/august/AllElections_ReportingUnit.csv")
