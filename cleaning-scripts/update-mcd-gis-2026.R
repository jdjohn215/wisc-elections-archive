rm(list = ls())

library(tidyverse)
library(sf)

old.mcd.boundaries <- st_read("processed-data/mcd-boundaries-2000_2009-2025.geojson")

mcd.2026 <- st_read("~/Dropbox/Projects/SHPfiles/WI_Cities_Towns_Villages_July_2026.geojson") |>
  mutate(MCD_FIPS = if_else(MCD_NAME == "French Island", "5506312300", MCD_FIPS)) |>
  separate(MCD_FIPS, into = c("state_fips","county_fips","cousub_fips"),
           sep = c(2,-5)) |>
  select(municipality_name = MCD_NAME, ctv = CTV, state_fips, county_fips, cousub_fips) |>
  mutate(ctv = case_when(
    ctv == "C" ~ "city",
    ctv == "V" ~ "village",
    ctv == "T" ~ "town"
  ),
  year = 2026) |>
  st_transform(crs = st_crs(old.mcd.boundaries)) |>
  # munge village of greenleaf
  mutate(ctv = if_else(county_fips == "009" & cousub_fips == "31375", "village", ctv),
         municipality_name = if_else(county_fips == "009" & cousub_fips == "31375", "GREENLEAF", municipality_name),
         ctv = if_else(municipality_name == "Rib Mountain", "village", ctv))

county.codes <- tidycensus::fips_codes |>
  filter(state_code == 55) |>
  group_by(county_fips = county_code, county_name = county) |>
  summarise() |>
  ungroup()

new.years <- mcd.2026 |>
  mutate(mcd_fips = paste0(state_fips, county_fips, cousub_fips),
         ctv = str_sub(ctv, 1, 1),
         across(where(is.character), str_to_upper)) |>
  left_join(county.codes) |>
  select(mcd_fips, year, county_name, municipality_name, ctv) |>
  filter(municipality_name != "COUNTY SUBDIVISIONS NOT DEFINED") |>
  # munges to match election data
  mutate(county_name = str_remove(county_name, " County$")) |>
  rename(municipality = municipality_name,
         county = county_name) |>
  mutate(across(where(is.character), str_to_upper),
         municipality = str_remove_all(municipality, "[.]"),
         municipality = case_when(
           municipality == "GRANDVIEW" ~ "GRAND VIEW",
           municipality == "MT STERLING" ~ "MOUNT STERLING",
           municipality == "POYSIPPI" ~ "POY SIPPI",
           TRUE ~ municipality
         ))

all.years <- bind_rows(old.mcd.boundaries, new.years)

all.wec <- open_dataset("~/dropbox/projects/2023/december/wisc-elections-archive/processed-data/parquet/") |>
  select(mcd_fips, year) |>
  collect() |>
  distinct() |>
  mutate(mcd_fips = as.character(mcd_fips))
all.wec |> filter(year %in% c(2000, 2009:2026)) |> anti_join(all.years)

st_write(all.years, "processed-data/mcd-boundaries-2000_2009-2026.geojson",
         delete_dsn = T)
