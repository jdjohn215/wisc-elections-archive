rm(list = ls())

library(tidyverse)
library(sf)

wi.cb <- tigris::states(cb = T) |> filter(GEOID == "55") |> select(STATEFP)

############################################################################
# 2000 and 2009 are downloaded from IPUMS NHGIS
mcd.2000 <- st_read("/Users/johnsonjoh/Downloads/nhgis0310_shape/nhgis0310_shapefile_tl2010_us_cty_sub_2000/US_cty_sub_2000_tl10.shp") |>
  filter(STATEFP00 == "55") |>
  mutate(LSAD = word(NAMELSAD00, -1)) |>
  select(municipality_name = NAME00, ctv = LSAD, state_fips = STATEFP00,
         county_fips = COUNTYFP00, cousub_fips = COUSUBFP00) |>
  mutate(year = 2000) %>%
  rmapshaper::ms_simplify(keep_shapes = T) %>%
  st_transform(crs = 4269)

mcd.2009 <- st_read("/Users/johnsonjoh/Downloads/nhgis0310_shape/nhgis0310_shapefile_tl2009_us_cty_sub_2009/US_cty_sub_2009.shp") |>
  filter(STATEFP == "55") |>
  mutate(LSAD = word(NAMELSAD, -1)) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP,
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2009) %>%
  rmapshaper::ms_simplify(keep_shapes = T) %>%
  st_transform(crs = 4269)

############################################################################
# 2010-2022 are fetched from the Census API via the {{tigris}} package
mcd.2010 <- tigris::county_subdivisions("WI", cb = TRUE, year = 2010) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUB) |>
  mutate(year = 2010)
mcd.2011 <- tigris::county_subdivisions("WI", year = 2011) |> # cb = TRUE not available
  mutate(LSAD = word(NAMELSAD, -1)) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2011) %>%
  st_intersection(wi.cb) |> # remove water areas
  st_collection_extract("POLYGON") |>
  rmapshaper::ms_simplify(keep_shapes = T)
mcd.2012 <- tigris::county_subdivisions("WI", year = 2012) |>
  mutate(LSAD = word(NAMELSAD, -1)) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2012) %>%
  st_intersection(wi.cb) |> # remove water areas
  st_collection_extract("POLYGON") |>
  rmapshaper::ms_simplify(keep_shapes = T)
mcd.2013 <- tigris::county_subdivisions("WI", year = 2013, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2013)
mcd.2014 <- tigris::county_subdivisions("WI", year = 2014, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2014)
mcd.2015 <- tigris::county_subdivisions("WI", year = 2015, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2015)
mcd.2016 <- tigris::county_subdivisions("WI", year = 2016, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2016)
mcd.2017 <- tigris::county_subdivisions("WI", year = 2017, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2017)
mcd.2018 <- tigris::county_subdivisions("WI", year = 2018, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2018)
mcd.2019 <- tigris::county_subdivisions("WI", year = 2019, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2019)
mcd.2020 <- tigris::county_subdivisions("WI", year = 2020, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2020)
mcd.2021 <- tigris::county_subdivisions("WI", year = 2021, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2021)
mcd.2022 <- tigris::county_subdivisions("WI", year = 2022, cb = T) |>
  mutate(LSAD = case_when(
    LSAD == "47" ~ "village",
    LSAD == "43" ~ "town",
    LSAD == "25" ~ "city"
  )) |>
  select(municipality_name = NAME, ctv = LSAD, state_fips = STATEFP, 
         county_fips = COUNTYFP, cousub_fips = COUSUBFP) |>
  mutate(year = 2022)

############################################################################
# combine the files
county.codes <- tidycensus::fips_codes |>
  filter(state_code == 55) |>
  group_by(county_fips = county_code, county_name = county) |>
  summarise() |>
  ungroup()
all.years <- bind_rows(mcd.2000, mcd.2009, mcd.2010, mcd.2011, mcd.2012,
                       mcd.2013, mcd.2014, mcd.2015, mcd.2016, mcd.2017,
                       mcd.2018, mcd.2019, mcd.2020, mcd.2021, mcd.2022) |>
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
st_write(all.years, "processed-data/mcd-boundaries-2000_2009-2022.geojson",
         delete_dsn = T)


