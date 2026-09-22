rm(list = ls())

library(tidyverse)
library(sf)

mcd2000 <- st_read("~/Dropbox/Projects/SHPfiles/wi-county-subdivisions-2000-tigris.geojson") |>
  mutate(mcd_fips = paste0(STATEFP00, COUNTYFP00, COUSUBFP00),
         year = 2000,
         source = "nhgis") |>
  select(mcd_fips, year, source) |>
  st_transform(crs = 4326)
mcd2009 <- st_read("~/Dropbox/Projects/SHPfiles/wi-county-subdivisions-2009-tigris.geojson") |>
  mutate(mcd_fips = paste0(STATEFP, COUNTYFP, COUSUBFP),
         year = 2009,
         source = "nhgis") |>
  select(mcd_fips, year, source) |>
  st_transform(crs = 4326)
mcd2010 <- tigris::county_subdivisions("WI", cb = T, year = 2010) |>
  mutate(mcd_fips = paste0(STATE, COUNTY, COUSUB),
         year = 2010,
         source = "tigris") |>
  select(mcd_fips, year, source) |>
  st_transform(crs = 4326)
mcd2011 <- tigris::county_subdivisions("WI", cb = F, year = 2011) |>
  mutate(year = 2011,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2012 <- tigris::county_subdivisions("WI", cb = F, year = 2012) |>
  mutate(year = 2012,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2013 <- tigris::county_subdivisions("WI", cb = F, year = 2013) |>
  mutate(year = 2013,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2014 <- tigris::county_subdivisions("WI", cb = T, year = 2014) |>
  mutate(year = 2014,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2015 <- tigris::county_subdivisions("WI", cb = T, year = 2015) |>
  mutate(year = 2015,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2016 <- tigris::county_subdivisions("WI", cb = T, year = 2016) |>
  mutate(year = 2016,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2017 <- tigris::county_subdivisions("WI", cb = T, year = 2017) |>
  mutate(year = 2017,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2018 <- tigris::county_subdivisions("WI", cb = T, year = 2018) |>
  mutate(year = 2018,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2019 <- tigris::county_subdivisions("WI", cb = T, year = 2019) |>
  mutate(year = 2019,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2020 <- tigris::county_subdivisions("WI", cb = T, year = 2020) |>
  mutate(year = 2020,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2021 <- tigris::county_subdivisions("WI", cb = T, year = 2021) |>
  mutate(year = 2021,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2022 <- tigris::county_subdivisions("WI", cb = T, year = 2022) |>
  mutate(year = 2022,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2023 <- tigris::county_subdivisions("WI", cb = T, year = 2023) |>
  mutate(year = 2023,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2024 <- tigris::county_subdivisions("WI", cb = T, year = 2024) |>
  mutate(year = 2024,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2025 <- tigris::county_subdivisions("WI", cb = T, year = 2025) |>
  mutate(year = 2025,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source) |>
  st_transform(crs = 4326)
mcd2026 <- st_read("~/Dropbox/Projects/SHPfiles/WI_Cities_Towns_Villages_July_2026.geojson") |>
  mutate(GEOID = if_else(MCD_NAME == "French Island", "5506312300", MCD_FIPS)) |>
  mutate(year = 2026,
         source = "tigris") |>
  select(mcd_fips = GEOID, year, source)

################################################################################
all.mcd <- bind_rows(mcd2000, mcd2009, mcd2010, mcd2011, mcd2012, mcd2013,
                     mcd2014, mcd2015, mcd2016, mcd2017, mcd2018, mcd2019,
                     mcd2020, mcd2021, mcd2022, mcd2023, mcd2024, mcd2025,
                     mcd2026) |>
  rmapshaper::ms_simplify(keep_shapes = TRUE)
