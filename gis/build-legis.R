rm(list = ls())

library(tidyverse)
library(sf)

wsa.2000 <- st_read("/Users/johnsonjoh/Downloads/nhgis0336_shape/nhgis0336_shapefile_tl2010_us_stleg_lo_2000/US_stleg_lo_2000_tl10.shp") |>
  filter(STATEFP00 == "55") |>
  select(wsa_dist = SLDLST00) |>
  mutate(wsa_dist = as.numeric(wsa_dist))
glimpse(wsa.2000)

wss.2000 <- wsa.2000 |>
  inner_join(tibble(wsa_dist = 1:99,
                    wss_dist = rep(1:33, each = 3))
  ) |>
  group_by(wss_dist) |>
  summarise(geometry = st_union(geometry))

wsa.2002 <- st_read("/Users/johnsonjoh/Downloads/nhgis0336_shape/nhgis0336_shapefile_tl2009_us_stleg_lo_2009/US_stleg_lo_2009.shp") |>
  filter(STATEFP == "55") |>
  select(wsa_dist = SLDLST) |>
  mutate(wsa_dist = as.numeric(wsa_dist))
glimpse(wsa.2002)

wss.2002 <- wsa.2002 |>
  inner_join(tibble(wsa_dist = 1:99,
                    wss_dist = rep(1:33, each = 3))
  ) |>
  group_by(wss_dist) |>
  summarise(geometry = st_union(geometry))


wsa.2012 <- tigris::state_legislative_districts("WI", "lower", cb = F, year = 2012) |>
  select(wsa_dist = SLDLST) |>
  mutate(wsa_dist = as.numeric(wsa_dist))
wss.2012 <- tigris::state_legislative_districts("WI", "upper", cb = F, year = 2012) |>
  select(wss_dist = SLDUST) |>
  mutate(wss_dist = as.numeric(wss_dist))
wsa.2022 <- tigris::state_legislative_districts("WI", "lower", cb = T, year = 2022) |>
  select(wsa_dist = SLDLST) |>
  filter(wsa_dist != "ZZZ") |>
  mutate(wsa_dist = as.numeric(wsa_dist))
wss.2022 <- tigris::state_legislative_districts("WI", "upper", cb = T, year = 2022) |>
  select(wss_dist = SLDUST) |>
  filter(wss_dist != "ZZZ") |>
  mutate(wss_dist = as.numeric(wss_dist))
wsa.2024 <- tigris::state_legislative_districts("WI", "lower", cb = T, year = 2024) |>
  select(wsa_dist = SLDLST) |>
  filter(wsa_dist != "ZZZ") |>
  mutate(wsa_dist = as.numeric(wsa_dist))
wss.2024 <- tigris::state_legislative_districts("WI", "upper", cb = T, year = 2024) |>
  select(wss_dist = SLDUST) |>
  filter(wss_dist != "ZZZ") |>
  mutate(wss_dist = as.numeric(wss_dist))

st_write(wsa.2000, "fgb/wsa/2000.fgb", delete_dsn = T)
st_write(wss.2000, "fgb/wss/2000.fgb", delete_dsn = T)

st_write(wsa.2002, "fgb/wsa/2002-2010.fgb", delete_dsn = T)
st_write(wss.2002, "fgb/wss/2002-2010.fgb", delete_dsn = T)

st_write(wsa.2012, "fgb/wsa/2012-2020.fgb", delete_dsn = T)
st_write(wss.2012, "fgb/wss/2012-2020.fgb", delete_dsn = T)

st_write(wsa.2022, "fgb/wsa/2022.fgb", delete_dsn = T)
st_write(wss.2022, "fgb/wss/2022.fgb", delete_dsn = T)

st_write(wsa.2024, "fgb/wsa/2024-2030.fgb", delete_dsn = T)
st_write(wss.2024, "fgb/wss/2024-2030.fgb", delete_dsn = T)

################################################################################
con.2000 <- st_read("/Users/johnsonjoh/Downloads/nhgis0338_shape/nhgis0338_shapefile_tl2000_us_cd106th_2000/US_cd106th_2000.shp") |>
  filter(STATE == "55") |>
  select(con_dist = DIST) |>
  mutate(con_dist = as.numeric(con_dist))
con.2002 <- st_read("/Users/johnsonjoh/Downloads/nhgis0337_shape/nhgis0337_shapefile_tl2010_us_cd108th_2000/US_cd108th_2000_tl10.shp") |>
  filter(STATEFP00 == "55") |>
  select(con_dist = CD108FP) |>
  mutate(con_dist = as.numeric(con_dist))
con.2012 <- st_read("/Users/johnsonjoh/Downloads/nhgis0337_shape/nhgis0337_shapefile_tl2014_us_cd114th_2014/US_cd114th_2014.shp") |>
  filter(STATEFP == "55") |>
  select(con_dist = CD114FP) |>
  mutate(con_dist = as.numeric(con_dist))
con.2022 <- st_read("/Users/johnsonjoh/Downloads/nhgis0337_shape/nhgis0337_shapefile_tl2024_us_cd119th_2024/US_cd119th_2024.shp") |>
  filter(STATEFP == "55") |>
  select(con_dist = CD119FP) |>
  mutate(con_dist = as.numeric(con_dist))

st_write(con.2000, "fgb/con/2000.fgb", delete_dsn = T)
st_write(con.2002, "fgb/con/2002-2010.fgb", delete_dsn = T)
st_write(con.2012, "fgb/con/2012-2020.fgb", delete_dsn = T)
st_write(con.2022, "fgb/con/2022-2030.fgb", delete_dsn = T)
