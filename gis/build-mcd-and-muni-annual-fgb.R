rm(list = ls())

library(tidyverse)
library(arrow)
library(sf)
library(leaflet)

# all of the original MCD polygons, by year, simplified
all.mcd <- read_rds("gis/all-mcd.rds")

################################################################################
all.wec <- open_dataset("~/dropbox/projects/2023/december/wisc-elections-archive/processed-data/parquet/") |>
  select(mcd_fips, year, month) |>
  collect() |>
  distinct() |>
  mutate(mcd_fips = as.character(mcd_fips))

# all MCD fips are present in the full set
all.wec |> distinct(mcd_fips) |> anti_join(all.mcd)


create_muni_polygons <- function(electionindex){
  election <- elections[electionindex,]
  mcd.1 <- all.wec |>
    inner_join(election) |>
    select(mcd_fips, election_year = year, election_month = month) |>
    inner_join(all.mcd) |>
    mutate(timediff = abs(year - election_year)) |>
    group_by(mcd_fips) |>
    slice_min(order_by = timediff, n = 1, with_ties = F) |>
    ungroup()
  
  # remove any overlaps
  mcd.2 <- mcd.1 |>
    st_sf() |>
    st_make_valid() |>
    # arrange smallest-to-largest, so that polygons inside larger polygons are kept
    mutate(area = st_area(geometry)) |>
    arrange(area) |>
    st_difference() |>
    arrange(mcd_fips)
  stopifnot(nrow(mcd.1) == nrow(mcd.2))
  
  # make sure only polygon geometry
  mcd.3 <- mcd.2 |> st_collection_extract("POLYGON")
  if(nrow(mcd.3) > nrow(mcd.2)){
    mcd.3 <- mcd.3 |>
      group_by(mcd_fips, area) |>
      summarise(geometry = st_combine(geometry), .groups = "drop")
  }
  stopifnot(nrow(mcd.3) == nrow(mcd.2))
  
  mcd.3 |>
    select(mcd_fips) |>
    rmapshaper::ms_simplify(keep_shapes = T) |>
    st_write(paste0("fgb/mcd/", election$year, "-", election$month, ".fgb"),
             delete_dsn = T)
  
  muni1 <- mcd.3 |>
    mutate(muni_fips = paste0("55", str_sub(mcd_fips, -5, -1))) |>
    group_by(muni_fips) |>
    summarise(geometry = st_combine(geometry)) |>
    rmapshaper::ms_simplify(keep_shapes = T) |>
    st_write(paste0("fgb/muni/", election$year, "-", election$month, ".fgb"),
             delete_dsn = T)
}

elections <- all.wec |> distinct(year, month)

walk(1:nrow(elections), create_muni_polygons)
