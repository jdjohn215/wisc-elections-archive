# prep-boundaries.R — copy per-election FGB boundaries into the website,
# build the single static county layer, validate that every results join key
# finds a boundary feature, and write geo-manifest.json.

rm(list = ls())
library(tidyverse)
library(sf)
library(arrow)
library(jsonlite)

geo.dir <- "website/app/data/geo"
for (d in c("mcd", "muni", "ru", "con", "wsa", "wss")) {
  dir.create(file.path(geo.dir, d), recursive = TRUE, showWarnings = FALSE)
}

# --- copy election FGBs (reprojecting to WGS84 for MapLibre) ------------------

# the reporting-unit FGBs are in Wisconsin Transverse Mercator; web maps need
# EPSG:4326, so read/transform/write instead of copying
write.web.fgb <- function(src, dest.dir) {
  g <- st_read(src, quiet = TRUE)
  if (is.na(st_crs(g)) || st_crs(g) != st_crs(4326)) g <- st_transform(g, 4326)
  st_write(g, file.path(dest.dir, basename(src)), delete_dsn = TRUE, quiet = TRUE)
}

for (lvl in c("mcd", "muni")) {
  files <- list.files(file.path("fgb", lvl), pattern = "\\.fgb$", full.names = TRUE)
  for (f in files) write.web.fgb(f, file.path(geo.dir, lvl))
}
files.ru <- list.files("fgb/reporting-units", pattern = "\\.fgb$", full.names = TRUE)
for (f in files.ru) write.web.fgb(f, file.path(geo.dir, "ru"))

# --- district layers (congressional / state senate / state assembly) ---------

# District maps are vintage files shared across elections; the filename gives
# the years each map was in force ("2002-2010.fgb" covers the 2002..2010
# elections). They are offered only for August/November elections — the only
# ones whose results carry con_dist/wss_dist/wsa_dist.
dist.geos <- c("con", "wss", "wsa")

for (lvl in dist.geos) {
  unlink(list.files(file.path(geo.dir, lvl), pattern = "\\.fgb$", full.names = TRUE))
  files <- list.files(file.path("fgb", lvl), pattern = "\\.fgb$", full.names = TRUE)
  for (f in files) write.web.fgb(f, file.path(geo.dir, lvl))
}

# map each election year to its vintage file per district geography
dist.files <- bind_rows(lapply(dist.geos, function(lvl) {
  stems <- sub("\\.fgb$", "", list.files(file.path(geo.dir, lvl), pattern = "\\.fgb$"))
  rng <- str_match(stems, "^(\\d{4})(?:-(\\d{4}))?$")
  stopifnot(!any(is.na(rng[, 2])))
  tibble(lvl, stem = stems, start = as.integer(rng[, 2]),
         end = as.integer(coalesce(as.integer(rng[, 3]), as.integer(rng[, 2]))))
}))

vintage.stem <- function(l, yr) {
  hit <- dist.files |> filter(.data$lvl == l, .data$start <= yr, yr <= .data$end)
  if (nrow(hit) != 1) return(NA_character_)
  hit$stem
}

# named list geo -> vintage stem for August/November elections, else NULL
district.vintages <- function(yr, mo) {
  if (!mo %in% c("AUGUST", "NOVEMBER")) return(NULL)
  v <- vapply(dist.geos, vintage.stem, character(1), yr = yr)
  if (any(is.na(v))) stop("no district vintage for ", yr, " ", mo)
  as.list(v)
}

# --- county layer (static; dissolve recent MCDs) ------------------------------

# fgb writers are inconsistent about fips type across layers
fips10 <- function(x) sprintf("%.0f", as.numeric(x))

mcd.ref <- st_read("fgb/mcd/2024-NOVEMBER.fgb", quiet = TRUE)
county.names <- open_dataset("processed-data/parquet") |>
  distinct(county_fips, county) |>
  collect() |>
  mutate(county_fips = sprintf("%05d", as.integer(county_fips)))

counties <- mcd.ref |>
  st_make_valid() |>
  mutate(county_fips = substr(fips10(mcd_fips), 1, 5)) |>
  group_by(county_fips) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  left_join(county.names, by = "county_fips") |>
  select(county_fips, county)

stopifnot(nrow(counties) == 72, !any(is.na(counties$county)))
st_write(counties, file.path(geo.dir, "counties.fgb"), delete_dsn = TRUE)

# --- validate join-key coverage ----------------------------------------------

wec <- open_dataset("processed-data/parquet") |> collect() |>
  mutate(
    county_fips = sprintf("%05d", as.integer(county_fips)),
    mcd_fips = sprintf("%.0f", mcd_fips)
  )

elections <- wec |> distinct(year, month) |> arrange(year, month) |>
  mutate(key = paste0(year, "-", month))

check.coverage <- function(elec.key, lvl) {
  path <- file.path(geo.dir, lvl, paste0(elec.key, ".fgb"))
  if (!file.exists(path)) return(NA_integer_)
  yr <- as.integer(sub("-.*", "", elec.key)); mo <- sub(".*-", "", elec.key)
  res <- wec |> filter(year == yr, month == mo)
  bnd <- st_read(path, quiet = TRUE) |> st_drop_geometry()
  if (lvl == "mcd") {
    missing <- setdiff(unique(res$mcd_fips), fips10(bnd$mcd_fips))
  } else if (lvl == "muni") {
    missing <- setdiff(unique(res$muni_fips), bnd$muni_fips)
  } else { # ru
    res.keys <- res |> distinct(mcd_fips, reporting_unit) |>
      mutate(k = paste(mcd_fips, toupper(reporting_unit)))
    bnd.keys <- bnd |> mutate(k = paste(fips10(mcd_fips), toupper(reporting_unit)))
    missing <- setdiff(res.keys$k, bnd.keys$k)
  }
  length(missing)
}

# district coverage: every district number in the results must have a polygon
# in the election's vintage file
check.dist.coverage <- function(yr, mo, lvl, stem) {
  res <- wec |> filter(year == yr, month == mo)
  col <- paste0(lvl, "_dist")
  vals <- unique(res[[col]])
  vals <- vals[!is.na(vals) & vals > 0]
  bnd <- st_read(file.path(geo.dir, lvl, paste0(stem, ".fgb")), quiet = TRUE) |>
    st_drop_geometry()
  length(setdiff(vals, bnd[[col]]))
}

manifest <- elections |>
  rowwise() |>
  mutate(
    districts = list(district.vintages(year, month)),
    geographies = list(c(
      "county",
      if (file.exists(file.path(geo.dir, "mcd", paste0(key, ".fgb")))) "mcd",
      if (file.exists(file.path(geo.dir, "muni", paste0(key, ".fgb")))) "muni",
      if (file.exists(file.path(geo.dir, "ru", paste0(key, ".fgb")))) "ru",
      if (!is.null(districts)) names(districts)
    )),
    missing.mcd = check.coverage(key, "mcd"),
    missing.muni = check.coverage(key, "muni"),
    missing.ru = check.coverage(key, "ru"),
    missing.dist = if (is.null(districts)) NA_integer_ else sum(vapply(
      names(districts),
      function(lvl) check.dist.coverage(year, month, lvl, districts[[lvl]]),
      integer(1)
    ))
  ) |>
  ungroup()

stopifnot(all(manifest$missing.dist == 0 | is.na(manifest$missing.dist)))

manifest |>
  select(key, missing.mcd, missing.muni, missing.ru, missing.dist) |>
  print(n = 50)

geo.manifest <- manifest |>
  transmute(key, year, month, geographies, districts) |>
  arrange(desc(year))
write_json(geo.manifest, file.path(geo.dir, "geo-manifest.json"),
           auto_unbox = TRUE, pretty = TRUE)
message("Wrote counties.fgb, copied FGBs, wrote geo-manifest.json")
