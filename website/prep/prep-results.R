# prep-results.R — write per-election parquet files and contests.json for the
# static website (website/app/). Source: processed-data/parquet/.

rm(list = ls())
library(tidyverse)
library(arrow)
library(jsonlite)

out.dir <- "website/app/data/results"
dir.create(out.dir, recursive = TRUE, showWarnings = FALSE)

wec <- open_dataset("processed-data/parquet") |>
  collect() |>
  # zero-pad fips to match boundary join keys
  mutate(
    county_fips = sprintf("%05d", as.integer(county_fips)),
    mcd_fips = sprintf("%.0f", mcd_fips)
  )

stopifnot(!any(is.na(wec$county_fips)), !any(is.na(wec$mcd_fips)),
          !any(is.na(wec$muni_fips)), !any(is.na(wec$district)))

# --- per-election parquet ---------------------------------------------------

elections <- wec |> distinct(year, month) |> arrange(year, month)

for (i in seq_len(nrow(elections))) {
  yr <- elections$year[i]; mo <- elections$month[i]
  wec |>
    filter(year == yr, month == mo) |>
    write_parquet(file.path(out.dir, sprintf("%d-%s.parquet", yr, mo)))
}

# --- contests.json ------------------------------------------------------------

month.type <- c(
  FEBRUARY = "Spring Primary",
  APRIL    = "Spring Election",
  JUNE     = "Recall General",
  AUGUST   = "Partisan Primary",
  NOVEMBER = "General Election"
)
month.order <- c(FEBRUARY = 2, APRIL = 4, JUNE = 6, AUGUST = 8, NOVEMBER = 11)

office.order <- c(
  "PRESIDENT", "US SENATE", "GOVERNOR", "LIEUTENANT GOVERNOR",
  "ATTORNEY GENERAL", "SECRETARY OF STATE", "STATE TREASURER",
  "CONGRESS", "STATE SENATE", "STATE ASSEMBLY",
  "SUPREME COURT", "STATE SUPERINTENDENT"
)

# one row per contest, per website/rules/DataAggregation.md identity rules
contests <- wec |>
  filter(party != "SCATTERING") |>
  distinct(year, month, office, party, district) |>
  mutate(
    # party defines the contest in partisan primaries only
    needs.party = month == "AUGUST" | (month == "APRIL" & office == "PRESIDENT")
  )

contest.list <- contests |>
  mutate(party = if_else(needs.party, party, NA_character_)) |>
  distinct(year, month, office, party, district) |>
  arrange(year, month, office, party, district)

election.json <- contest.list |>
  group_by(year, month) |>
  summarise(
    offices = list(sort(unique(office))),
    contests = list(tibble(office, party, district)),
    .groups = "drop"
  ) |>
  mutate(
    type = if_else(
      month == "APRIL" & map_lgl(offices, ~ "PRESIDENT" %in% .x),
      "Spring Election & Presidential Preference",
      unname(month.type[month])
    ),
    label = paste0(str_to_title(str_to_lower(month)), " ", year, " \u2014 ", type),
    sort.key = year + month.order[month] / 100
  ) |>
  arrange(desc(sort.key)) |>
  # order offices within each election; I() keeps single-office elections as
  # JSON arrays (auto_unbox would otherwise write a bare string, and the app
  # iterates offices, exploding a string into single characters)
  mutate(offices = map(offices, ~ I(.x[order(match(.x, office.order))]))) |>
  select(year, month, label, offices, contests)

write_json(election.json, "website/app/data/contests.json",
           auto_unbox = TRUE, pretty = TRUE, digits = NA)

message("Wrote ", nrow(elections), " parquet files and contests.json (",
        nrow(contest.list), " contests across ", nrow(election.json), " elections)")
