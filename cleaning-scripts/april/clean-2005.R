rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the april 2005 nonpartisan (spring) general election
orig.path <- "original-data/april/2005-04-05_SpringElection_SupCt_WardbyWard_2005.xls"

# the 2005-2009 GAB exports are wide single-sheet files: one row per
# reporting unit, one column per candidate, with two header rows (row 1 has
# the candidate names and the structural group labels, row 2 the party
# labels). there is no ballots-cast column, so total_votes is computed as
# the sum of the candidate columns per reporting unit. reporting_unit_name
# is blank for single-ward municipalities ("Ward 1").
#
# this file holds the JUSTICE OF THE SUPREME COURT contest only (Bradley
# unopposed). 2005 was also a superintendent race year (Burmaster vs.
# Underheim), but this file does not contain it; a separate source is
# needed if it is ever added.
#
# reporting_unit is built era-natively as "<municipality> <type> <ward
# info>" (e.g. "ADAMS TOWN WARDS 1 & 2"), which differs from the 2011+
# workbooks' "TOWN OF ADAMS WARDS 1 & 2" — beware when joining across eras.

# structural columns; everything after them is a candidate column
structural.names <- c("election_date", "election_type", "election_subtype",
                      "office_type_keyword", "office_name",
                      "us_congress_district", "state_senate_district",
                      "state_assembly_district", "court_of_appeals_district",
                      "county_number", "county_name", "municipality_type",
                      "municipality_number", "municipality_name",
                      "hindi_number", "order_number", "reporting_unit_name")

header.rows <- readxl::read_excel(orig.path, sheet = 1, n_max = 2,
                                  col_names = FALSE, .name_repair = "unique_quiet")
orig.names <- as.character(unlist(header.rows[1, ]))
clean.names <- janitor::make_clean_names(
  paste(orig.names, as.character(unlist(header.rows[2, ])), sep = "_")
)
# candidate display names come from header row 1 ("Ann W. Bradley",
# "Scattering"); row 2 holds the party label, which is ignored — every row
# gets party = "NONPARTISAN"
cand.positions <- which(!(clean.names %in% structural.names))
cand.display <- orig.names[cand.positions]

raw <- readxl::read_excel(orig.path, sheet = 1, skip = 2,
                          col_names = clean.names, col_types = "text")

# the sheet ends with a "TOTAL" row (blank structural columns, certified
# statewide totals in the candidate columns); capture it for the sanity
# check below, then drop it from the ward data
certified.row <- raw |>
  filter(is.na(municipality_name), !is.na(reporting_unit_name))
stopifnot(nrow(certified.row) == 1)
raw <- raw |> filter(!is.na(municipality_name))

results.2005 <- raw |>
  select(county = county_name, municipality_name, municipality_type,
         reporting_unit_name, all_of(clean.names[cand.positions])) |>
  mutate(reporting_unit = str_c(municipality_name, " ", municipality_type, " ",
                                if_else(is.na(reporting_unit_name),
                                        "Ward 1", reporting_unit_name))) |>
  select(-municipality_name, -municipality_type, -reporting_unit_name) |>
  pivot_longer(cols = -c(county, reporting_unit),
               names_to = "col", values_to = "votes") |>
  mutate(candidate = cand.display[match(col, clean.names[cand.positions])],
         votes = as.numeric(votes)) |>
  # no ballots-cast column in this format; total_votes = candidate vote sum
  group_by(county, reporting_unit) |>
  mutate(total_votes = sum(votes)) |>
  ungroup() |>
  mutate(across(where(is.character), str_to_upper),
         year = 2005,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         office = "JUSTICE OF THE SUPREME COURT",
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order
stopifnot(identical(names(results.2005), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2005$votes)), !any(is.na(results.2005$total_votes)))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2005 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# sanity: candidate vote sums must equal the workbook's own certified
# "TOTAL" row exactly
certified <- certified.row |>
  pivot_longer(cols = all_of(clean.names[cand.positions]),
               names_to = "col", values_to = "certified") |>
  mutate(candidate = str_to_upper(cand.display[match(col, clean.names[cand.positions])]),
         certified = as.numeric(certified)) |>
  select(candidate, certified)
totals.cmp <- full_join(certified,
                       results.2005 |> summarise(votes = sum(votes), .by = candidate),
                       by = "candidate")
stopifnot(all(totals.cmp$certified == totals.cmp$votes))
totals.cmp

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2005, "processed-data/april/annual/2005.csv")
