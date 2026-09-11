rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the august 2024 partisan primary
orig.path <- "original-data/august/Ward by Ward Report_August 2024 Partisan Primary_All Contests.xlsx"

# this workbook differs from the 2026 one in three ways:
# - sheets are generically named ("Sheet2"..."Sheet981") instead of by contest,
#   so the contest title is taken from each sheet's header block
# - it is "All Contests", so it also contains county-level district attorney
#   primaries, which are filtered out below to match the 2026 state-contests
#   scope
# - congressional district 8 appears twice per party: a special election for
#   the term ending Jan. 3, 2025 alongside the regular 2025-2027 term; offices
#   are renamed below so the regular term keeps the plain name and the special
#   gets an "(SPECIAL)" suffix
#
# header rows vary slightly between sheets, so anchor on the "Total Votes
# Cast" cell rather than fixed row numbers
read_sheet_long <- function(sheet.no) {
  raw <- readxl::read_excel(orig.path, sheet = sheet.no, col_names = FALSE,
                            .name_repair = "unique_quiet")
  if (ncol(raw) < 3) return(NULL)
  header.start <- which(raw[[3]] == "Total Votes Cast")
  if (length(header.start) == 0) return(NULL)

  contest <- raw[[1]][str_detect(raw[[1]], " - ") & !is.na(raw[[1]])][1]

  cand.names <- as.character(unlist(raw[header.start + 1, -(1:3)]))
  cand.cols <- which(!is.na(cand.names)) + 3
  cand.names <- make.unique(cand.names[!is.na(cand.names)], sep = "__dup")

  rows <- raw[(header.start + 2):nrow(raw), ]
  # ward rows only; %in% (not !=) so rows with a blank first column don't
  # become all-NA rows when used as a logical subscript
  ward.rows <- !is.na(rows[[2]]) &
    !str_detect(rows[[2]], "County Totals") &
    !(rows[[1]] %in% "Office Totals:")
  data <- rows[ward.rows, ]

  # the workbook's own per-candidate totals row, for the sanity check below
  totals.row <- which(rows[[1]] %in% "Office Totals:")
  if (length(totals.row) == 0) return(NULL)

  # unlist() works column-major, so expand the base columns to match:
  # one block of ward rows per candidate
  n.wards <- nrow(data)
  n.cands <- length(cand.names)
  list(
    votes = tibble(
      contest = contest,
      county = rep(data[[1]], times = n.cands),
      reporting_unit = rep(data[[2]], times = n.cands),
      total_votes = rep(as.numeric(data[[3]]), times = n.cands),
      candidate = rep(cand.names, each = n.wards),
      votes = as.numeric(unlist(data[, cand.cols]))
    ),
    totals = tibble(
      contest = contest,
      candidate = cand.names,
      votes = as.numeric(unlist(rows[totals.row, cand.cols]))
    )
  )
}

sheets.2024 <- lapply(2:981, read_sheet_long)

long.2024 <- bind_rows(map(sheets.2024, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2024, "totals"))
vote.sums <- long.2024 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

results.2024 <- long.2024 |>
  # split on the LAST " - " so the district 8 term annotation
  # "(Term Jan. 3, 2025 - Jan. 3, 2027)" stays inside the office half
  extract(contest, into = c("office", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper)) |>
  # drop county-level district attorney contests (this workbook is "all
  # contests"; the 2026 file covered state contests only) — note the joint
  # Menominee/Shawano race is "COUNTIES", hence the (Y|IES) pattern — and
  # drop contests whose only candidate column is SCATTERING, i.e. no one on
  # the ballot
  filter(!str_detect(office, "COUNT(Y|IES) DISTRICT ATTORNEY")) |>
  group_by(office, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  # distinguish the two district 8 primaries
  mutate(
    office = str_replace(
      office,
      "^REPRESENTATIVE IN CONGRESS DISTRICT 8:.*$",
      "REPRESENTATIVE IN CONGRESS DISTRICT 8 (SPECIAL)"
    ),
    office = str_replace(
      office,
      "^REPRESENTATIVE IN CONGRESS DISTRICT 8 \\(TERM.*$",
      "REPRESENTATIVE IN CONGRESS DISTRICT 8"
    )
  ) |>
  mutate(year = 2024,
         month = "AUGUST",
         election_type = "PARTISAN PRIMARY") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order; the
# template's single example row is 2026 data, so the row itself can't be
# matched against a 2024 file
stopifnot(identical(names(results.2024), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2024$votes)), !any(is.na(results.2024$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2024$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2024 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check the democratic senate primary
results.2024 |>
  filter(office == "UNITED STATES SENATOR", party == "REPUBLICAN") |>
  group_by(candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

dir.create("processed-data/august/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2024, "processed-data/august/annual/2024.csv")
