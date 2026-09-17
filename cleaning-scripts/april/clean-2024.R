rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the April 2, 2024 spring election / presidential preference vote
orig.path <- "original-data/april/Ward by Ward Report__April 2 2024 Spring Election_All Contests.xlsx"

# "All Contests" workbook: 62 data sheets under generic names (Sheet2...Sheet63).
# Only the two presidential preference primaries are kept; the other 60 sheets
# (courts of appeals, circuit courts, referenda) are dropped, mirroring the
# February/April scope decisions. There was no April 2024 spring general —
# the presidential preference vote is this workbook's state-level partisan
# content, so this script follows the August partisan-primary conventions.
#
# layout matches the August partisan-primary family (see
# cleaning-scripts/august/2024.R): "Total Votes Cast" anchor in column 3,
# party codes one row above the candidate names, trailing SCATTERING column,
# per-county "County Totals:" rows and a final "Office Totals:" row. The
# republican sheet has an all-NA spacer column between Haley and Trump; the
# NA-based candidate-column selection skips it.
#
# sheets are keyed by number, not contest title: the nonpartisan sheets'
# header titles lack the " - " party suffix (partisan style), so title-based
# grouping would pool all 60 of them under contest = NA. The contest title
# (extracted below, " - " style) is only used to select the presidential
# primaries.
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
      sheet = sheet.no,
      contest = contest,
      county = rep(data[[1]], times = n.cands),
      reporting_unit = rep(data[[2]], times = n.cands),
      total_votes = rep(as.numeric(data[[3]]), times = n.cands),
      candidate = rep(cand.names, each = n.wards),
      votes = as.numeric(unlist(data[, cand.cols]))
    ),
    totals = tibble(
      sheet = sheet.no,
      candidate = cand.names,
      votes = as.numeric(unlist(rows[totals.row, cand.cols]))
    )
  )
}

sheet.nos <- setdiff(seq_along(excel_sheets(orig.path)), 1)
sheets.2024 <- lapply(sheet.nos, read_sheet_long)

long.2024 <- bind_rows(map(sheets.2024, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(sheet) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per sheet must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2024, "totals"))
vote.sums <- long.2024 |>
  group_by(sheet, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

results.2024 <- long.2024 |>
  extract(contest, into = c("office_title", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the presidential preference primaries; drop the court and
  # referendum contests this "All Contests" workbook also holds
  filter(office_title == "PRESIDENT OF THE UNITED STATES") |>
  # drop contests whose only candidate column is SCATTERING, i.e. no one on
  # the ballot (both presidential contests have named candidates, so this is
  # a no-op here, kept to match the august conventions)
  group_by(office_title, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  # the archive's office label for presidential preference primaries
  # (user decision, Sept 2026); the source title is
  # "PRESIDENT OF THE UNITED STATES"
  mutate(office = "PRESIDENT",
         year = 2024,
         month = "APRIL",
         election_type = "PRESIDENTIAL PREFERENCE PRIMARY") |>
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

# spot check candidate statewide sums against the workbook's office totals
results.2024 |>
  group_by(party, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2024, "processed-data/april/annual/2024.csv")
