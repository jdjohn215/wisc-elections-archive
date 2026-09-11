rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the april 2021 nonpartisan (spring) general election
orig.path <- "original-data/april/2021_Ward_20by_20Ward_20Report-4.6.21_20Spring_20Election-State_20Superintendent.xlsx"

# no supreme court race was on the april 2021 ballot, so this workbook holds
# just the STATE SUPERINTENDENT OF PUBLIC INSTRUCTION general (Underly vs.
# Kerr). this is the only superintendent race year in the archive without a
# concurrent supreme court race, and the only april election whose statewide
# results are superintendent-only.
#
# layout is the same as the february nonpartisan workbooks:
# - the contest title sits a variable number of rows above the "Total Votes
#   Cast" anchor cell in column 3, so it is taken as the last non-empty cell in
#   column 1 above the anchor
# - party codes ("NP") sit on the anchor row itself and candidate names
#   directly below it; the party column is set to "NONPARTISAN" for all rows
#   instead, so the codes are ignored
# - numbers arrive as text; as.numeric() plus the no-NA assertion below catches
#   conversion failures
# - source candidate names contain double spaces ("Jill  Underly"); kept as-is
#   per the 2018 precedent — only str_to_upper() is applied
read_sheet_long <- function(sheet.no) {
  raw <- readxl::read_excel(orig.path, sheet = sheet.no, col_names = FALSE,
                            .name_repair = "unique_quiet")
  if (ncol(raw) < 3) return(NULL)
  header.start <- which(raw[[3]] == "Total Votes Cast")
  if (length(header.start) == 0) return(NULL)

  # contest title: last non-empty cell in column 1 above the anchor — the
  # offset from the anchor varies between sheets within a workbook
  title.rows <- which(!is.na(raw[[1]]) & seq_len(nrow(raw)) < header.start)
  if (length(title.rows) == 0) return(NULL)
  contest <- raw[[1]][max(title.rows)]

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

sheets.2021 <- lapply(seq_along(readxl::excel_sheets(orig.path)), read_sheet_long)

long.2021 <- bind_rows(map(sheets.2021, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2021, "totals"))
vote.sums <- long.2021 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

results.2021 <- long.2021 |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the two offices in scope for april nonpartisan generals
  filter(str_detect(contest, "SUPERINTENDENT OF PUBLIC INSTRUCTION|SUPREME COURT")) |>
  mutate(year = 2021,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         # nonpartisan contests carry no party, so office = contest title
         office = contest,
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order; the
# template's single example row is 2026 august data, so the row itself can't
# be matched against a 2021 file
stopifnot(identical(names(results.2021), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2021$votes)), !any(is.na(results.2021$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2021$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2021 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check the superintendent general
results.2021 |> count(office, candidate, wt = votes)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2021, "processed-data/april/annual/2021.csv")
