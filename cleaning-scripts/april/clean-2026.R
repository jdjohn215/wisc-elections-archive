rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the april 2026 nonpartisan (spring) general election
orig.path <- "original-data/april/Ward by Ward Report_Spring Election 2026_All State Contests.xlsx"

# this workbook holds every april 2026 state-level contest: JUSTICE OF THE
# SUPREME COURT (Taylor vs. Lazar) plus court of appeals and county circuit
# court judges. 2026 was not a superintendent race year, so only the supreme
# court contest is in scope; the judicial races are dropped by the scope
# filter below.
#
# quirk: for two contests (JUSTICE OF THE SUPREME COURT and COURT OF APPEALS
# JUDGE DISTRICT 4) the workbook's own "Office Totals:" row disagrees with the
# sum of its own "County Totals:" rows — by 20 and 19 votes respectively.
# the ward rows and county totals rows agree with each other everywhere, so
# the summary row looks stale. the sanity checks below therefore verify
# candidate sums against the per-county totals (the self-consistent
# quantity) and only report office-totals differences within a small
# tolerance.
#
# layout is the same as the february nonpartisan workbooks: the contest title
# sits a variable number of rows above the "Total Votes Cast" anchor in
# column 3 (taken as the last non-empty cell in column 1 above the anchor),
# party codes ("NP") sit on the anchor row and candidate names directly below
# it, and trailing "County Totals:"/"Office Totals:" rows bracket the ward
# table. party codes are ignored — every row gets party = "NONPARTISAN".
read_sheet_long <- function(sheet.no) {
  raw <- readxl::read_excel(orig.path, sheet = sheet.no, col_names = FALSE,
                            .name_repair = "unique_quiet")
  if (ncol(raw) < 3) return(NULL)
  header.start <- which(raw[[3]] == "Total Votes Cast")
  if (length(header.start) == 0) return(NULL)

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

  totals.row <- which(rows[[1]] %in% "Office Totals:")
  if (length(totals.row) == 0) return(NULL)
  ct.rows <- which(rows[[2]] == "County Totals:")

  n.wards <- nrow(data)
  n.cands <- length(cand.names)
  n.counties <- length(ct.rows)
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
    ),
    # the workbook's own per-county totals rows, for the stronger sanity
    # check below (county labels only appear on the first row of each county
    # block, so fill down first)
    county.totals = tibble(
      contest = contest,
      county = rep(zoo::na.locf(rows[[1]])[ct.rows], times = n.cands),
      candidate = rep(cand.names, each = n.counties),
      votes = as.numeric(unlist(rows[ct.rows, cand.cols]))
    )
  )
}

sheets.2026 <- lapply(seq_along(readxl::excel_sheets(orig.path)), read_sheet_long)

long.2026 <- bind_rows(map(sheets.2026, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest and county must equal the workbook's own
# "County Totals:" rows — this is the self-consistent quantity in this
# workbook (see the quirk note above)
county.totals <- bind_rows(map(sheets.2026, "county.totals"))
county.sums <- long.2026 |>
  group_by(contest, county, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(county.totals, county.sums)) == 0)
stopifnot(nrow(anti_join(county.sums, county.totals)) == 0)

# office totals should agree except for the two documented stale rows; any
# larger disagreement is a real extraction error
office.cmp <- bind_rows(map(sheets.2026, "totals")) |>
  left_join(
    long.2026 |> group_by(contest, candidate) |>
      summarise(votes = sum(votes), .groups = "drop"),
    by = c("contest", "candidate"), suffix = c(".official", ".summed")
  ) |>
  mutate(diff = votes.official - votes.summed)
stopifnot(max(abs(office.cmp$diff)) < 100)
office.cmp |> filter(diff != 0)

results.2026 <- long.2026 |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the two offices in scope for april nonpartisan generals
  filter(str_detect(contest, "SUPERINTENDENT OF PUBLIC INSTRUCTION|SUPREME COURT")) |>
  mutate(year = 2026,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         # nonpartisan contests carry no party, so office = contest title
         office = contest,
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order
stopifnot(identical(names(results.2026), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2026$votes)), !any(is.na(results.2026$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2026$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2026 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check the supreme court race
results.2026 |> count(office, candidate, wt = votes)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2026, "processed-data/april/annual/2026.csv")
