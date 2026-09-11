rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the april 2025 nonpartisan (spring) general election
orig.path <- "original-data/april/April 1 2025 Spring Election_Ward by Ward_State Level Contests and Referendum_2025.xlsx"

# this workbook holds every april 2025 state-level contest plus county circuit
# court and referendum sheets: STATE SUPERINTENDENT OF PUBLIC INSTRUCTION,
# JUSTICE OF THE SUPREME COURT, court of appeals judges, and dozens of county
# circuit court judges. only the superintendent and supreme court contests are
# in scope; everything else is dropped by the scope filter below.
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
# - source candidate names contain double spaces ("Brittany  Kinser", "Jill
#   Underly", "Brad  Schimel") and the write-in suffix is kept in the candidate
#   name ("Adrianne  Melby (write-in)"); kept as-is per the 2018 precedent —
#   only str_to_upper() is applied
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

sheets.2025 <- lapply(seq_along(readxl::excel_sheets(orig.path)), read_sheet_long)

long.2025 <- bind_rows(map(sheets.2025, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2025, "totals"))
vote.sums <- long.2025 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

results.2025 <- long.2025 |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the two offices in scope for april nonpartisan generals
  filter(str_detect(contest, "SUPERINTENDENT OF PUBLIC INSTRUCTION|SUPREME COURT")) |>
  mutate(year = 2025,
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
# be matched against a 2025 file
stopifnot(identical(names(results.2025), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2025$votes)), !any(is.na(results.2025$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2025$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2025 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check: candidate totals, with the official race total from
# race-totals.xlsx for reference (small differences = scattering/undervotes)
results.2025 |> count(office, candidate, wt = votes)
readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
  filter(year == 2025)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2025, "processed-data/april/annual/2025.csv")
