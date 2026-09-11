rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the april 2023 nonpartisan (spring) general election
orig.path <- "original-data/april/Ward by Ward Report statewide contests_2023.xlsx"

# NOTE: this workbook does NOT contain the april 2023 STATE SUPERINTENDENT OF
# PUBLIC INSTRUCTION general (Underly vs. Kerr) — its document map lists only
# the STATE SENATOR DISTRICT 8 special election, JUSTICE OF THE SUPREME COURT,
# court of appeals judges, and county circuit court judges. only the supreme
# court contest survives the scope filter below; the SD-8 special and judicial
# races are out of scope. the superintendent results need a separate source
# file if they are ever added.
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
# - source candidate names contain double spaces ("Daniel  Kelly"); kept as-is
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

sheets.2023 <- lapply(seq_along(readxl::excel_sheets(orig.path)), read_sheet_long)

long.2023 <- bind_rows(map(sheets.2023, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2023, "totals"))
vote.sums <- long.2023 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

results.2023 <- long.2023 |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the two offices in scope for april nonpartisan generals; in this
  # workbook that means the supreme court alone (see note above)
  filter(str_detect(contest, "SUPERINTENDENT OF PUBLIC INSTRUCTION|SUPREME COURT")) |>
  mutate(year = 2023,
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
# be matched against a 2023 file
stopifnot(identical(names(results.2023), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2023$votes)), !any(is.na(results.2023$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2023$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2023 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check: candidate totals, with the official race total from
# race-totals.xlsx for reference (small differences = scattering/undervotes)
results.2023 |> count(office, candidate, wt = votes)
readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
  filter(year == 2023)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2023, "processed-data/april/annual/2023.csv")
