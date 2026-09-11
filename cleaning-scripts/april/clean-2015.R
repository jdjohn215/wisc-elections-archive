rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the april 2015 nonpartisan (spring) general election
orig.path <- "original-data/april/Ward_20Results_204.7.15_20Spring_20Election_2015.xlsx"

# this workbook holds every april 2015 state-level contest: JUSTICE OF THE
# SUPREME COURT (Bradley vs. Daley), court of appeals judges, and many county
# circuit court judges. only the supreme court contest is in scope; the
# judicial races are dropped by the scope filter below. 2015 was not a
# superintendent race year.
#
# quirk: the supreme court sheet's contest title carries a term annotation
# ("JUSTICE OF THE SUPREME COURT - 2015-2025"), which is stripped below so
# office names match the other years.
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

sheets.2015 <- lapply(seq_along(readxl::excel_sheets(orig.path)), read_sheet_long)

long.2015 <- bind_rows(map(sheets.2015, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2015, "totals"))
vote.sums <- long.2015 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

results.2015 <- long.2015 |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the two offices in scope for april nonpartisan generals
  filter(str_detect(contest, "SUPERINTENDENT OF PUBLIC INSTRUCTION|SUPREME COURT")) |>
  mutate(year = 2015,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         # nonpartisan contests carry no party, so office = contest title;
         # strip the term annotation so it matches the other years' titles
         office = str_remove(contest, " - \\d{4}-\\d{4}$"),
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order
stopifnot(identical(names(results.2015), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2015$votes)), !any(is.na(results.2015$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2015$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2015 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check: candidate totals, with the official race total from
# race-totals.xlsx for reference (small differences = scattering/undervotes)
results.2015 |> count(office, candidate, wt = votes)
readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
  filter(year == 2015)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2015, "processed-data/april/annual/2015.csv")
