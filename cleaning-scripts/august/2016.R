rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the august 2016 partisan primary
#
# like 2018/2020/2022, the 2016 ward-by-ward data comes as one workbook per
# office, with generically named sheets (contest titles pulled from each
# sheet's header block) and URL-encoded file names. four offices were on the
# 2016 primary ballot: US Senator, Representative in Congress, State Senator,
# and Representative to the Assembly -- no Governor (2014-2018 gubernatorial
# term). 2016's state senate class was the EVEN districts 2-32
orig.files <- list.files("original-data/august/2016", full.names = TRUE)

# header rows vary slightly between sheets, so anchor on the "Total Votes
# Cast" cell rather than fixed row numbers
read_sheet_long <- function(orig.path, sheet.no) {
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

# one empty trailing stub sheet exists in the assembly workbook (496 contest
# sheets vs 495 mapped contests); the ncol < 3 / no-anchor guards return NULL
# for it. all five parties (incl. Wisconsin Green) ran primaries for every
# office, so the other three files have no stubs: 620 contests total
sheets.2016 <- lapply(orig.files, function(f) {
  lapply(2:length(excel_sheets(f)), function(s) read_sheet_long(f, s))
}) |> unlist(recursive = FALSE)

long.2016 <- bind_rows(map(sheets.2016, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2016, "totals"))
vote.sums <- long.2016 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

# every extracted contest must match its file's document map (sheet 1),
# in both directions; map() + unlist() (not map_chr) because each file's
# map lists multiple contests
map.contests <- map(orig.files, function(f) {
  na.omit(unlist(read_excel(f, sheet = 1, col_names = FALSE,
                            .name_repair = "unique_quiet")[[2]]))
}) |> unlist()
stopifnot(setequal(unique(long.2016$contest), map.contests))

results.2016 <- long.2016 |>
  extract(contest, into = c("office", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper)) |>
  # drop contests whose only candidate column is SCATTERING, i.e. no one on
  # the ballot
  group_by(office, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  mutate(year = 2016,
         month = "AUGUST",
         election_type = "PARTISAN PRIMARY") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order; the
# template's single example row is 2026 data, so the row itself can't be
# matched against a 2016 file
stopifnot(identical(names(results.2016), names(template)))

# office inventory: no Governor; congressional districts 1-8 under the
# pre-2022 maps, 2016's state senate class was the EVEN districts 2-32
# (2018's was odd), and all 99 assembly districts
stopifnot(setequal(unique(results.2016$office),
                   c("UNITED STATES SENATOR",
                     str_c("REPRESENTATIVE IN CONGRESS DISTRICT ", 1:8),
                     str_c("STATE SENATOR DISTRICT ", seq(2, 32, 2)),
                     str_c("REPRESENTATIVE TO THE ASSEMBLY DISTRICT ", 1:99))))

# parties: no Constitution or Wisconsin Green candidate was on the 2016
# primary ballot anywhere (all their contests were scattering-only), so only
# DEMOCRATIC, REPUBLICAN, and LIBERTARIAN survive the filter; 424 of 620
# contests were dropped, with the Libertarian survivors being US Senator,
# congressional districts 1/4/5, and assembly districts 7/54/66. the only
# write-in candidate is DMITRY BECKER (WRITE-IN) in the Republican State
# Senator District 28 primary
stopifnot(setequal(unique(results.2016$party),
                   c("DEMOCRATIC", "REPUBLICAN", "LIBERTARIAN")))

# no failed numeric conversions
stopifnot(!any(is.na(results.2016$votes)), !any(is.na(results.2016$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2016$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2016 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check the statewide primary
results.2016 |>
  filter(office == "UNITED STATES SENATOR") |>
  group_by(office, party, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

write_csv(results.2016, "processed-data/august/annual/2016.csv")
