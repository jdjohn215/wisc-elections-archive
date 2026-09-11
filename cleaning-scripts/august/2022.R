rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the august 2022 partisan primary
#
# unlike 2024 and 2026 (one "All Contests"/"All State Contests" workbook), the
# 2022 ward-by-ward data comes as one file per office: US Senator, Governor,
# State Senator, Representative to Assembly, and Representative in Congress
# (the congressional workbook, added to the archive in September 2026, uses
# the same generic sheet names and "Total Votes Cast" anchor layout as the
# other four files). The contest title is taken from each sheet's header
# block, as in 2024
#
# no district attorney contests exist in these files (each file is a single
# state office), so the 2024 "COUNT(Y|IES) DISTRICT ATTORNEY" filter is not
# needed; a check below asserts the office inventory is as expected.
orig.files <- list.files("original-data/august/2022", full.names = TRUE)

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

# one empty trailing stub sheet exists in the assembly workbook (5 rows x 1
# column); the ncol < 3 / no-anchor guards return NULL for it
sheets.2022 <- lapply(orig.files, function(f) {
  lapply(2:length(excel_sheets(f)), function(s) read_sheet_long(f, s))
}) |> unlist(recursive = FALSE)

long.2022 <- bind_rows(map(sheets.2022, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2022, "totals"))
vote.sums <- long.2022 |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

# every extracted contest must appear in its file's document map (sheet 1)
map.contests <- map(orig.files, function(f) {
  na.omit(unlist(read_excel(f, sheet = 1, col_names = FALSE,
                            .name_repair = "unique_quiet")[[2]]))
}) |> unlist()
stopifnot(nrow(anti_join(distinct(long.2022, contest),
                         tibble(contest = map.contests))) == 0)

results.2022 <- long.2022 |>
  extract(contest, into = c("office", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper)) |>
  # drop contests whose only candidate column is SCATTERING, i.e. no one on
  # the ballot
  group_by(office, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  mutate(year = 2022,
         month = "AUGUST",
         election_type = "PARTISAN PRIMARY") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order; the
# template's single example row is 2026 data, so the row itself can't be
# matched against a 2022 file
stopifnot(identical(names(results.2022), names(template)))

# office inventory: no congressional or gubernatorial special elections were
# on the 2022 primary ballot, and 2022's state senate class was the ODD
# districts (2024's was even); congressional districts 1-8 use the post-2022
# maps (2016/2018/2020 used the pre-2022 district boundaries)
stopifnot(setequal(unique(results.2022$office),
                   c("UNITED STATES SENATOR", "GOVERNOR",
                     str_c("REPRESENTATIVE IN CONGRESS DISTRICT ", 1:8),
                     str_c("STATE SENATOR DISTRICT ", seq(1, 33, 2)),
                     str_c("REPRESENTATIVE TO THE ASSEMBLY DISTRICT ", 1:99))))

# parties: 2022 put Democratic, Republican, Libertarian, and Constitution
# primaries on the ballot (no Wisconsin Green contests), but ALL 126
# Constitution contests were scattering-only (118 state-office + 8
# congressional), as were 124/126 Libertarian contests — the named-candidate
# exceptions were Assembly District 66 and congressional District 8 (Jacob
# J. VandenPlas). So only three parties survive in the output. The CD8
# Democratic write-in is JULIE HANCOCK (WRITE-IN CANDIDATE) — this file uses
# the "(write-in candidate)" suffix, unlike other years' "(write-in)"
stopifnot(setequal(unique(results.2022$party),
                   c("REPUBLICAN", "DEMOCRATIC", "LIBERTARIAN")))

# no failed numeric conversions
stopifnot(!any(is.na(results.2022$votes)), !any(is.na(results.2022$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2022$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2022 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check the gubernatorial primaries
results.2022 |>
  filter(office == "GOVERNOR") |>
  group_by(party, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

dir.create("processed-data/august/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2022, "processed-data/august/annual/2022.csv")
