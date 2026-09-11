rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the august 2020 partisan primary
#
# like 2022 (see clean-2022 notes / 2022.R), the 2020 ward-by-ward data comes
# as one file per office — but this year there is no Governor or US Senator
# workbook (neither office was on the 2020 primary ballot): only
# Representative in Congress, State Senator, and Representative to the
# Assembly. Sheets within each file are generically named, so the contest
# title is taken from each sheet's header block, as in 2022/2024
#
# only three parties held primaries: Democratic, Republican, and Constitution
# (no Libertarian contests at all in the source files, unlike 2022/2024)
orig.files <- list.files("original-data/august/2020", full.names = TRUE)

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

# one empty trailing stub sheet exists in the assembly workbook (298 contest
# sheets vs 297 mapped contests); the ncol < 3 / no-anchor guards return NULL
# for it
sheets.2020 <- lapply(orig.files, function(f) {
  lapply(2:length(excel_sheets(f)), function(s) read_sheet_long(f, s))
}) |> unlist(recursive = FALSE)

long.2020 <- bind_rows(map(sheets.2020, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
office.totals <- bind_rows(map(sheets.2020, "totals"))
vote.sums <- long.2020 |>
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
stopifnot(setequal(unique(long.2020$contest), map.contests))

results.2020 <- long.2020 |>
  extract(contest, into = c("office", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper)) |>
  # drop contests whose only candidate column is SCATTERING, i.e. no one on
  # the ballot
  group_by(office, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  mutate(year = 2020,
         month = "AUGUST",
         election_type = "PARTISAN PRIMARY") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order; the
# template's single example row is 2026 data, so the row itself can't be
# matched against a 2020 file
stopifnot(identical(names(results.2020), names(template)))

# office inventory: 2020's state senate class was the EVEN districts 2-32
# (2022's was odd 1-33), all 99 assembly districts, and congressional
# districts 1-8 under the pre-2022 maps; no Governor, US Senator, or
# special-election contests are in the source files
stopifnot(setequal(unique(results.2020$office),
                   c(str_c("REPRESENTATIVE IN CONGRESS DISTRICT ", 1:8),
                     str_c("STATE SENATOR DISTRICT ", seq(2, 32, 2)),
                     str_c("REPRESENTATIVE TO THE ASSEMBLY DISTRICT ", 1:99))))

# parties: unlike 2022 (where all Constitution contests were scattering-only)
# and 2024 (no minor-party candidates at all), 2020 has one named-candidate
# Constitution contest — Assembly District 3, with write-in Joshua Young —
# so all three ballot parties survive the scattering-only filter. The
# scattering-only counts were 12 Democratic and 10 Republican no-candidate
# contests plus 122/123 Constitution
stopifnot(setequal(unique(results.2020$party),
                   c("REPUBLICAN", "DEMOCRATIC", "CONSTITUTION")))

# no failed numeric conversions
stopifnot(!any(is.na(results.2020$votes)), !any(is.na(results.2020$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2020$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2020 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check the congressional primaries
results.2020 |>
  filter(office == str_c("REPRESENTATIVE IN CONGRESS DISTRICT ", 1:8)) |>
  group_by(office, party, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

write_csv(results.2020, "processed-data/august/annual/2020.csv")
