rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the April 7, 2020 spring election and presidential preference vote. April
# 2020 held BOTH a nonpartisan spring general (justice of the supreme court,
# Karofsky vs. Kelly) and the presidential preference vote, so this script
# reads two workbooks and writes both to processed-data/april/annual/2020.csv
# (user decision, Sept 2026: one file per year per election month, contests
# distinguished by election_type and party — 2024 avoided this collision only
# because it had no spring general).

# ---- spring general: justice of the supreme court ---------------------------

spring.path <- "original-data/april/Ward_20by_20Ward_20Report_Supreme_20Court_2020.xlsx"

# the spring workbook holds only the JUSTICE OF THE SUPREME COURT contest
# (held during the april 2020 presidential preference primary amid the
# covid-19 pandemic). layout is the same as the february nonpartisan
# workbooks: the contest title sits a variable number of rows above the
# "Total Votes Cast" anchor in column 3 (taken as the last non-empty cell in
# column 1 above the anchor), party codes ("NP") sit on the anchor row and
# candidate names directly below it, and trailing "County Totals:"/"Office
# Totals:" rows bracket the ward table. party codes are ignored — every row
# gets party = "NONPARTISAN". source candidate names contain double spaces
# ("Daniel  Kelly"); kept as-is per the 2018 precedent — only str_to_upper()
# is applied.
read_sheet_long_nonpartisan <- function(sheet.no) {
  raw <- readxl::read_excel(spring.path, sheet = sheet.no, col_names = FALSE,
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

spring.sheets <- lapply(seq_along(readxl::excel_sheets(spring.path)),
                        read_sheet_long_nonpartisan)

long.spring <- bind_rows(map(spring.sheets, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup() |>
  # quirk: the workbook lists the town of MEENON's reporting unit (Burnett
  # county, overlapping the St. Croix reservation) three times — once with
  # the actual votes and twice as all-zero rows. keep the first occurrence,
  # which is the one with the votes; the all-zero duplicates add nothing, so
  # the office-totals check below still passes either way
  distinct(contest, county, reporting_unit, candidate, .keep_all = TRUE)

# candidate vote sums per contest must equal the workbook's own "Office
# Totals:" row values
spring.office.totals <- bind_rows(map(spring.sheets, "totals"))
spring.vote.sums <- long.spring |>
  group_by(contest, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(spring.office.totals, spring.vote.sums)) == 0)
stopifnot(nrow(anti_join(spring.vote.sums, spring.office.totals)) == 0)

results.spring <- long.spring |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the two offices in scope for april nonpartisan generals
  filter(str_detect(contest, "SUPERINTENDENT OF PUBLIC INSTRUCTION|SUPREME COURT")) |>
  mutate(year = 2020,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         # nonpartisan contests carry no party, so office = contest title
         office = contest,
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# ---- presidential preference vote --------------------------------------------

pres.path <- "original-data/april/2020_Ward_20by_20Ward_20Report_President.xlsx"

# the presidential workbook holds sheet 1 (a document map) plus one sheet per
# party's primary: "PRESIDENT OF THE UNITED STATES - Democratic" (15 named
# candidates incl. the Uninstructed Delegate ballot line) and
# "... - Republican" (Donald J. Trump, an Adam Nicholas Paul write-in, and
# the Uninstructed Delegate line). conventions follow the August
# partisan-primary family via the 2024 presidential script (see
# cleaning-scripts/april/clean-2024.R): "Total Votes Cast" anchor in column 3
# (row 10 on the democratic sheet, row 9 on the republican — leading rows vary
# between sheets in this workbook too), party codes on the anchor row,
# candidate names directly below it, trailing SCATTERING column, and
# "County Totals:"/"Office Totals:" trailing rows. the democratic sheet has an
# all-NA spacer column between Deval Patrick and Bernie Sanders; the NA-based
# candidate-column selection skips it. this workbook's ballot-line label is
# "Uninstructed Delegate" (2024's was "Uninstructed Delegation") and the
# write-in suffix is "(write-in)" — kept as-is per the minimal-munging
# convention; beware when joining years on candidate name.
read_sheet_long_partisan <- function(sheet.no) {
  raw <- readxl::read_excel(pres.path, sheet = sheet.no, col_names = FALSE,
                            .name_repair = "unique_quiet")
  if (ncol(raw) < 3) return(NULL)
  header.start <- which(raw[[3]] == "Total Votes Cast")
  if (length(header.start) == 0) return(NULL)

  contest <- raw[[1]][str_detect(raw[[1]], " - ") & !is.na(raw[[1]])][1]

  cand.names <- as.character(unlist(raw[header.start + 1, -(1:3)]))
  cand.cols <- which(!is.na(cand.names)) + 3
  cand.names <- make.unique(cand.names[!is.na(cand.names)], sep = "__dup")

  rows <- raw[(header.start + 2):nrow(raw), ]
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
      sheet = sheet.no,
      contest = contest,
      county = rep(data[[1]], times = n.cands),
      reporting_unit = rep(data[[2]], times = n.cands),
      total_votes = rep(as.numeric(data[[3]]), times = n.cands),
      candidate = rep(cand.names, each = n.wards),
      # the republican sheet has one blank candidate cell (City of DE PERE
      # Ward 17, Adam Nicholas Paul (write-in)) that the source left empty
      # instead of entering 0 — coalesce blanks to 0 (user decision, Sept
      # 2026; the office-totals check confirms the zero-fill). the totals
      # rows below stay strict so that check keeps meaning.
      votes = coalesce(as.numeric(unlist(data[, cand.cols])), 0)
    ),
    totals = tibble(
      sheet = sheet.no,
      candidate = cand.names,
      votes = as.numeric(unlist(rows[totals.row, cand.cols]))
    )
  )
}

# sheet 1 is the document map; the document map must list exactly the
# contests extracted from the remaining sheets
pres.sheet.names <- readxl::excel_sheets(pres.path)
stopifnot(pres.sheet.names[1] == "Document map")
map.contests <- readxl::read_excel(pres.path, sheet = 1, col_names = FALSE,
                                   .name_repair = "unique_quiet")[[2]]
pres.sheets <- lapply(setdiff(seq_along(pres.sheet.names), 1),
                      read_sheet_long_partisan)
long.pres <- bind_rows(map(pres.sheets, "votes"))
stopifnot(setequal(na.omit(map.contests), distinct(long.pres, contest)$contest))

long.pres <- long.pres |>
  # county is only labelled on the first row of each county block; keyed by
  # sheet, not contest title (the 2024 script's convention)
  group_by(sheet) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup() |>
  # quirk: like the spring workbook, the town of MEENON's reporting unit
  # (Burnett county, overlapping the St. Croix reservation) is listed three
  # times — once with the votes and twice as all-zero rows — but here the
  # vote-carrying occurrence comes LAST (in the spring workbook it comes
  # first). order by ballots cast before distinct() so the occurrence with
  # the votes survives; the office-totals check below verifies it.
  arrange(sheet, county, reporting_unit, candidate, desc(total_votes)) |>
  distinct(sheet, county, reporting_unit, candidate, .keep_all = TRUE)

# candidate vote sums per sheet must equal the workbook's own "Office
# Totals:" row values
pres.office.totals <- bind_rows(map(pres.sheets, "totals"))
pres.vote.sums <- long.pres |>
  group_by(sheet, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(pres.office.totals, pres.vote.sums)) == 0)
stopifnot(nrow(anti_join(pres.vote.sums, pres.office.totals)) == 0)

results.pres <- long.pres |>
  extract(contest, into = c("office_title", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper)) |>
  # keep only the presidential preference primaries; this workbook holds no
  # other contests, but the filter mirrors the 2024 script's scope guard
  filter(office_title == "PRESIDENT OF THE UNITED STATES") |>
  # drop contests whose only candidate column is SCATTERING, i.e. no one on
  # the ballot (a no-op here: both parties had named candidates; kept to
  # match the august conventions)
  group_by(office_title, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  # the archive's office label for presidential preference primaries
  # (user decision, Sept 2026); the source title is
  # "PRESIDENT OF THE UNITED STATES"
  mutate(office = "PRESIDENT",
         year = 2020,
         month = "APRIL",
         election_type = "PRESIDENTIAL PREFERENCE PRIMARY") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# ---- combine and sanity-check -------------------------------------------------

results.2020 <- bind_rows(results.spring, results.pres)

# output schema must match the template's column names and order; the
# template's single example row is 2026 data, so the row itself can't be
# matched against a 2020 file
stopifnot(identical(names(results.2020), names(template)))

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

# spot checks: candidate totals, with the official race total from
# race-totals.xlsx for the spring general (small differences =
# scattering/undervotes) and the workbook's office-totals rows for the
# presidential primaries
results.2020 |> count(election_type, party, office, candidate, wt = votes)
readxl::read_excel("processed-data/legacy/nonpartisan/race-totals.xlsx") |>
  filter(year == 2020)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2020, "processed-data/april/annual/2020.csv")
