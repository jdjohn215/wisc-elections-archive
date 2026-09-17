rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the April 3, 2012 spring election and presidential preference vote. The 2012
# workbook is an "All Contests" file: sheet 1 is a document map, then one
# generically named sheet per contest — the two presidential preference
# primaries (Republican, Democratic) plus 4 courts of appeals and 52 circuit
# court contests, which are dropped, mirroring the February/April scope
# decisions. There was no in-scope spring general in 2012 (no superintendent
# or supreme court race on the April 2012 ballot), so the presidential
# primaries are this script's entire output.
orig.path <- "original-data/april/2012-04-03_Results_by_Ward_Spring_Election_and_Pres_Pref_vot.xls"

# layout matches the 2020/2024 presidential family (see
# cleaning-scripts/april/clean-2024.R): "Total Votes Cast" anchor in column 3
# (row 10 on both presidential sheets), party codes on the anchor row,
# candidate names directly below it, trailing SCATTERING column, per-county
# "County Totals:" rows and a final "Office Totals:" row. Both presidential
# sheets carry an all-NA spacer column among the candidate columns (between
# Huntsman and Romney, and between Bachmann and Paul, on the republican
# sheet; between the Uninstructed Delegation line and SCATTERING on the
# democratic); the NA-based candidate-column selection skips them.
#
# two 2012 quirks: (1) the contest titles read
# "President of the United States - Republican Party" — the party suffix
# includes the word "Party", stripped after uppercasing. (2) unlike 2024's
# mixed workbook, the nonpartisan court sheets' titles DO contain " - "
# (courts of appeals) or lack it entirely (circuit courts, so their extracted
# contest is NA) — extraction and the office-totals check are therefore keyed
# by sheet number, the contest title is only used to select the presidential
# primaries, and the document-map check below is scoped to the presidential
# contests.
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

# the document map is a single column: a "Ward by Ward Report" header row,
# then one contest per row in sheet order (map row i corresponds to sheet i)
sheet.names <- readxl::excel_sheets(orig.path)
stopifnot(sheet.names[1] == "Document map")
map.contests <- readxl::read_excel(orig.path, sheet = 1, col_names = FALSE,
                                   .name_repair = "unique_quiet")[[1]][-1]

sheet.nos <- setdiff(seq_along(sheet.names), 1)
sheets.2012 <- lapply(sheet.nos, read_sheet_long)

# every contest on the map must have a successfully extracted sheet
stopifnot(length(sheet.nos) == length(map.contests))
stopifnot(sum(map_lgl(sheets.2012, is.null)) == 0)

long.2012 <- bind_rows(map(sheets.2012, "votes")) |>
  # county is only labelled on the first row of each county block
  group_by(sheet) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup()

# sanity checks (before any contest filtering) ------------------------------

# candidate vote sums per sheet must equal the workbook's own "Office
# Totals:" row values — across all 58 sheets, presidential and court alike
# (all 58 pass for this workbook, which also confirms the extraction function
# ports to the nonpartisan sheets' layout unchanged)
office.totals <- bind_rows(map(sheets.2012, "totals"))
vote.sums <- long.2012 |>
  group_by(sheet, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")
stopifnot(nrow(anti_join(office.totals, vote.sums)) == 0)
stopifnot(nrow(anti_join(vote.sums, office.totals)) == 0)

# the document map's presidential contests must match exactly what the
# extraction found (the full map can't be compared to extracted contest
# titles: the circuit courts' titles never make it into `contest`)
pres.map <- map.contests[str_detect(map.contests, "President of the United States")]
pres.extracted <- long.2012 |>
  filter(str_detect(contest, "President of the United States")) |>
  distinct(contest) |>
  pull(contest)
stopifnot(setequal(pres.map, pres.extracted))

results.2012 <- long.2012 |>
  extract(contest, into = c("office_title", "party"), regex = "^(.*) - ([^-]+)$") |>
  mutate(across(where(is.character), str_to_upper),
         # the 2012 contest titles append the word "Party" to the suffix
         # ("... - Republican Party"), which the other years' titles omit
         party = str_remove(party, " PARTY$")) |>
  # keep only the presidential preference primaries; drop the court contests
  # this "All Contests" workbook also holds
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
         year = 2012,
         month = "APRIL",
         election_type = "PRESIDENTIAL PREFERENCE PRIMARY") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order; the
# template's single example row is 2026 data, so the row itself can't be
# matched against a 2012 file
stopifnot(identical(names(results.2012), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2012$votes)), !any(is.na(results.2012$total_votes)))

# make.unique() only intervenes if a contest lists the same candidate name
# twice, which shouldn't happen
stopifnot(!any(str_detect(results.2012$candidate, "__dup")))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2012 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check candidate statewide sums against the workbook's office totals:
# including SCATTERING, both parties' candidate sums exactly equal the
# office-totals ballots cast (Rep 787,847; Dem 300,255), and the named
# candidates match certified results (Romney 346,876; Santorum 290,139;
# Paul 87,858; Gingrich 45,978; Obama 293,914)
results.2012 |>
  group_by(party, candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2012, "processed-data/april/annual/2012.csv")
