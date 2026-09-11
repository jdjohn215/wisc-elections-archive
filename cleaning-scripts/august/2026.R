rm(list = ls())

library(tidyverse)
library(readxl)

template <- read_csv("template.csv")

# the august 2026 partisan primary
orig.path <- "original-data/august/Ward by Ward Report_Partisan Primary 2026_All State Contests.xlsx"

# the workbook has one sheet per office-party contest (e.g. "GOVERNOR -
# Republican"), plus a "Document map" sheet and an empty stub at the end.
# header rows vary slightly between sheets, so anchor on the "Total Votes
# Cast" cell rather than fixed row numbers
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

  data <- raw[(header.start + 2):nrow(raw), ]
  # ward rows only; %in% (not !=) so rows with a blank first column don't
  # become all-NA rows when used as a logical subscript
  ward.rows <- !is.na(data[[2]]) &
    !str_detect(data[[2]], "County Totals") &
    !(data[[1]] %in% "Office Totals:")
  data <- data[ward.rows, ]

  # unlist() works column-major, so expand the base columns to match:
  # one block of ward rows per candidate
  n.wards <- nrow(data)
  n.cands <- length(cand.names)
  tibble(
    contest = contest,
    county = rep(data[[1]], times = n.cands),
    reporting_unit = rep(data[[2]], times = n.cands),
    total_votes = rep(as.numeric(data[[3]]), times = n.cands),
    candidate = rep(cand.names, each = n.wards),
    votes = as.numeric(unlist(data[, cand.cols]))
  )
}

results.2026 <- lapply(2:647, read_sheet_long) |>
  bind_rows() |>
  # county is only labelled on the first row of each county block
  group_by(contest) |>
  mutate(county = zoo::na.locf(county, na.rm = FALSE)) |>
  ungroup() |>
  separate(contest, into = c("office", "party"), sep = " - ") |>
  # drop contests where no named candidate received any votes, i.e. contests
  # whose only column is SCATTERING (mostly minor parties, but also 14
  # democratic/republican contests with no one on the ballot)
  group_by(office, party) |>
  filter(any(candidate != "SCATTERING")) |>
  ungroup() |>
  mutate(year = 2026,
         month = "AUGUST",
         election_type = "PARTISAN PRIMARY") |>
  mutate(across(where(is.character), str_to_upper)) |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks -------------------------------------------------------------

# every row of the template must appear in the output
stopifnot(nrow(anti_join(template, results.2026)) == 0)

# candidate vote sums should match the workbook's own office totals; spot
# check the republican governor primary
results.2026 |>
  filter(office == "GOVERNOR", party == "DEMOCRATIC") |>
  group_by(candidate) |>
  summarise(votes = sum(votes), .groups = "drop")

dir.create("processed-data/august/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2026, "processed-data/august/annual/2026.csv")
