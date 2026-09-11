rm(list = ls())

library(tidyverse)
library(pdftools)

template <- read_csv("template.csv")

# the april 2001 nonpartisan (spring) general election
orig.path <- "original-data/april/2001-04-03_SpringElection_SupremeCourt_WardbyWard_2001.pdf"

# the 2000-2003 supreme court results are published as pdf tables: one row
# per reporting unit ("<municipality> <type> <ward info>", e.g. "Adams Town
# Wards 1 & 2"), candidate names in the page header, and "X County" header
# rows labelling each county block. candidate columns are separated by 2+
# spaces, so the table can be split positionally. single-ward municipalities
# carry no ward segment in the pdf; following the 2005-2009 convention,
# " WARD 1" is appended. the pdf has no ballots-cast column, so total_votes
# is computed as the candidate vote sum per reporting unit.
#
# this file holds the JUSTICE OF THE SUPREME COURT contest only (Prosser
# unopposed). 2001 was also a superintendent race year (Burmaster vs.
# Cross), but this file does not contain it; a separate source is needed if
# it is ever added.
#
# reporting_unit is the era-native string uppercased (e.g. "ADAMS TOWN
# WARDS 1 & 2"), which differs from the 2011+ workbooks' "TOWN OF ADAMS
# WARDS 1 & 2" — beware when joining across eras.

all.pages <- pdf_text(orig.path)

read_pdf_page <- function(page) {
  d <- all.pages[[page]]

  # split into lines, collapsing blank lines
  d2 <- d |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\n\n", "\n") |>
    str_split("\n")
  d3 <- tibble(x1 = d2[[1]])

  # the first line ending in "County" starts the data table
  start.row <- min(which(str_detect(d3$x1, "County$")))

  d3 |>
    filter(row_number() >= start.row,
           # drop repeated column headers and totals rows
           str_detect(x1, "Municipality Name", negate = TRUE),
           str_detect(x1, "Totals", negate = TRUE)) |>
    mutate(x1 = str_remove(x1, "^ ")) |>
    separate(x1, into = c("rep_unit", "david t prosser, jr.", "Scattering"),
             sep = " {2,}") |>
    mutate(across(where(is.character), str_squish),
           across(where(is.character), ~na_if(.x, "")),
           county = if_else(str_detect(rep_unit, " County$"), rep_unit, NA),
           county = zoo::na.locf(county)) |>
    filter(rep_unit != county,
           str_detect(rep_unit, "Totals :| Totals:", negate = TRUE),
           !is.na(rep_unit),
           rep_unit != "")
}

all.df <- map_df(seq_along(all.pages), read_pdf_page)

results.2001 <- all.df |>
  pivot_longer(cols = -c(rep_unit, county), names_to = "candidate", values_to = "votes") |>
  filter(!is.na(votes)) |>
  mutate(
    votes = as.numeric(str_remove_all(votes, coll(","))),
    county = str_to_upper(str_remove(county, " County$")),
    candidate = str_to_upper(candidate),
    reporting_unit = str_to_upper(rep_unit),
    # single-ward municipalities carry no ward segment in the pdf; append the
    # implicit "Ward 1" to match the 2005-2009 convention
    reporting_unit = if_else(str_detect(reporting_unit, "WARD", negate = TRUE),
                             str_c(reporting_unit, " WARD 1"), reporting_unit)
  ) |>
  select(-rep_unit) |>
  # no ballots-cast figure in the pdf; total_votes = candidate vote sum
  group_by(county, reporting_unit) |>
  mutate(total_votes = sum(votes)) |>
  ungroup() |>
  mutate(year = 2001,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         office = "JUSTICE OF THE SUPREME COURT",
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order
stopifnot(identical(names(results.2001), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2001$votes)), !any(is.na(results.2001$total_votes)))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2001 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check: candidate totals, cross-checked against the official race
# total from race-totals.xlsx (the pdf table's sum should track it closely;
# a large difference means a parsing error)
results.2001 |> summarise(votes = sum(votes), .by = candidate)
results.2001 |> summarise(total = sum(votes))
readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
  filter(year == 2001)
stopifnot(abs(sum(results.2001$votes) -
  (readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
     filter(year == 2001) |> pull(total))) < 1000)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2001, "processed-data/april/annual/2001.csv")
