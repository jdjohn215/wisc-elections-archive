rm(list = ls())

library(tidyverse)
library(pdftools)

template <- read_csv("template.csv")

# the april 2003 nonpartisan (spring) general election
orig.path <- "original-data/april/2003-04-01_elec_wbw_supreme_2003.pdf"

# the 2000-2003 supreme court results are published as pdf tables: one row
# per reporting unit ("<municipality> <type> <ward info>", e.g. "Adams Town
# Wards 1 & 2"), candidate names in the page header, and "X County" header
# rows labelling each county block. candidate columns are separated by 2+
# spaces, so the table can be split positionally. single-ward municipalities
# carry no ward segment in the pdf; following the 2005-2009 convention,
# " WARD 1" is appended. the pdf has no ballots-cast column, so total_votes
# is computed as the candidate vote sum per reporting unit. 2003 was not a
# superintendent race year.
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
           # drop repeated column headers (totals rows are munged below
           # because the 2003 pdf splits some totals rows differently)
           str_detect(x1, "Municipality Name", negate = TRUE)) |>
    mutate(x1 = str_remove(x1, "^ ")) |>
    separate(x1, into = c("rep_unit", "ed brunner", "pat roggensack", "Scattering"),
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

all.df <- map_df(seq_along(all.pages), read_pdf_page) |>
  # quirk: Marshfield City Ward 2 straddles a pdf page break and parses
  # wrong; patch it with the correct values from the report
  mutate(`ed brunner` = if_else(rep_unit == "Marshfield City Ward 2", "169", `ed brunner`),
         `pat roggensack` = if_else(rep_unit == "Marshfield City Ward 2", "205", `pat roggensack`),
         Scattering = if_else(rep_unit == "Marshfield City Ward 2", "0", Scattering))

results.2003 <- all.df |>
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
  mutate(year = 2003,
         month = "APRIL",
         election_type = "SPRING GENERAL",
         office = "JUSTICE OF THE SUPREME COURT",
         party = "NONPARTISAN") |>
  select(year, month, county, reporting_unit, election_type,
         office, party, candidate, total_votes, votes)

# sanity checks ---------------------------------------------------------------

# output schema must match the template's column names and order
stopifnot(identical(names(results.2003), names(template)))

# no failed numeric conversions
stopifnot(!any(is.na(results.2003$votes)), !any(is.na(results.2003$total_votes)))

# no duplicate rows by (office, party, county, reporting_unit, candidate)
stopifnot(
  results.2003 |>
    count(office, party, county, reporting_unit, candidate) |>
    filter(n > 1) |>
    nrow() == 0
)

# spot check: candidate totals, cross-checked against the official race
# total from race-totals.xlsx (the pdf table's sum should track it closely;
# a large difference means a parsing error)
results.2003 |> summarise(votes = sum(votes), .by = candidate)
results.2003 |> summarise(total = sum(votes))
readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
  filter(year == 2003)
stopifnot(abs(sum(results.2003$votes) -
  (readxl::read_excel("processed-data/nonpartisan/race-totals.xlsx") |>
     filter(year == 2003) |> pull(total))) < 1000)

dir.create("processed-data/april/annual", recursive = TRUE, showWarnings = FALSE)
write_csv(results.2003, "processed-data/april/annual/2003.csv")
