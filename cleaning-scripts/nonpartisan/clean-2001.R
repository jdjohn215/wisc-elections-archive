rm(list = ls())

library(tidyverse)
library(pdftools)

all.pages <- pdf_text("original-data/nonpartisan/2001-04-03_SpringElection_SupremeCourt_WardbyWard_2001.pdf")


read_pdf_page <- function(page){
  # the page
  d <- all.pages[[page]]
  
  # split into separate lines
  d2 <- d |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\n\n", "\n") |>
    str_replace_all("\n\n", "\n") |>
    str_split("\n")
  
  # make a tibble with 1 column
  d3 <- tibble(x1 = d2[[1]])
  
  vote.cols <- unlist(str_split(str_squish(d3$x1[5]), " "))
  vote.cols <- replace(vote.cols, duplicated(vote.cols), paste0(vote.cols[duplicated(vote.cols)], "2"))
  start.row <- min(which(str_detect(d3$x1, "County$")))
  
  d4 <- d3 |>
    filter(row_number() >= start.row,
           str_detect(x1, "Municipality Name", negate = T),
           str_detect(x1, "Totals", negate = T)) |>
    mutate(x1 = str_remove(x1, "^ ")) |>
    separate(x1, into = c("rep_unit", "david t prosser, jr.", "Scattering"),
             sep = " {2,}") |>
    mutate(across(where(is.character), str_squish),
           across(where(is.character), ~na_if(.x, "")),
           county = if_else(str_detect(rep_unit, " County$"), rep_unit, NA),
           county = zoo::na.locf(county)) |>
    filter(rep_unit != county,
           str_detect(rep_unit, "Totals :| Totals:", negate = T),
           !is.na(rep_unit),
           rep_unit != "")
  d4
}

all.df <- map_df(1:length(all.pages), read_pdf_page, .progress = T)

all.df.final <- all.df %>%
  pivot_longer(cols = -c(rep_unit, county), 
               names_to = "candidate", values_to = "votes") %>%
  filter(!is.na(votes)) %>%
  mutate(
    party = case_when(
      candidate == "david t prosser, jr." ~ "Conservative",
      candidate == "Scattering" ~ "Scattering")
  ) %>%
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward)") %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(word(municipality_name, -1), 1, 1),
         municipality_name = word(municipality_name, 1, -2),
         year = 2001,
         office = "Justice of the Supreme Court") |>
  rename(municipality = municipality_name, ctv = municipality_type,
         reporting_unit = reporting_unit_name) |>
  mutate(votes = str_remove_all(votes, coll(","))) |>
  group_by(county, municipality, ctv, reporting_unit) |>
  mutate(total_votes = sum(as.numeric(votes)))

all.df.final %>%
  group_by(candidate) %>%
  summarise(count = n()) %>%
  print(n = 25)

write_csv(all.df.final, "processed-data/nonpartisan/annual/2001.csv")
