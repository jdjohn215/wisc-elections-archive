rm(list = ls())

library(tidyverse)
library(pdftools)

all.pages <- pdf_text("original-data/2004-11-02_FallElection_USCongress_WardbyWard.pdf")


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
  
  district <- d3$x1[str_detect(d3$x1, "US Congress, District No")]
  vote.cols <- unlist(str_split(str_squish(d3$x1[5]), " "))
  vote.cols <- replace(vote.cols, duplicated(vote.cols), paste0(vote.cols[duplicated(vote.cols)], "2"))
  start.row <- min(which(str_detect(d3$x1, "County$")))
  
  d4 <- d3 |>
    filter(row_number() >= start.row) |>
    separate(x1, into = c("rep_unit", vote.cols, "Scattering"),
             sep = " {2,}") |>
    mutate(across(where(is.character), str_squish),
           county = if_else(str_detect(rep_unit, " County$"), rep_unit, NA),
           county = zoo::na.locf(county),
           district = district) |>
    filter(rep_unit != county,
           str_detect(rep_unit, "Totals :| Totals:", negate = T),
           !is.na(rep_unit),
           rep_unit != "")
  d4
}

all.df <- map_df(1:length(all.pages), read_pdf_page, .progress = T)

all.df.final <- all.df %>%
  pivot_longer(cols = -c(rep_unit, county, district), 
               names_to = "party", values_to = "votes") %>%
  filter(!is.na(votes)) %>%
  mutate(
    district = as.numeric(word(district, -1)),
    party = case_when(
      party == "DEM" ~ "Democratic",
      party == "REP" ~ "Republican",
      party == "LIB" ~ "Libertarian",
      party == "WGR" ~ "Wisconsin Greens",
      party == "IND" ~ "Independent",
      party == "IND2" ~ "Independent 2",
      party == "CON" ~ "Constitution",
      party == "Scattering" ~ "Scattering"),
    candidate = case_when(
      district == 1 & party == "Democratic" ~ "Jeffrey Chapman Thomas",
      district == 1 & party == "Republican" ~ "Paul Ryan",
      district == 1 & party == "Libertarian" ~ "Don Bernau",
      district == 1 & party == "Independent" ~ "Norman Aulabaugh",
      district == 2 & party == "Democratic" ~ "Tammy Baldwin",
      district == 2 & party == "Republican" ~ "Dave Magnum",
      district == 3 & party == "Democratic" ~ "Ron Kind",
      district == 3 & party == "Republican" ~ "Dale W Schultz",
      district == 4 & party == "Democratic" ~ "Gwen Moore",
      district == 4 & party == "Republican" ~ "Gerald H Boyle",
      district == 4 & party == "Constitution" ~ "Collin Hudson",
      district == 4 & party == "Independent" ~ "Robert R Raymond",
      district == 4 & party == "Independent 2" ~ "Tim Johnson",
      district == 5 & party == "Democratic" ~ "Bryan Kennedy",
      district == 5 & party == "Republican" ~ "F James Sensenbrenner, Jr",
      district == 5 & party == "Libertarian" ~ "Tim Peterson",
      district == 6 & party == "Democratic" ~ "Jeff Hall",
      district == 6 & party == "Republican" ~ "Tom Petri",
      district == 6 & party == "Wisconsin Greens" ~ "Carol Ann Rittenhouse",
      district == 7 & party == "Democratic" ~ "David R Obey",
      district == 7 & party == "Wisconsin Greens" ~ "Mike Miles",
      district == 7 & party == "Constitution" ~ "Larry Oftedahl",
      district == 8 & party == "Democratic" ~ "Dottie Le Clair",
      district == 8 & party == "Republican" ~ "Mark Green",
      party == "Scattering" ~ "Scattering"
    )
  ) %>%
  separate(rep_unit, into = c("municipality_name", "reporting_unit_name"),
           sep = " (?=Ward)") %>%
  mutate(reporting_unit_name = if_else(is.na(reporting_unit_name), "Ward 1", reporting_unit_name),
         municipality_type = str_sub(word(municipality_name, -1), 1, 1),
         municipality_name = word(municipality_name, 1, -2))

all.df.final %>%
  group_by(county, municipality_name, municipality_type, reporting_unit_name, candidate) %>%
  filter(n() > 1)

all.df.final %>%
  group_by(candidate) %>%
  summarise(count = n()) %>%
  print(n = 25)

write_csv(all.df.final, "processed-data/annual/2004-congress-only.csv")
