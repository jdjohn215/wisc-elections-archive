rm(list = ls())

library(tidyverse)

# The 2010, 2012, and 2014 election files for statewide office lack
#   legislative district codes. This script extracts the reporting units used
#   in each state assembly district, then uses state assembly district to
#   determine state senate district. The output is then appended to the election
#   results in the primary build script.

sheets.2010 <- readxl::read_excel("original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls",
                                  sheet = 1) |>
  mutate(sheetno = row_number() + 1)
sheets.2012 <- readxl::read_excel("original-data/2012-11-06_Ward_by_Ward.xls",
                                  sheet = 1) |>
  mutate(sheetno = row_number() + 1)
sheets.2014 <- readxl::read_excel("original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx",
                                  sheet = 1) |>
  mutate(sheetno = row_number() + 1)

# assign state senate district based on state assembly district
wsa.to.wss <- tibble(wsa_dist = 1:99,
                     wss_dist = rep(1:33, each = 3))

read_assembly <- function(sheet, path){
  orig <- readxl::read_excel(path = path, sheet = sheet, col_names = F)
  distrow <- which(str_detect(str_to_upper(orig$...1), "ASSEMBLY - DISTRICT"))

  readxl::read_excel(path = path, sheet = sheet, col_names = F,
                     skip = (distrow + 3)) |>
    select(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county),
           wsa_dist = orig$...1[distrow],
           wsa_dist = as.numeric(word(wsa_dist, -1))) |>
    filter(str_detect(rep_unit, "Totals:", negate = T),
           !is.na(rep_unit)) |>
    select(county, rep_unit, wsa_dist) |>
    left_join(wsa.to.wss)
}

assembly.2010 <- map_df(32:130, read_assembly, path = "original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls")
assembly.2012 <- map_df(28:126, read_assembly, path = "original-data/2012-11-06_Ward_by_Ward.xls")
assembly.2014 <- map_df(31:129, read_assembly, path = "original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx")


read_congress <- function(sheet, path){
  orig <- readxl::read_excel(path = path, sheet = sheet, col_names = F)
  distrow <- which(str_detect(str_to_upper(orig$...1), "CONGRESSIONAL - DISTRICT"))
  
  readxl::read_excel(path = path, sheet = sheet, col_names = F,
                     skip = (distrow + 3)) |>
    select(county = 1, rep_unit = 2) |>
    mutate(county = zoo::na.locf(county),
           con_dist = orig$...1[distrow],
           con_dist = as.numeric(word(con_dist, -1))) |>
    filter(str_detect(rep_unit, "Totals:", negate = T),
           !is.na(rep_unit)) |>
    select(county, rep_unit, con_dist)
}

congress.2010  <- map_df(7:14, read_congress, path = "original-data/2010-11-02_Fall_General_All_Races_Ward_by_Ward.xls")
congress.2012 <- map_df(4:11, read_congress, path = "original-data/2012-11-06_Ward_by_Ward.xls")
congress.2014 <- map_df(6:13, read_congress, path = "original-data/11.4.2014%20Election%20Results%20-%20all%20offices%20w%20x%20w%20report.xlsx")

# in 2012
#   TOWN OF EAGLE POINT Wards 1 - 5 & 5S appears in multiple congressional districts
#     3rd congressional district segment has 0 votes
#   TOWN OF EDSON Wards 1 - 2 & 2 S appears in multiple congressional districts
#     
#   CITY OF SHEBOYGAN FALLS WARDS 1-2 & 9 appears in multiple assembly districts
#     27th assembly district segment has 0 votes
#   CITY OF SHEBOYGAN FALLS WARDS 3-5
#     27th assembly district segment has 0 votes
#   VILLAGE OF MONTFORT Ward 2
#     51st assembly district segment has 0 votes
#   TOWN OF DELMAR Ward 1
#     68th assembly district segment has 0 votes
#   CITY OF STANLEY Wards 1 - 4, 6 - 7
#     67th assembly district segment has 0 votes

all.assembly <- bind_rows(
  assembly.2010 |> mutate(year = 2010),
  assembly.2012 |> mutate(year = 2012),
  assembly.2014 |> mutate(year = 2014)
) |>
  filter(! (rep_unit == "CITY OF SHEBOYGAN FALLS WARDS 1-2 & 9" &
              wsa_dist == 27 & year == 2012),
         ! (rep_unit == "CITY OF SHEBOYGAN FALLS WARDS 3-5" &
              wsa_dist == 27 & year == 2012),
         ! (rep_unit == "VILLAGE OF MONTFORT Ward 2" &
              wsa_dist == 51 & year == 2012),
         ! (rep_unit == "TOWN OF DELMAR Ward 1" &
              wsa_dist == 68 & year == 2012),
         ! (rep_unit == "CITY OF STANLEY Wards 1 - 4, 6 - 7" &
              wsa_dist == 67 & year == 2012))
all.congress <- bind_rows(
  congress.2010 |> mutate(year = 2010),
  congress.2012 |> mutate(year = 2012),
  congress.2014 |> mutate(year = 2014)
) |>
  filter(! (rep_unit == "TOWN OF EAGLE POINT Wards 1 - 5 & 5S" &
              con_dist == 3 & year == 2012),
         ! (rep_unit == "TOWN OF EDSON Wards 1 - 2 & 2 S" &
              con_dist == 7 & year == 2012))

all.rep.units.to.districts <-  left_join(all.assembly, all.congress)

write_csv(all.rep.units.to.districts, "processed-data/annual/2010-2014-rep-unit-dists.csv")

all.rep.units.to.districts |>
  group_by(county, rep_unit, year) |>
  filter(n_distinct(con_dist) > 1)
all.rep.units.to.districts |>
  group_by(county, rep_unit, year) |>
  filter(n_distinct(wsa_dist) > 1)
