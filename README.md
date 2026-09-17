# README


This repository includes Wisconsin election results from four different
kinds of elections: February nonpartisan primaries, April nonpartisan
general elections & presidential preference votes, August partisan
primaries, and November partisan general elections.

Original, official election results at the voting tabulation district
level are included in the `original-data/` directory. Scripts processing
these files into a standardized format are in `cleaning-scripts/`. The
script `cleaning-scripts/combine-all.R` combines all of these files into
a single, master dataset. You can access this dataset as a single zipped
CSV file (`processed-data/AllResults_ReportingUnit.csv.gz`) or as a
Hive-partitioned parquet dataset in `processed-data/parquet/`. The data
looks like this:

    Rows: 186,363
    Columns: 17
    $ mcd_fips       <dbl> 5500100275, 5500100275, 5500100275, 5500100275, 5500100…
    $ county_fips    <dbl> 55001, 55001, 55001, 55001, 55001, 55001, 55001, 55001,…
    $ county         <chr> "ADAMS", "ADAMS", "ADAMS", "ADAMS", "ADAMS", "ADAMS", "…
    $ municipality   <chr> "ADAMS", "ADAMS", "ADAMS", "ADAMS", "ADAMS", "ADAMS", "…
    $ ctv            <chr> "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", …
    $ reporting_unit <chr> "WARDS 1-5", "WARDS 1-5", "WARDS 1-5", "WARDS 1-5", "WA…
    $ year           <dbl> 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2026, 2…
    $ office         <chr> "ATTORNEY GENERAL", "ATTORNEY GENERAL", "ATTORNEY GENER…
    $ party          <chr> "DEMOCRATIC", "DEMOCRATIC", "REPUBLICAN", "REPUBLICAN",…
    $ candidate      <chr> "JOSH KAUL", "SCATTERING", "ERIC TONEY", "SCATTERING", …
    $ votes          <dbl> 141, 0, 113, 1, 36, 115, 0, 113, 2, 0, 0, 0, 56, 55, 7,…
    $ total_votes    <dbl> 255, 255, 255, 255, 266, 266, 266, 266, 266, 268, 268, …
    $ month          <chr> "AUGUST", "AUGUST", "AUGUST", "AUGUST", "AUGUST", "AUGU…
    $ district       <dbl> 0, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    $ con_dist       <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3…
    $ wss_dist       <dbl> 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,…
    $ wsa_dist       <dbl> 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,…

## Data dictionary

Each row is one candidate’s vote total in one contest in one reporting
unit (a ward or group of wards). The same data is also available as a
Hive-partitioned parquet dataset in `processed-data/parquet/`
(partitioned by `year` / `month` / `office`).

### Coverage

Coverage is complete back to 2000 for some offices, while results for
other offices have yet to be added. Note shown in this table, but
nonetheless included in the data, is the June 2012 gubernatorial recall
election.

| year | FEBRUARY | APRIL | AUGUST | NOVEMBER |
|---:|:---|:---|:---|:---|
| 2000 | NA | JUSTICE OF THE SUPREME COURT | NA | CONGRESS, PRESIDENT, US SENATE |
| 2001 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2002 | NA | NA | NA | CONGRESS, GOVERNOR |
| 2003 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2004 | NA | NA | NA | CONGRESS, PRESIDENT, US SENATE |
| 2005 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2006 | NA | JUSTICE OF THE SUPREME COURT | NA | CONGRESS, GOVERNOR, US SENATE |
| 2007 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2008 | NA | JUSTICE OF THE SUPREME COURT | NA | CONGRESS, PRESIDENT |
| 2009 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2010 | NA | NA | NA | CONGRESS, GOVERNOR, STATE ASSEMBLY, STATE SENATE, US SENATE |
| 2011 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2012 | NA | PRESIDENT | NA | CONGRESS, PRESIDENT, STATE ASSEMBLY, STATE SENATE, US SENATE |
| 2013 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2014 | NA | NA | NA | CONGRESS, GOVERNOR, STATE ASSEMBLY, STATE SENATE |
| 2015 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2016 | NA | JUSTICE OF THE SUPREME COURT, PRESIDENT | CONGRESS, STATE ASSEMBLY, STATE SENATE, US SENATE | CONGRESS, PRESIDENT, STATE ASSEMBLY, STATE SENATE, US SENATE |
| 2017 | STATE SUPERINTENDENT OF PUBLIC INSTRUCTION | JUSTICE OF THE SUPREME COURT, STATE SUPERINTENDENT OF PUBLIC INSTRUCTION | NA | NA |
| 2018 | JUSTICE OF THE SUPREME COURT | JUSTICE OF THE SUPREME COURT | CONGRESS, GOVERNOR, STATE ASSEMBLY, STATE SENATE, US SENATE | CONGRESS, GOVERNOR, STATE ASSEMBLY, STATE SENATE, US SENATE |
| 2019 | NA | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2020 | NA | JUSTICE OF THE SUPREME COURT, PRESIDENT | CONGRESS, STATE ASSEMBLY, STATE SENATE | CONGRESS, PRESIDENT, STATE ASSEMBLY, STATE SENATE |
| 2021 | STATE SUPERINTENDENT OF PUBLIC INSTRUCTION | STATE SUPERINTENDENT OF PUBLIC INSTRUCTION | NA | NA |
| 2022 | NA | NA | CONGRESS, GOVERNOR, STATE ASSEMBLY, STATE SENATE, US SENATE | CONGRESS, GOVERNOR, STATE ASSEMBLY, STATE SENATE, US SENATE |
| 2023 | JUSTICE OF THE SUPREME COURT | JUSTICE OF THE SUPREME COURT | NA | NA |
| 2024 | NA | PRESIDENT | CONGRESS, REPRESENTATIVE IN CONGRESS DISTRICT 8 (SPECIAL), STATE ASSEMBLY, STATE SENATE, US SENATE | CONGRESS, PRESIDENT, STATE ASSEMBLY, STATE SENATE, US SENATE |
| 2025 | STATE SUPERINTENDENT OF PUBLIC INSTRUCTION | JUSTICE OF THE SUPREME COURT, STATE SUPERINTENDENT OF PUBLIC INSTRUCTION | NA | NA |
| 2026 | NA | JUSTICE OF THE SUPREME COURT | ATTORNEY GENERAL, CONGRESS, GOVERNOR, LIEUTENANT GOVERNOR, SECRETARY OF STATE, STATE ASSEMBLY, STATE SENATE, STATE TREASURER | NA |

### Columns

| column | type | description |
|----|----|----|
| mcd_fips | character | 10-digit Census FIPS code for the minor civil division (55 + 3-digit county + 5-digit MCD) |
| county_fips | character | 5-digit state + county FIPS (first 5 characters of `mcd_fips`). |
| county | character | County name, uppercase, standardized (e.g. `LA CROSSE`). |
| municipality | character | Municipality (MCD) name, uppercase and standardized, without the CITY/TOWN/VILLAGE prefix (e.g. `ADAMS`, `FONTANA-ON-GENEVA LAKE`). |
| ctv | character | Municipality type: `C` = city, `T` = town, `V` = village. |
| reporting_unit | character | Ward group within the municipality, e.g. `WARD 1`, `WARDS 1-3`. Filled with `WARD 1` when the source named no wards. The boundaries of wards with same name can change between years. |
| year | double | Election year, 2000–2026. |
| month | character | `FEBRUARY`, `APRIL`, `AUGUST`, or `NOVEMBER`; identifies the election type (see coverage table). |
| office | character | Contest name. Partisan district offices are collapsed to `CONGRESS`, `STATE SENATE`, `STATE ASSEMBLY` with the number in `district`; the one exception is `REPRESENTATIVE IN CONGRESS DISTRICT 8 (SPECIAL)` (August 2024). |
| party | character | Ballot party of the contest. In November generals, `SCATTERING` and `WRITE-IN` (plus `WRITE-IN 2` … `WRITE-IN 9`, `INDEPENDENT 2` … `INDEPENDENT 5`) appear as party values; numbered variants distinguish multiple write-in/independent candidates in one contest. |
| candidate | character | Candidate name, uppercase. Presidential tickets include both names (`KAMALA D. HARRIS TIM WALZ`). Write-in candidates may carry a `(WRITE-IN)` suffix. `SCATTERING` is a pseudo-candidate for write-in scattering votes. Source spelling is preserved including year-to-year variation for the same person. |
| votes | double | Votes received by the candidate in this reporting unit. |
| total_votes | double | Sum of all candidate votes (including SCATTERING) for this contest in this reporting unit. |
| district | double | District number for CONGRESS, STATE SENATE, and STATE ASSEMBLY contests; `0` for statewide and other offices (including the CD-8 special); NA for all February/April rows. Districts are those in effect for that election. |
| con_dist | double | Congressional district containing the reporting unit in that year. Only populated for August/November rows. |
| wss_dist | double | State senate district containing the reporting unit in that year. Populated for August/November rows. |
| wsa_dist | double | State assembly district containing the reporting unit in that year. Same coverage as `wss_dist`. |
