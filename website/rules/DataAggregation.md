This document contains rules for how the election data may be aggregated.

Data may be aggregated between geographies--not across election years.
Beginning with reporting unit election returns, all election data may be aggregated to:
- counties (`county_fips`). There are 72 counties and they are identical in every year.
- municipalities (`muni_fips`). Municipalities may cross county lines and may vary in name and number from year to year.
- minor civil divisions, or MCDs (`mcd_fips`). These are municipalities-within-counties. They may vary in name and number from year to year.

Data may also be aggregated by the `district` field. This column contains the
district number of the contest in question. Specifically, it contains values for
STATE ASSEMBLY, STATE SENATE, and CONGRESS races. In statewide elections,
`district == 0`. However, in these statewide elections, the results may be
aggregated into state legislative or congressional districts. In the data:
- state assembly district = `wsa_dist`
- state senate district = `wss_dist`
- congressional district = `con_dist`.
If a contest is not statewide, i.e. where `district != 0`, it may not be
aggregated into state legislative or congressional districts.

Data can only be aggregated within a given contest. The rules for identifying a
contest in the long format data are different between the kinds of elections.
- all elections in NOVEMBER are partisan general elections. Identify the contest
  by `year`, `month`, `mcd_fips`, `reporting_unit`, `office`, `district`
- all elections in AUGUST are partisan primary elections. Identify the contest
  by `year`, `month`, `mcd_fips`, `reporting_unit`, `office`, `district`, `party`
- all elections in FEBRUARY are statewide nonpartisan primary elections. Identify the contest
  by `year`, `month`, `mcd_fips`, `reporting_unit`, `office`
  - you can also use the values for `district` and `party` but these are identical
    for all offices
- elections in APRIL are either nonpartisan general elections or presidential
  primaries, which are partisan and occur simultaneously with the nonpartisan
  general election.
  - if `office == "PRESIDENT"`, identify the contest using `year`, `month`,
    `mcd_fips`, `reporting_unit`, `party`
  - if `office != "PRESIDENT"`, identify the contest using `year`, `month`,
    `mcd_fips`, `reporting_unit`, `office`