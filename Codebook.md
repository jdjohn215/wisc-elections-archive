# Codebook - Standardized Wisconsin Elections Data

This resource includes the results of Wisconsin races for President, Governor, Senate, and the House of Representatives from the years 2000 through 2022 in a single standardized format. Results are preserved as published in the official election canvass, following any recounts.

The base file is `AllElections_ReportingUnit.csv`. It includes the following fields.

* mcd_fips: the 10-digit FIPS code for the minor civil division
* county_fips:  the 5-digit county FIPS code
* county: county name
* municipality: municipality name
* ctv: city, town, or village
* reporting_unit: the ward or wards making up the reporting unit
* year: fall general election
* office: one of PRESIDENT, GOVERNOR, CONGRESS, or SENATE
* district: the district in which the race was held, 0 indicates a statewide office
* con_dist: congressional district containing the reporting unit
* wss_dist: state senate district containing the reporting unit
* wsa_dist: state assembly district containing the reporting unit
* party: the party name which appeared on the ballot. In the case of an official write-in candidate, the party is recorded as "write-in," regardless of whether or not a party name is displayed in the canvass returns. When multiple independents or write-in candidates appeared on the ballot, they are recorded as "independent 2" or "write-in 2", etc.
* candidate: the candidate name as it appeared on the ballot. In some cases, this is two names.
* votes: the votes received by the candidate
* total_votes: the total number of votes cast for this office in this reporting unit

The data consists of three files corresponding to different geography levels.

## Geography

The basic unit is the "**reporting unit**." Reporting units are either single wards, or in smaller areas, combinations of wards. This is the level at which election results are actually tallied and reported. Any combined wards must all be within a single municipality and county and they must share all the same races on the ballot for the given fall general election. The same set of reporting units are used for each office *within* a given election, but they may differ *between* elections. Ward boundaries are usually (not always) stable in between redistricting cycles (i.e. 2002-2010, 2012-2020), but they can change completely during redistricting. Ward 1 of the Village of Anytown might correspond to a wholly different area in 2010 than in 2012. Municipal annexations occur routinely in Wisconsin, and wards can be added, subtracted, or redrawn to accomodate these changes at any time.

All of Wisconsin lies within a city, town, or village. These cannot overlap, but cities and villages can straddle county lines. Census data is often published at the level of municipalities-within-counties. These are called "**minor civil divisions**" or "county subdivisions." We provide a file with election results summarized by minor civil division named `AllElections_MinorCivilDivisions.csv`. For example, portions of the City of Wisconsin Dells lie within 4 different counties, and election results are presented separately for each in the minor civil division file.

We also aggregate minor civil divisions into complete **municipalities**. See `AllElections_Municipalities.csv`. Necessarily, this file does not contain a county code, as municipalities can cover multiple counties. We do include a "muni_fips" field, which is "55" (the Wisconsin state fips code) followed by the 5-digit fips which uniquely identifies municipalities.

### Geographical integration

Use the mcd_fips or muni_fips fields to track municipalities or minor civil divisions across time. These fields provide a value-added form of **nominal integration**. When a town incorporates as a village, it retains the same fips code, as the underlying unit remains fundamentally the same. New municipalities formed out of the consolidation of multiple former municipalities receive an entirely new fips code. When a city wholly annexes a nearby town, the fips code used by that town ceases to exist, but the city's fips code remains unchanged. 

## Technical notes

In 2002, the Town of Springfield in Marquette County was incorrectly assigned to senate district 14. I correct this to senate district 24. There was no state senate election in this district during 2002.

In 2016, Ward 11 in the City of Wisconsin Dells and Ward 48 in the City of Waukesha are both assigned to senate districts inconsistent with their state assembly district. However, both districts recorded 0 votes, so I leave this unchanged.

In 2004, the canvass returns show a ward of the Village of Mukwonago within Racine County, which recorded 0 votes. This only occurs in 2004, and I can find no evidence from other election or census data that Mukwonago ever stretched into Racine County. Because it recorded 0 votes and cannot be matched to a census minor civil division, I drop it from the final data.

In 2012, some reporting units appear in the records for two legislative districts. However, one of these duplicate entries always shows 0 voters, so I removed them from the final data. I removed the following 0-vote duplicate reporting unit records:

  * CITY OF SHEBOYGAN FALLS WARDS 1-2 & 9 in AD27
  * CITY OF SHEBOYGAN FALLS WARDS 3-5 in AD27
  * VILLAGE OF MONTFORT Ward 2 in AD51
  * TOWN OF DELMAR Ward 1 in AD68
  * CITY OF STANLEY Wards 1 - 4, 6 - 7 in AD67
  * TOWN OF EAGLE POINT Wards 1 - 5 & 5S in CD3
  * TOWN OF EDSON Wards 1 - 2 & 2 S in CD7
  