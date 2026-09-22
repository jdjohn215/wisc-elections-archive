This document explains how the geography in the election data can be used.

The source election data is at the reporting unit level. Reporting units are
voting tabulation districts--the lowest level at which votes are counted and
reported. Each reporting unit consists of one or more wards within a single
municipality and county, i.e. within a single minor civil division. Reporting
units cannot straddle political district boundaries, provided that political
district reports results during the given election. In other words, during
August and November elections, every reporting unit lies wholly within a single
state legislative or congressional district, but this is not necessarily true
during February and April elections.

There are 72 counties in the state. Their names and boundaries remain unchanged
throughout the entire dataset.

There are roughly 3,500 reporting units in the state. The exact number fluctuates
between elections. Ward boundaries change in substantial ways following statewide
redistricting, but they can also change between elections thanks to municipal
boundary changes. Also, in municipalities where multiple wards are combined to
form multi-ward reporting units, the reporting units may use different
combinations of wards from one election to another.

Minor civil division (MCD) and municipality boundaries may also change from one
election to another, reflecting municipal incorporation and annexation.

FIPS codes for municipalities, counties, and minor civil divisions uniquely
identify the geographies across time. For counties, the names are also the same
in every year. For municipalities and minor civil divisions, the name assigned
to a specific FIPS code can, on rare occasions, change. This is because a 
municipality that incorporates or changes its name but whose geography remains
substantially unchanged is allowed to keep its same FIPS code. In this scenario
the consistent FIPS code across name changes gives us important information that
this is substantially the same municipality.

For all these reasons, this election data should be understood as **nominally**
integrated, not **spatially** integrated. It is up to the reader to decide if
the boundaries of their geography of interest remained sufficiently the same
between their elections of interest.
