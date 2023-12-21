# wisc-elections-archive
This repository combines official Wisconsin election returns from selected elections into a single standardized dataset.

See [processed-data](processed-data) for the final output. Refer to [Codebook.md](Codebook.md) for details.

See [cleaning-scripts](cleaning-scripts) for documented and reproducible scripts translating the original source files in [original-data](original-data) into the final output.

--------------------------------------------------------------------------------

This data differs in several ways from the disaggregated ward returns published by the [Wisconsin Legislative Technology Services Bureau](https://gis-ltsb.hub.arcgis.com/) (LTSB). The LTSB files are spatially integrated, meaning that the LTSB uses statistical techniques in an effort to make geographically comparable units over time. These files have many uses, but they sacrifice some degree of accuracy. In contrast, this dataset contains the actual, official results as finalized by the State of Wisconsin.
