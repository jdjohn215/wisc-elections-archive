# Website development notes

Handoff document for continuing work on the Wisconsin election results
website. Written September 2026, after building v1 (one contest at a time;
map view + table view; reporting unit / MCD / municipality / county
geographies). The long-term vision is in
`website/wisconsin-election-results-website-plan.md`; nothing there is set in
stone, but v1 follows its static-architecture decision.

## What v1 does

- Fully static site in `website/app/` — no build step, no backend.
- Contest picker cascade: Election → Contest → Party (partisan primaries
  only) → District (districted races only). Defaults to November 2024 —
  President. Election and District are text-input + `<datalist>` search
  boxes (commit on exact label match via the `input` event); Contest and
  Party remain plain `<select>`s.
- **Map view**: choropleth at a user-selected geography (reporting unit,
  minor civil division, municipality, county) over a CARTO Positron
  (black-and-white, no API key) basemap; data layers are inserted below the
  basemap's first symbol layer so place/road labels draw above the fills.
  Fill = winner's party color,
  lightness interpolated toward white as the winner's margin over the
  runner-up shrinks (full color at ≥ 50-point margin). **Primary contests**
  (any contest with a party picker — August partisan primaries and April
  presidential preference primaries) and **nonpartisan spring contests**
  (where every named candidate's party is NONPARTISAN — detected as ≤ 1
  distinct party among named candidates, so legacy-schema years work too)
  instead color by **winning candidate**
  using ColorBrewer Dark2, assigned in descending contest-wide vote order to
  every candidate who won ≥ 1 displayed unit (same margin lightening on top;
  > 8 unit-winners wraps the palette). A legend in the lower-left maps each
  color to its candidate (one entry per candidate who won ≥ 1 displayed
  unit; generals label the party too). Click a unit for a popup listing
  every candidate with ≥ 1% of the contest-wide (statewide or districtwide)
  vote, even candidates with 0 votes in that unit. County outlines drawn on
  top of sub-county geographies. Districted contests zoom to the units with
  results.
- **Table view**: candidate/party/votes/share for the contest, statewide by
  default, scopeable to one county, one MCD, or one municipality via
  text-input + `<datalist>` search boxes (type to filter; the MCD picker is a
  single statewide search with the county in the label — no cascade). The
  option lists are **contest-scoped**: re-queried on every
  election/office/party/district change with `HAVING SUM(votes) > 0`, so
  only geographies with votes in the selected contest appear. Winner +
  margin summary line. SCATTERING rows shown (grayed) but excluded from
  winner/margin math.

## Running locally

```sh
cd website/app && python3 -m http.server 8765
# open http://localhost:8765
```

All libraries load from CDNs (pinned): maplibre-gl 4.7.1 (unpkg),
flatgeobuf 3.28.0 (unpkg UMD), @duckdb/duckdb-wasm 1.29.0 (jsdelivr `+esm`).

## Architecture

```
website/
  prep/
    prep-results.R       # processed-data/parquet/ -> app/data/results/*.parquet + contests.json
    prep-boundaries.R    # fgb/ -> app/data/geo/**.fgb (WGS84), counties.fgb, geo-manifest.json
  app/
    index.html           # selector UI + view toggle
    style.css
    main.js              # everything: DuckDB-WASM init, pickers, table, map
    data/
      contests.json              # one entry per election; flat contest list per DataAggregation.md rules
      results/<YEAR>-<MONTH>.parquet   # 47 files, ~11 MB total
      geo/
        counties.fgb             # built once by dissolving 2024 MCDs
        geo-manifest.json        # per-election available geographies + district vintages
        mcd/<YEAR>-<MONTH>.fgb   # all 47 elections
        muni/<YEAR>-<MONTH>.fgb
        ru/<YEAR>-<MONTH>.fgb    # 2016-APRIL onward only (25 elections; none for 2024-APRIL)
        con/<VINTAGE>.fgb        # district maps, one per vintage (e.g. 2012-2020.fgb),
        wsa/<VINTAGE>.fgb        # shared across elections; August/November only
        wss/<VINTAGE>.fgb
```

Data flow: on election change, the app fetches that election's parquet into
DuckDB-WASM (`registerFileBuffer`, view `results`); all table and map
numbers come from SQL against that view. On map render, the election's FGB
for the chosen geography is fetched and deserialized to GeoJSON client-side,
results are aggregated in DuckDB to the geography's join key, and colors are
computed per feature in JS (`properties.fill`, `['get','fill']` paint).

## Data conventions and quirks

- Results schema: `county_fips`, `muni_fips`, `mcd_fips` (all strings on the
  JS side; prep zero-pads), `county`, `municipality`, `ctv` (C/T/V),
  `reporting_unit`, `year`, `month`, `office`, `party`, `candidate`,
  `votes`, `total_votes`, `district`, `con_dist`, `wss_dist`, `wsa_dist`.
- `contests.json` `offices` must always be a JSON array: jsonlite's
  `auto_unbox = TRUE` writes a length-1 vector as a bare string, and the
  app's office picker then iterates it character-by-character (pickers full
  of `P`, `R`, `E`…). `prep-results.R` wraps it in `I()` to prevent this;
  `populateOffices()` also guards with `Array.isArray`. The `contests`
  field is immune (tibbles always serialize as arrays of objects).
- Contest identity (`contests.json`, per `website/rules/DataAggregation.md`):
  `party` is non-null only for August partisan primaries and April
  presidential preference primaries (office == PRESIDENT in April).
  `district` is 0 for statewide offices; districted offices get a district
  picker. June 2012 is the recall general (partisan, party is a candidate
  attribute, not part of contest identity).
- SCATTERING is `candidate == 'SCATTERING'`; its `party` varies (SCATTERING
  in legacy years, the contest's party in new-schema years). Always exclude
  from winner/margin, keep in tables.
- Vote share / margin denominators: sum of **named** (non-SCATTERING)
  candidate votes in the unit.
- Winner/margin need ≥ 2 named candidates; single-candidate units get full
  color and "(unopposed)" in tables.
- Reporting-unit join key: `mcd_fips + '|' + upper(reporting_unit)`;
  county/MCD/muni join on their fips alone. District geographies
  (`con`/`wss`/`wsa`, offered only for August/November elections) join on the
  results' `<geo>_dist` column against the same column in the vintage FGB; the
  manifest's per-election `districts` map (`{con: "2022-2030", ...}`) tells
  the app which vintage file to load, and `{}` for non-fall elections. The
  SQL adds `AND <geo>_dist > 0` as a null/zero guard. (The `fgb/con/`
  filenames were briefly mislabeled relative to content and `fgb/wss/2022.fgb`
  was missing due to an `fgb/ws/` typo in `gis/build-legis.R`; both were
  fixed at the source in Sept 2026, and prep now copies all district files
  verbatim.)
- Some August-year candidate names contain double spaces (`ERIC  HOVDE`,
  `BRAD  SCHIMEL`) — kept as-is per the repo's minimal-munging convention.
- RU boundary coverage misses 0–11 units per election; verified all are
  zero-vote wards (brand-new wards absent from the census ward geography the
  RU layer was built from). They simply don't render on the map.
- Unit display names come from a per-election
  `SELECT DISTINCT county_fips, county, mcd_fips, muni_fips, municipality,
  ctv` query, not from the boundary files (MCD/muni FGBs carry only fips).

## Hard-won debugging lessons (don't regress these)

1. **DuckDB-WASM file loading**: `registerFileURL(..., HTTP, ...)` fails from
   the blob-wrapped worker (`InvalidStateError: ... no longer usable`).
   Fetch the parquet on the main thread and use `registerFileBuffer`.
2. **CRS**: the source reporting-unit FGBs in `fgb/reporting-units/` are in
   NAD83(HARN) / Wisconsin Transverse Mercator. MapLibre silently renders
   them off-screen (blank gray map). `prep-boundaries.R` reprojects
   everything to EPSG:4326 as it copies; all files under `app/data/geo/`
   must be WGS84 (bbox ≈ -92.9…-86.8, 42.5…47.1).
3. **flatgeobuf UMD API**: the `flatgeobuf-geojson.min.js` bundle exposes
   `deserialize` directly on the `flatgeobuf` global —
   `flatgeobuf.deserialize(bytes)`, NOT `flatgeobuf.geojson.deserialize`.
   With a `Uint8Array` it returns a FeatureCollection synchronously; with a
   stream/URL it returns an async iterable. `fgbToGeoJSON()` in main.js
   handles both.
4. The DuckDB-WASM worker is wrapped in a same-origin blob
   (`importScripts` of the jsdelivr worker URL) because cross-origin Worker
   construction is blocked.
5. Render errors are surfaced in the `#loading` box; keep it that way —
   silent catch-and-log made the first bugs much harder to diagnose.
6. **Join-key types**: the fips columns are strings in the parquets, but
   `con_dist`/`wss_dist`/`wsa_dist` are doubles, so DuckDB-WASM returns
   district aggregation keys as JS numbers while `featureKey()` stringifies
   the FGB properties — the `Map` lookup then never matches and every
   district renders gray with no zoom. The tell: the legend still populates
   (it's built from query rows, not matched features). `renderMap`
   stringifies the query key (`String(r.key)`) when building the unit map;
   keep both sides of every geography join as strings.

## Sanity-check reference numbers (certified)

Use these to verify the app after any change:

| Contest | Expected |
|---|---|
| 2024 Nov President, statewide | Trump 1,697,626 / Harris 1,668,229 |
| 2024 Nov President, Dane County | Harris 273,995 / Trump 85,454 |
| 2024 Aug US Senate, Republican primary | Hovde 477,197 |
| 2025 Apr Supreme Court, statewide | Crawford 1,301,137 / Schimel 1,062,330 |

## Rerunning the prep scripts

Run from the repo root in R, in order:

```r
source("website/prep/prep-results.R")      # needs arrow, tidyverse, jsonlite
source("website/prep/prep-boundaries.R")   # also needs sf
```

`prep-results.R` reads `processed-data/parquet/` (the combined 2000–2026
dataset). `prep-boundaries.R` reads `fgb/{mcd,muni,reporting-units}/`,
validates that every results join key finds a boundary feature, and prints a
per-election coverage table. Two session gotchas seen while developing:
`dir.create()` here doesn't accept a vector of paths (loop instead), and
this R session had a stray `options(error=...)` pointing at a tigris call
that masked real error messages — if errors look unrelated to your code,
check `options("error")` and `traceback()` skeptically.

## Known limitations / next steps (user's list, to be tackled one by one)

- Map polish: review MCD
  boundary simplification (v1 does **no**
  simplification — shapes come straight from `gis/`; if they look
  oversimplified, that's upstream in the GIS pipeline), plus assorted
  rendering bugs to be identified.
- Reporting-unit geography unavailable before April 2016 and for April 2024
  (no source layer).
- Not yet built (from the long-term plan): cross-year comparison, CSV
  download, bookmarkable URLs, PMTiles if FGB performance ever becomes a
  problem. District geographies exist only for August/November (the spring
  results lack `con_dist`/`wss_dist`/`wsa_dist`).
