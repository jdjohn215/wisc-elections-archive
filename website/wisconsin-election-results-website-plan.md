# Wisconsin Election Results Website — Plan

## Purpose
A website for fast, direct-access retrieval of official, certified Wisconsin election results across many elections and geographic levels, with secondary support for visual/map exploration and bulk data download.

## Primary audience
People who already know the site and come looking for a specific result (e.g., "2024 President, City of Madison"), often wanting to then compare that result across other years for the same office. SEO/discoverability is not a design goal.

## Secondary audience
People who want to explore results visually (map) and then download underlying data for their own analysis.

## Scope decisions already made
- **No boundary reconciliation across elections.** Each election's official boundaries are shown as-is. It's on the user to judge whether geography is comparable across years. This keeps the data "official and certified" without an interpretive crosswalk layer.
- **Data scale is small.** ~2.16M rows, ~1,800 races, long format (1 row per candidate per race). This fits comfortably in a single Parquet file or a few, well under sizes that need a database server.
- **Boundary files are small.** Most detailed layer (precincts) tops out around 3,600 features per election — easily handled by vector tiles.
- **Fully static architecture, no backend.** All data (results + boundaries) shipped as static files; all querying happens client-side.

## Architecture

**App type:** Single-page app (SPA), not a static-site-generator/pre-rendered-pages approach — since users arrive with intent rather than via search engines, an app-shell-plus-instant-lookup pattern serves them better than thousands of pre-rendered pages.

**Data layer:**
- Full results dataset as Parquet, loaded into **DuckDB-WASM** in the browser on app load (one-time cost, likely a couple hundred MB, cached after).
- Loading the *entire* dataset up front (rather than lazy-loading per election) is the right tradeoff, because it makes cross-year comparisons and election-switching instant with no repeated network waits.

**Map layer:**
- GIS boundaries converted to vector tiles via **Tippecanoe**, packaged as **PMTiles** archives (one per election), served as static files.
- Rendered client-side with **MapLibre GL JS**.
- Map loads on demand per election when the user opens map view (boundaries don't need to be preloaded like the results data).

**Hosting:** Fully static file hosting (e.g., Cloudflare Pages/R2, Netlify, GitHub Pages + CDN, or S3+CloudFront) — no server compute required anywhere in this design.

## Core features

1. **Structured lookup** — Election → Race/Office → Geography level → Geography unit, each step filtering the next, as the primary interaction (rather than a search box).
2. **Instant results table** — winner, vote totals, margin, etc., rendered client-side from the already-loaded DuckDB-WASM dataset.
3. **Cross-year comparison** — from any result, an "add year" affordance lets the user pull in the same (normalized) office/geography combination from other elections, reflowing into a pivoted comparison table (years as columns) rather than stacking repeated tables. This is a core feature, not a stretch goal, and is only fast/pleasant because the full dataset is preloaded.
4. **Map view** — per-election choropleth (MapLibre + PMTiles), click a unit to jump to its result table.
5. **Table/download view** — full filterable table via DuckDB-WASM queries, with CSV export of whatever the user has filtered to. A bulk-download page for the full dataset or per-election extracts also serves power users directly.
6. **Bookmarkable state** — client-side routing (hash or history-based URLs) so a specific lookup can be linked/shared, without needing static pre-rendering.

## Known open item (parked, not urgent)
To make cross-year "same office" comparisons clean, a normalized office/category field (distinct from the as-certified race label) will make querying much simpler — worth building into the data prep if not already present, but this is a data-layer refinement, not a blocker for the overall architecture.

## Suggested build workflow

1. **Data prep**: Finalize long-format results as Parquet (consider per-election files for build/versioning convenience even though they'll be loaded together). Add normalized office/category field if not already present.
2. **Boundary prep**: Run Tippecanoe per election's boundary file → PMTiles archives, one per election.
3. **App shell**: Build SPA skeleton, wire up DuckDB-WASM to load the full Parquet dataset on startup with a loading indicator.
4. **Lookup interface**: Build the Election → Race → Geography drill-down and the resulting results table view.
5. **Comparison feature**: Add "expand to other years" on the results table, with the pivoted comparison table view.
6. **Map view**: Integrate MapLibre + PMTiles, choropleth styling, click-to-result-table interaction.
7. **Table/download view**: Full filterable table UI over DuckDB-WASM, CSV export; add a bulk-download page.
8. **Routing/bookmarking**: Client-side URL state for shareable lookups.
9. **Deploy**: Push static assets (app bundle + Parquet + PMTiles) to static hosting/CDN.
