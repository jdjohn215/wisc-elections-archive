'use strict';

// Wisconsin Election Results — static SPA.
// Data: per-election parquet via DuckDB-WASM; boundaries via FlatGeobuf.

const DUCKDB_VERSION = '1.29.0';

const PARTY_COLORS = {
  DEMOCRATIC: [33, 102, 172],
  REPUBLICAN: [178, 24, 43],
  'WISCONSIN GREEN': [27, 120, 55],
  LIBERTARIAN: [184, 134, 11],
  CONSTITUTION: [106, 61, 154],
  NONPARTISAN: [69, 117, 180],
};
const DEFAULT_COLOR = [117, 117, 117];

// ColorBrewer Dark2 — per-candidate colors for primary contests, assigned in
// descending order of contest-wide votes to candidates who won >= 1 unit.
const DARK2 = [
  [27, 158, 119], [217, 95, 2], [117, 112, 179], [231, 41, 138],
  [102, 166, 30], [230, 171, 2], [166, 118, 29], [102, 102, 102],
];

const CTV_WORD = { C: 'city', T: 'town', V: 'village' };
const GEO_LABELS = {
  county: 'County', mcd: 'Minor civil division', muni: 'Municipality',
  ru: 'Reporting unit', con: 'Congressional district',
  wss: 'State senate district', wsa: 'State assembly district',
};
// District geographies (August/November elections only): boundaries are
// per-vintage files named in the geo manifest's `districts` map; the results
// join key is the <geo>_dist column.
const DIST_LABELS = { con: 'Congressional District', wss: 'State Senate District', wsa: 'State Assembly District' };

let duckdb, db, conn;
let elections = [];
let electionByLabel = new Map(); // picker label -> election key
let districtByLabel = new Map(); // picker label -> district number
let geoManifest = {};
let districtFiles = {}; // election key -> {con|wss|wsa -> vintage file stem}
let unitNames = null;
let scopeUnits = null; // contest-scoped geography lists for the table pickers
let scopeToken = 0;
let map = null;
let mapReady = false;
const geoCache = new Map();
const unitDataCache = new Map(); // contest+geo -> {key -> rows}
let renderToken = 0;

const state = {
  election: null,
  office: null,
  party: null,
  district: null,
  view: 'map',
  geography: 'ru',
  scope: { level: 'statewide', county_fips: null, mcd_fips: null, muni_fips: null },
};

const $ = (id) => document.getElementById(id);

// ---------------------------------------------------------------- helpers

function showLoading(on) {
  if (on) $('loading').textContent = 'Loading\u2026';
  $('loading').hidden = !on;
}

function esc(s) { return String(s).replace(/'/g, "''"); }

function titleCase(s) {
  return String(s).toLowerCase().replace(/\b[a-z]/g, (c) => c.toUpperCase());
}

function fmt(n) { return Number(n).toLocaleString('en-US'); }

async function q(sql) {
  const res = await conn.query(sql);
  return res.toArray().map((r) => r.toJSON());
}

function electionKey(elec) { return `${elec.year}-${elec.month}`; }

function contestWhere() {
  let w = `office = '${esc(state.office)}'`;
  if (state.party) w += ` AND party = '${esc(state.party)}'`;
  if (state.district !== null && state.district !== undefined) w += ` AND district = ${state.district}`;
  return w;
}

// ---------------------------------------------------------------- setup

async function initDuckDB() {
  duckdb = await import(`https://cdn.jsdelivr.net/npm/@duckdb/duckdb-wasm@${DUCKDB_VERSION}/+esm`);
  const bundle = await duckdb.selectBundle(duckdb.getJsDelivrBundles());
  // Worker must be same-origin: wrap the CDN script in a blob
  const workerUrl = URL.createObjectURL(
    new Blob([`importScripts("${bundle.mainWorker}");`], { type: 'text/javascript' })
  );
  const worker = new Worker(workerUrl);
  db = new duckdb.AsyncDuckDB(new duckdb.ConsoleLogger(duckdb.LogLevel.WARNING), worker);
  await db.instantiate(bundle.mainModule, bundle.pthreadWorker);
  conn = await db.connect();
}

async function loadElectionData(elec) {
  const key = electionKey(elec);
  const fname = `results_${key}.parquet`;
  // Buffer the file from the main thread; registerFileURL over HTTP fails
  // inside the blob-wrapped worker (InvalidStateError)
  const buf = await fetch(`data/results/${key}.parquet`).then((r) => {
    if (!r.ok) throw new Error(`Failed to fetch results for ${key} (HTTP ${r.status})`);
    return r.arrayBuffer();
  });
  await db.registerFileBuffer(fname, new Uint8Array(buf));
  await conn.query(`CREATE OR REPLACE VIEW results AS SELECT * FROM parquet_scan('${fname}')`);

  const rows = await q(
    `SELECT DISTINCT county_fips, county, mcd_fips, muni_fips, municipality, ctv FROM results`
  );
  // Election-wide name lookup, used for map labels/popups and scopeLabel().
  // The table-scope pickers use the contest-scoped scopeUnits instead.
  const names = { counties: new Map(), mcd: new Map(), muni: new Map() };
  for (const r of rows) {
    const ctv = CTV_WORD[r.ctv] || String(r.ctv).toLowerCase();
    names.counties.set(r.county_fips, r.county);
    names.mcd.set(r.mcd_fips, { name: r.municipality, ctv, countyFips: r.county_fips, county: r.county });
    if (!names.muni.has(r.muni_fips)) {
      names.muni.set(r.muni_fips, { name: r.municipality, ctv, counties: new Set() });
    }
    names.muni.get(r.muni_fips).counties.add(r.county);
  }
  unitNames = names;
}

// Contest-scoped geography lists for the table-scope pickers: only units
// with at least one vote in the selected contest (SUM(votes) > 0), so e.g.
// a districted race offers only the counties/MCDs/munis in that district.
// Separate from unitNames, which stays election-wide for map labels.
async function refreshScopeUnits() {
  const token = ++scopeToken;
  const rows = await q(
    `SELECT county_fips, county, mcd_fips, muni_fips, municipality, ctv
     FROM results
     WHERE ${contestWhere()}
     GROUP BY county_fips, county, mcd_fips, muni_fips, municipality, ctv
     HAVING SUM(votes) > 0`
  );
  if (token !== scopeToken) return;
  const su = {
    countyList: [], mcdList: [], muniList: [],
    byLabel: { county: new Map(), mcd: new Map(), muni: new Map() },
  };
  const muniAcc = new Map(); // fips -> { name, ctv, counties: Set }
  for (const r of rows) {
    const ctv = CTV_WORD[r.ctv] || String(r.ctv).toLowerCase();
    su.byLabel.county.set(`${titleCase(r.county)} County`, r.county_fips);
    su.byLabel.mcd.set(`${titleCase(r.municipality)} ${ctv}, ${titleCase(r.county)} County`, r.mcd_fips);
    if (!muniAcc.has(r.muni_fips)) {
      muniAcc.set(r.muni_fips, { name: r.municipality, ctv, counties: new Set() });
    }
    muniAcc.get(r.muni_fips).counties.add(r.county);
  }
  for (const [fips, d] of muniAcc) {
    su.byLabel.muni.set(
      `${titleCase(d.name)} ${d.ctv} (${[...d.counties].map(titleCase).join('/')})`, fips
    );
  }
  const toSortedList = (m) => [...m.entries()].map(([label, fips]) => ({ label, fips }))
    .sort((a, b) => a.label.localeCompare(b.label));
  su.countyList = toSortedList(su.byLabel.county);
  su.mcdList = toSortedList(su.byLabel.mcd);
  su.muniList = toSortedList(su.byLabel.muni);
  scopeUnits = su;
}

// ---------------------------------------------------------------- selectors

function fillSelect(sel, items, labelFn, valueFn) {
  sel.innerHTML = '';
  for (const item of items) {
    const opt = document.createElement('option');
    opt.value = valueFn(item);
    opt.textContent = labelFn(item);
    sel.appendChild(opt);
  }
}

function populateElections() {
  electionByLabel = new Map(elections.map((e) => [e.label, electionKey(e)]));
  fillDatalist('election-list', elections.map((e) => ({ label: e.label })));
  const def = elections.find((e) => e.year === 2024 && e.month === 'NOVEMBER') || elections[0];
  $('election').value = def.label;
}

function populateOffices() {
  // guard against a bare string (jsonlite auto_unbox of a length-1 vector):
  // iterating a string would fill the picker with single characters
  const offices = Array.isArray(state.election.offices)
    ? state.election.offices : [state.election.offices];
  fillSelect($('office'), offices, (o) => titleCase(o), (o) => o);
  state.office = offices[0];
}

function updatePartyDistrict() {
  const contests = state.election.contests.filter((c) => c.office === state.office);
  const parties = [...new Set(contests.map((c) => c.party).filter((p) => p != null))].sort();

  $('party-label').hidden = parties.length === 0;
  if (parties.length) {
    fillSelect($('party'), parties, (p) => titleCase(p), (p) => p);
    $('party').value = parties.includes(state.party) ? state.party : parties[0];
    state.party = $('party').value;
  } else {
    state.party = null;
  }

  const partyContests = state.party ? contests.filter((c) => c.party === state.party) : contests;
  const districts = [...new Set(partyContests.map((c) => c.district))].sort((a, b) => a - b);
  const needsDistrict = districts.length > 1 || (districts.length === 1 && districts[0] !== 0);
  $('district-label').hidden = !needsDistrict;
  if (needsDistrict) {
    districtByLabel = new Map(districts.map((d) => [`District ${d}`, d]));
    fillDatalist('district-list', districts.map((d) => ({ label: `District ${d}` })));
    state.district = districts[0];
    $('district').value = `District ${districts[0]}`;
  } else {
    state.district = null;
  }
}

function updateGeographyOptions() {
  const avail = geoManifest[electionKey(state.election)] || ['county'];
  for (const opt of $('geography').options) {
    opt.disabled = !avail.includes(opt.value);
  }
  if (!avail.includes(state.geography)) {
    state.geography = avail.includes('ru') ? 'ru' : avail[avail.length - 1];
  }
  $('geography').value = state.geography;
}

// ---------------------------------------------------------------- table view

function fillDatalist(id, list) {
  const el = $(id);
  el.innerHTML = '';
  for (const it of list) {
    const opt = document.createElement('option');
    opt.value = it.label;
    el.appendChild(opt);
  }
}

// Point the search input at the stored fips if it's still valid for this
// contest; otherwise default to the first entry (matches the old <select>
// behavior of always having a selection).
function syncScopeInput(kind) {
  const list = scopeUnits[`${kind}List`];
  const fipsKey = `${kind}_fips`;
  let item = list.find((it) => it.fips === state.scope[fipsKey]);
  if (!item) {
    item = list[0] || null;
    state.scope[fipsKey] = item ? item.fips : null;
  }
  $(`scope-${kind}`).value = item ? item.label : '';
}

function updateScopeControls() {
  if (!scopeUnits) return;
  const lvl = state.scope.level;
  $('scope-county-label').hidden = lvl !== 'county';
  $('scope-mcd-label').hidden = lvl !== 'mcd';
  $('scope-muni-label').hidden = lvl !== 'muni';

  if (lvl === 'county') { fillDatalist('scope-county-list', scopeUnits.countyList); syncScopeInput('county'); }
  if (lvl === 'mcd') { fillDatalist('scope-mcd-list', scopeUnits.mcdList); syncScopeInput('mcd'); }
  if (lvl === 'muni') { fillDatalist('scope-muni-list', scopeUnits.muniList); syncScopeInput('muni'); }
}

function scopeWhere() {
  const s = state.scope;
  if (s.level === 'county' && s.county_fips) return ` AND county_fips = '${s.county_fips}'`;
  if (s.level === 'mcd' && s.mcd_fips) return ` AND mcd_fips = '${s.mcd_fips}'`;
  if (s.level === 'muni' && s.muni_fips) return ` AND muni_fips = '${s.muni_fips}'`;
  return '';
}

function scopeLabel() {
  const s = state.scope;
  if (s.level === 'county') return `${titleCase(unitNames.counties.get(s.county_fips) || '')} County`;
  if (s.level === 'mcd') {
    const d = unitNames.mcd.get(s.mcd_fips);
    return d ? `${titleCase(d.name)} ${d.ctv}, ${titleCase(d.county)} County` : '';
  }
  if (s.level === 'muni') {
    const d = unitNames.muni.get(s.muni_fips);
    return d ? `${titleCase(d.name)} ${d.ctv}` : '';
  }
  return 'Statewide';
}

function summarizeRows(rows) {
  const named = rows.filter((r) => r.candidate !== 'SCATTERING').sort((a, b) => b.votes - a.votes);
  const total = rows.reduce((acc, r) => acc + Number(r.votes), 0);
  return { named, total };
}

async function renderTable(token) {
  const rows = await q(
    `SELECT candidate, party, SUM(votes) AS votes FROM results
     WHERE ${contestWhere()}${scopeWhere()}
     GROUP BY candidate, party`
  );
  if (token !== renderToken) return;

  const { named, total } = summarizeRows(rows);
  const scopeTxt = scopeLabel();
  let summary = `<span class="scope">${scopeTxt}</span> — `;
  if (named.length >= 2) {
    const margin = named[0].votes - named[1].votes;
    const pct = total ? (100 * margin / total).toFixed(1) : '0.0';
    summary += `<span class="winner">${titleCase(named[0].candidate)}</span> wins by ${fmt(margin)} votes (${pct} pts)`;
  } else if (named.length === 1) {
    summary += `<span class="winner">${titleCase(named[0].candidate)}</span> (unopposed)`;
  } else {
    summary += 'no votes recorded';
  }
  $('table-summary').innerHTML = summary;

  const sorted = [...named, ...rows.filter((r) => r.candidate === 'SCATTERING')];
  let html = '<table class="results"><thead><tr>' +
    '<th>Candidate</th><th>Party</th><th class="num">Votes</th><th class="num">%</th>' +
    '</tr></thead><tbody>';
  for (const r of sorted) {
    const pct = total ? (100 * Number(r.votes) / total).toFixed(1) : '0.0';
    const cls = r.candidate === 'SCATTERING' ? ' class="scattering"' : '';
    html += `<tr${cls}><td>${titleCase(r.candidate)}</td><td>${titleCase(r.party)}</td>` +
      `<td class="num">${fmt(r.votes)}</td><td class="num">${pct}</td></tr>`;
  }
  html += `<tr><td><strong>Total</strong></td><td></td><td class="num"><strong>${fmt(total)}</strong></td><td></td></tr>`;
  html += '</tbody></table>';
  $('table-container').innerHTML = html;
}

// ---------------------------------------------------------------- map view

async function fgbToGeoJSON(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`Failed to fetch ${url} (HTTP ${res.status})`);
  const bytes = new Uint8Array(await res.arrayBuffer());
  // the UMD geojson bundle exposes deserialize directly on `flatgeobuf`;
  // with a Uint8Array it returns a FeatureCollection, with a stream/URL an
  // async iterable — handle both forms
  const out = flatgeobuf.deserialize(bytes);
  if (out && typeof out[Symbol.asyncIterator] === 'function') {
    const fc = { type: 'FeatureCollection', features: [] };
    for await (const f of out) fc.features.push(f);
    return fc;
  }
  return await out;
}

async function loadGeo(key, geo) {
  const cacheKey = `${key}|${geo}`;
  if (!geoCache.has(cacheKey)) {
    const vintage = (districtFiles[key] || {})[geo];
    const url = geo === 'county' ? 'data/geo/counties.fgb'
      : vintage ? `data/geo/${geo}/${vintage}.fgb`
      : `data/geo/${geo}/${key}.fgb`;
    geoCache.set(cacheKey, await fgbToGeoJSON(url));
  }
  return geoCache.get(cacheKey);
}

function featureKey(f, geo) {
  const p = f.properties;
  if (geo === 'county') return String(p.county_fips);
  if (geo === 'mcd') return String(p.mcd_fips);
  if (geo === 'muni') return String(p.muni_fips);
  if (geo in DIST_LABELS) return String(p[`${geo}_dist`]);
  return `${p.mcd_fips}|${String(p.reporting_unit).toUpperCase()}`;
}

function unitLabel(key, geo) {
  if (geo in DIST_LABELS) return `${DIST_LABELS[geo]} ${key}`;
  if (!unitNames) return key;
  if (geo === 'county') return `${titleCase(unitNames.counties.get(key) || key)} County`;
  if (geo === 'mcd') {
    const d = unitNames.mcd.get(key);
    return d ? `${titleCase(d.name)} ${d.ctv} (${titleCase(d.county)} Co.)` : key;
  }
  if (geo === 'muni') {
    const d = unitNames.muni.get(key);
    return d ? `${titleCase(d.name)} ${d.ctv}` : key;
  }
  const [mcdFips, ru] = key.split('|');
  const d = unitNames.mcd.get(mcdFips);
  const mcdTxt = d ? `${titleCase(d.name)} ${d.ctv}` : mcdFips;
  return `${mcdTxt} — ${titleCase(ru || '')}`;
}

function shadeColor(base, margin) {
  const t = Math.min(margin / 0.5, 1); // full color at >= 50-point margin
  const lighten = 0.82 * (1 - t);
  const rgb = base.map((c) => Math.round(c + (255 - c) * lighten));
  return `rgb(${rgb.join(',')})`;
}

function renderLegend(entries) {
  const el = $('legend');
  if (!entries.length) { el.hidden = true; return; }
  el.innerHTML = entries.map((e) =>
    `<div class="legend-item"><span class="legend-swatch" style="background:${e.color}"></span>${e.label}</div>`
  ).join('') + '<div class="legend-note">Darker = wider margin</div>';
  el.hidden = false;
}

function initMap() {
  map = new maplibregl.Map({
    container: 'map',
    // CARTO Positron: light black-and-white basemap, free without an API key.
    // Data layers are inserted below its first symbol layer (see renderMap),
    // so basemap geography sits under the choropleth but labels sit above it.
    style: 'https://basemaps.cartocdn.com/gl/positron-gl-style/style.json',
    center: [-89.6, 44.7],
    zoom: 5.8,
  });
  map.addControl(new maplibregl.NavigationControl(), 'top-right');
  map.on('load', () => { mapReady = true; render(); });

  map.on('click', 'units-fill', (e) => {
    const f = e.features && e.features[0];
    if (!f) return;
    const data = unitDataCache.get('current');
    const rows = data && data.units.get(f.properties._key);
    let html = `<strong>${unitLabel(f.properties._key, state.geography)}</strong>`;
    if (rows && rows.named.length) {
      // every candidate with >= 1% of the contest-wide vote, even if 0 here
      const votesByCand = new Map(rows.named.map((r) => [r.candidate, Number(r.votes)]));
      html += '<table>';
      for (const c of data.popupCands) {
        const v = votesByCand.get(c) || 0;
        const pct = rows.namedTotal ? (100 * v / rows.namedTotal).toFixed(1) : '0.0';
        html += `<tr><td>${titleCase(c)}</td><td style="text-align:right;padding-left:8px">${fmt(v)}</td><td style="text-align:right;padding-left:8px">${pct}%</td></tr>`;
      }
      html += '</table>';
    } else {
      html += '<br>No votes recorded';
    }
    new maplibregl.Popup().setLngLat(e.lngLat).setHTML(html).addTo(map);
  });
  map.on('mouseenter', 'units-fill', () => { map.getCanvas().style.cursor = 'pointer'; });
  map.on('mouseleave', 'units-fill', () => { map.getCanvas().style.cursor = ''; });
}

async function renderMap(token) {
  const key = electionKey(state.election);
  const geo = state.geography;

  const isDist = geo in DIST_LABELS;
  const keyExpr = geo === 'ru' ? "mcd_fips || '|' || upper(reporting_unit)"
    : isDist ? `${geo}_dist`
    : `${geo}_fips`;
  const [fc, rows] = await Promise.all([
    loadGeo(key, geo),
    q(`SELECT ${keyExpr} AS key,
              candidate, party, SUM(votes) AS votes
       FROM results WHERE ${contestWhere()}${isDist ? ` AND ${geo}_dist > 0` : ''}
       GROUP BY key, candidate, party`),
    geo === 'county' ? Promise.resolve(null) : loadGeo(key, 'county'),
  ]);
  if (token !== renderToken) return;

  // aggregate to winner + margin per unit
  const byUnit = new Map();
  for (const r of rows) {
    // district columns are doubles in the parquet; feature keys are strings
    const k = String(r.key);
    if (!byUnit.has(k)) byUnit.set(k, []);
    byUnit.get(k).push(r);
  }
  const unitData = new Map();
  for (const [k, list] of byUnit) {
    const named = list.filter((r) => r.candidate !== 'SCATTERING').sort((a, b) => b.votes - a.votes);
    const namedTotal = named.reduce((a, r) => a + Number(r.votes), 0);
    unitData.set(k, { named, namedTotal });
  }
  // Contest-wide named totals (contestWhere already scopes to the district
  // when relevant): used for the popup's >=1% threshold, the legend, and
  // per-candidate color assignment in primaries.
  const contestTotals = new Map();
  const candParty = new Map();
  const winners = new Set();
  let contestNamedTotal = 0;
  for (const d of unitData.values()) {
    for (const r of d.named) {
      contestTotals.set(r.candidate, (contestTotals.get(r.candidate) || 0) + Number(r.votes));
      candParty.set(r.candidate, r.party);
      contestNamedTotal += Number(r.votes);
    }
    if (d.named.length && d.namedTotal > 0) winners.add(d.named[0].candidate);
  }
  const byVotesDesc = (a, b) => (contestTotals.get(b) || 0) - (contestTotals.get(a) || 0);

  unitDataCache.set('current', {
    units: unitData,
    popupCands: [...contestTotals.keys()]
      .filter((c) => contestNamedTotal > 0 && contestTotals.get(c) / contestNamedTotal >= 0.01)
      .sort(byVotesDesc),
  });

  // Color by candidate when party can't distinguish them: primaries
  // (state.party set) and contests where every named candidate shares one
  // party — the nonpartisan spring races (all NONPARTISAN). Only candidates
  // who won >= 1 displayed unit get a Dark2 color, assigned in descending
  // contest-wide vote order.
  const singleParty = new Set(candParty.values()).size <= 1;
  const colorByCandidate = !!state.party || singleParty;
  let candColors = null;
  if (colorByCandidate) {
    const ranked = [...winners].sort(byVotesDesc);
    candColors = new Map(ranked.map((c, i) => [c, DARK2[i % DARK2.length]]));
  }

  renderLegend([...winners].sort(byVotesDesc).map((c) => ({
    label: colorByCandidate ? titleCase(c) : `${titleCase(c)} (${titleCase(candParty.get(c))})`,
    color: `rgb(${(candColors ? candColors.get(c) : PARTY_COLORS[candParty.get(c)] || DEFAULT_COLOR).join(',')})`,
  })));

  for (const f of fc.features) {
    const k = featureKey(f, geo);
    f.properties._key = k;
    const d = unitData.get(k);
    if (d && d.named.length && d.namedTotal > 0) {
      const margin = d.named.length > 1 ? (d.named[0].votes - d.named[1].votes) / d.namedTotal : 1;
      const base = candColors
        ? candColors.get(d.named[0].candidate) || DEFAULT_COLOR
        : PARTY_COLORS[d.named[0].party] || DEFAULT_COLOR;
      f.properties.fill = shadeColor(base, margin);
      f.properties.fillOpacity = 0.85;
    } else {
      // no results for this unit — show enough basemap through to orient
      f.properties.fill = '#e8e8e8';
      f.properties.fillOpacity = 0.3;
    }
  }

  const src = map.getSource('units');
  if (src) {
    src.setData(fc);
  } else {
    // Insert data layers just below the basemap's first symbol layer so
    // place/road labels render above the choropleth.
    const firstSymbol = map.getStyle().layers.find((l) => l.type === 'symbol');
    const beforeId = firstSymbol && firstSymbol.id;
    map.addSource('units', { type: 'geojson', data: fc });
    map.addLayer({
      id: 'units-fill', type: 'fill', source: 'units',
      paint: { 'fill-color': ['get', 'fill'], 'fill-opacity': ['get', 'fillOpacity'] },
    }, beforeId);
    map.addLayer({
      id: 'units-line', type: 'line', source: 'units',
      paint: { 'line-color': '#ffffff', 'line-width': 0.4 },
    }, beforeId);
    map.addSource('county-lines', { type: 'geojson', data: { type: 'FeatureCollection', features: [] } });
    map.addLayer({
      id: 'county-lines', type: 'line', source: 'county-lines',
      paint: { 'line-color': '#888888', 'line-width': 1.0 },
    }, beforeId);
  }
  if (geo !== 'county') {
    map.getSource('county-lines').setData(await loadGeo(key, 'county'));
  } else {
    map.getSource('county-lines').setData({ type: 'FeatureCollection', features: [] });
  }

  // zoom to units that have results (a district, for districted contests)
  const bounds = new maplibregl.LngLatBounds();
  let found = false;
  for (const f of fc.features) {
    if (!unitData.has(f.properties._key)) continue;
    const walk = (coords) => {
      if (typeof coords[0] === 'number') { bounds.extend(coords); found = true; }
      else coords.forEach(walk);
    };
    walk(f.geometry.coordinates);
  }
  if (found) map.fitBounds(bounds, { padding: 25, maxZoom: 11 });
}

// ---------------------------------------------------------------- render

async function render() {
  if (!state.election || !state.office) return;
  if (state.view === 'map' && !mapReady) return; // map load handler re-renders
  const token = ++renderToken;
  showLoading(true);
  try {
    if (state.view === 'map') await renderMap(token);
    else await renderTable(token);
    if (token === renderToken) showLoading(false);
  } catch (err) {
    console.error(err);
    if (token === renderToken) $('loading').textContent = 'Error: ' + err.message;
  }
}

async function onElectionChange() {
  // 'input' fires per keystroke; only a full label is a committed selection
  const key = electionByLabel.get($('election').value);
  if (!key) return;
  if (state.election && electionKey(state.election) === key) return;
  showLoading(true);
  state.election = elections.find((e) => electionKey(e) === key);
  await loadElectionData(state.election);
  populateOffices();
  updatePartyDistrict();
  updateGeographyOptions();
  await refreshScopeUnits();
  updateScopeControls();
  render();
}

// Scope pickers depend on the contest (which units have votes), so any
// contest change re-queries them before re-rendering.
async function onContestChange() {
  await refreshScopeUnits();
  updateScopeControls();
  render();
}

function setView(view) {
  state.view = view;
  $('view-map').classList.toggle('active', view === 'map');
  $('view-table').classList.toggle('active', view === 'table');
  $('map-controls').hidden = view !== 'map';
  $('table-controls').hidden = view !== 'table';
  $('map').style.visibility = view === 'map' ? 'visible' : 'hidden';
  $('legend').hidden = view !== 'map';
  $('table-view').hidden = view !== 'table';
  if (view === 'map' && map) map.resize();
  render();
}

// ---------------------------------------------------------------- boot

async function boot() {
  showLoading(true);
  let manifestList;
  [elections, manifestList] = await Promise.all([
    fetch('data/contests.json').then((r) => r.json()),
    fetch('data/geo/geo-manifest.json').then((r) => r.json()),
  ]);
  geoManifest = Object.fromEntries(manifestList.map((m) => [m.key, m.geographies]));
  districtFiles = Object.fromEntries(manifestList.map((m) => [m.key, m.districts || {}]));
  await initDuckDB();
  populateElections();
  initMap();

  $('election').addEventListener('input', onElectionChange);
  $('office').addEventListener('change', () => { state.office = $('office').value; updatePartyDistrict(); onContestChange(); });
  $('party').addEventListener('change', () => { state.party = $('party').value; updatePartyDistrict(); onContestChange(); });
  $('district').addEventListener('input', () => {
    const d = districtByLabel.get($('district').value);
    if (d !== undefined && d !== state.district) { state.district = d; onContestChange(); }
  });
  $('geography').addEventListener('change', () => { state.geography = $('geography').value; render(); });
  $('view-map').addEventListener('click', () => setView('map'));
  $('view-table').addEventListener('click', () => setView('table'));
  $('scope-level').addEventListener('change', () => {
    state.scope.level = $('scope-level').value; updateScopeControls(); render();
  });
  // Commit on an exact datalist-label match (fires per keystroke via 'input',
  // so picking a suggestion or typing a full name both work).
  const onScopePick = (kind) => () => {
    if (!scopeUnits) return;
    const fipsKey = `${kind}_fips`;
    const fips = scopeUnits.byLabel[kind].get($(`scope-${kind}`).value);
    if (fips && fips !== state.scope[fipsKey]) {
      state.scope[fipsKey] = fips;
      render();
    }
  };
  $('scope-county').addEventListener('input', onScopePick('county'));
  $('scope-mcd').addEventListener('input', onScopePick('mcd'));
  $('scope-muni').addEventListener('input', onScopePick('muni'));

  await onElectionChange();
  showLoading(false);
}

boot().catch((err) => {
  console.error(err);
  $('loading').textContent = 'Failed to load: ' + err.message;
});
