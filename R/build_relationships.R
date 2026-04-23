# R/build_relationships.R
# Simple faceted relationships page inspired by the paper figure style

required_packages <- c("htmltools", "readr", "dplyr", "jsonlite")
to_install <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(htmltools)
library(readr)
library(dplyr)
library(jsonlite)

dir.create("docs", recursive = TRUE, showWarnings = FALSE)

rel_file <- "relationship_summary_all_crops.csv"
rel_df <- read_csv(rel_file, show_col_types = FALSE)

names(rel_df) <- trimws(names(rel_df))

required_cols <- c(
  "crop", "crop_group", "climate", "climate_group", "relationship",
  "x_label", "y_label", "n", "intercept", "slope", "se",
  "p_value", "r2", "slope_std", "se_std", "p_value_std"
)

missing_cols <- setdiff(required_cols, names(rel_df))
if (length(missing_cols) > 0) {
  stop(
    "Missing expected columns in relationship_summary_all_crops.csv: ",
    paste(missing_cols, collapse = ", ")
  )
}

rel_df <- rel_df %>%
  mutate(
    relationship = as.character(relationship),
    crop = as.character(crop),
    crop_group = as.character(crop_group),
    climate = as.character(climate),
    climate_group = as.character(climate_group)
  )

relationship_choices <- c("GPP ~ SIF", "SIF ~ NDVI", "GPP ~ NDVI")
relationship_choices <- relationship_choices[relationship_choices %in% unique(rel_df$relationship)]

crop_choices <- sort(unique(rel_df$crop))
crop_group_choices <- c("Cropland", "Natural Vegetation", "Perennial Crops", "Other")
crop_group_choices <- crop_group_choices[crop_group_choices %in% unique(rel_df$crop_group)]

climate_choices <- sort(unique(rel_df$climate))
climate_group_choices <- c("Arid", "Continental", "Temperate", "Polar", "Oceanic", "Tropical")
climate_group_choices <- climate_group_choices[climate_group_choices %in% unique(rel_df$climate_group)]

rel_json <- toJSON(rel_df, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = 10)
relationship_choices_json <- toJSON(relationship_choices, auto_unbox = TRUE)
crop_choices_json <- toJSON(crop_choices, auto_unbox = TRUE)
crop_group_choices_json <- toJSON(crop_group_choices, auto_unbox = TRUE)
climate_choices_json <- toJSON(climate_choices, auto_unbox = TRUE)
climate_group_choices_json <- toJSON(climate_group_choices, auto_unbox = TRUE)

css <- "
:root{
  --bg:#000000;
  --bg-soft:#06100b;
  --panel:rgba(255,255,255,0.05);
  --panel-soft:rgba(255,255,255,0.03);
  --border:rgba(157,255,179,0.12);
  --border-soft:rgba(255,255,255,0.08);
  --text:#f6f7f7;
  --muted:#b8c7be;
  --accent:#9dffb3;
  --accent-strong:#72f59a;
  --shadow:0 20px 60px rgba(0,0,0,0.42);
  --radius:24px;
  --max-width:1400px;
}

*{box-sizing:border-box}

html,body{
  margin:0;
  padding:0;
  background:
    radial-gradient(circle at top right, rgba(90,255,160,0.05), transparent 24%),
    linear-gradient(to bottom, #000000, #04110a 30%, #000000 100%);
  color:var(--text);
  font-family:Inter,system-ui,-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;
  min-height:100%;
}

body{
  opacity: 0;
  transform: translateY(10px);
  transition: opacity .55s ease, transform .55s ease;
}

body.page-loaded{
  opacity: 1;
  transform: translateY(0);
}

.site-nav a,
.btn,
.multi-select-trigger{
  transition: opacity .2s ease, transform .2s ease, color .2s ease, background .2s ease, border-color .2s ease;
}

.site-nav a:hover,
.btn:hover{
  transform: translateY(-1px);
}

a{color:inherit;text-decoration:none}

.site-header{
  position:sticky;
  top:0;
  z-index:50;
  backdrop-filter:blur(14px);
  background:rgba(0,0,0,0.58);
  border-bottom:1px solid rgba(255,255,255,0.06);
}

.site-header-inner{
  max-width:var(--max-width);
  margin:0 auto;
  padding:16px 24px;
  display:flex;
  align-items:center;
  justify-content:space-between;
  gap:18px;
}

.site-brand{
  color:var(--accent);
  font-weight:700;
  letter-spacing:.03em;
}

.site-nav{
  display:flex;
  gap:18px;
  flex-wrap:wrap;
}

.site-nav a{
  color:var(--muted);
  font-size:.95rem;
}

.site-nav a.active,
.site-nav a:hover{
  color:var(--text);
}

.page-shell{
  max-width:var(--max-width);
  margin:0 auto;
  padding:34px 24px 60px;
}

.hero{
  padding:10px 0 22px;
}

.eyebrow{
  color:var(--accent);
  text-transform:uppercase;
  letter-spacing:.14em;
  font-size:.78rem;
  font-weight:600;
  margin-bottom:18px;
}

.hero-title{
  margin:0;
  font-size:clamp(2.1rem, 4.8vw, 4.2rem);
  line-height:.98;
  letter-spacing:-.04em;
  max-width:980px;
}

.hero-lead{
  margin:18px 0 0;
  color:var(--muted);
  line-height:1.8;
  max-width:940px;
  font-size:1.03rem;
}

.toolbar{
  margin-top:26px;
  display:flex;
  gap:12px;
  flex-wrap:wrap;
}

.status-pill{
  display:inline-flex;
  align-items:center;
  gap:8px;
  padding:9px 12px;
  border-radius:999px;
  background:rgba(157,255,179,.08);
  border:1px solid rgba(157,255,179,.14);
  color:var(--accent);
  font-size:.84rem;
}

.layout{
  margin-top:28px;
  display:grid;
  grid-template-columns:330px minmax(0,1fr);
  gap:22px;
  align-items:start;
}

.control-panel{
  position:sticky;
  top:82px;
  padding:22px;
  border-radius:26px;
  background:linear-gradient(180deg, rgba(255,255,255,.05), rgba(255,255,255,.03));
  border:1px solid var(--border);
  box-shadow:var(--shadow);
}

.control-group + .control-group{margin-top:18px}

.control-label{
  display:block;
  margin-bottom:8px;
  font-size:.92rem;
  font-weight:600;
  color:var(--text);
}

.control-help{
  margin-top:7px;
  color:var(--muted);
  font-size:.83rem;
  line-height:1.55;
}

.radio-stack{
  display:grid;
  gap:10px;
}

.option-chip{
  display:flex;
  align-items:center;
  gap:10px;
  padding:12px 14px;
  border-radius:16px;
  border:1px solid rgba(255,255,255,.07);
  background:rgba(255,255,255,.03);
  color:var(--muted);
}

.option-chip input{accent-color:#9dffb3}

.btn{
  display:inline-flex;
  align-items:center;
  justify-content:center;
  min-width:170px;
  padding:14px 20px;
  border-radius:999px;
  font-size:.96rem;
  font-weight:600;
  cursor:pointer;
  border:none;
}

.btn-primary{
  color:#021108;
  background:linear-gradient(135deg, var(--accent), var(--accent-strong));
  box-shadow:0 10px 30px rgba(114,245,154,.22);
}

.panel-stack{
  display:grid;
  gap:20px;
}

.data-panel{
  padding:22px;
  border-radius:26px;
  background:linear-gradient(180deg, rgba(255,255,255,.05), rgba(255,255,255,.025));
  border:1px solid rgba(157,255,179,.10);
  box-shadow:var(--shadow);
}

.panel-header{
  display:flex;
  align-items:center;
  justify-content:space-between;
  gap:14px;
  margin-bottom:12px;
}

.panel-title{
  margin:0;
  font-size:1.08rem;
}

.panel-note{
  margin:0 0 16px;
  color:var(--muted);
  line-height:1.7;
  font-size:.95rem;
}

#r2Plot, #slopePlot{
  width:100%;
  min-height:520px;
}

.empty-state{
  display:flex;
  align-items:center;
  justify-content:center;
  min-height:220px;
  border-radius:20px;
  border:1px dashed rgba(157,255,179,.18);
  background:rgba(255,255,255,.02);
  color:var(--muted);
  text-align:center;
  padding:22px;
  line-height:1.75;
}

.multi-select {
  position: relative;
}

.multi-select-trigger {
  width: 100%;
  min-height: 52px;
  padding: 10px 14px;
  border-radius: 16px;
  border: 1px solid var(--border-soft);
  background: rgba(255,255,255,.04);
  color: var(--text);
  display: flex;
  align-items: center;
  gap: 8px;
  flex-wrap: wrap;
  cursor: pointer;
}

.multi-select-placeholder {
  color: var(--muted);
}

.multi-select-chip {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding: 6px 10px;
  border-radius: 999px;
  background: rgba(157,255,179,.10);
  border: 1px solid rgba(157,255,179,.18);
  color: var(--text);
  font-size: .84rem;
}

.multi-select-chip button {
  border: none;
  background: transparent;
  color: var(--accent);
  cursor: pointer;
  font-size: .9rem;
  line-height: 1;
  padding: 0;
}

.multi-select-dropdown {
  position: absolute;
  top: calc(100% + 8px);
  left: 0;
  right: 0;
  z-index: 1000;
  padding: 12px;
  border-radius: 18px;
  background: #08120c;
  border: 1px solid rgba(157,255,179,.14);
  box-shadow: var(--shadow);
  display: none;
  max-height: 320px;
  overflow: hidden;
}

.multi-select.open .multi-select-dropdown {
  display: block;
}

.multi-select-search {
  width: 100%;
  padding: 11px 12px;
  border-radius: 12px;
  border: 1px solid rgba(255,255,255,.08);
  background: rgba(255,255,255,.04);
  color: var(--text);
  margin-bottom: 10px;
}

.multi-select-actions {
  display: flex;
  gap: 8px;
  margin-bottom: 10px;
}

.multi-select-actions button {
  border: 1px solid rgba(157,255,179,.14);
  background: rgba(255,255,255,.03);
  color: var(--text);
  border-radius: 10px;
  padding: 8px 10px;
  cursor: pointer;
  font-size: .82rem;
}

.multi-select-options {
  max-height: 180px;
  overflow-y: auto;
  display: grid;
  gap: 8px;
  padding-right: 4px;
}

.multi-select-options::-webkit-scrollbar {
  width: 6px;
}

.multi-select-options::-webkit-scrollbar-thumb {
  background: rgba(157,255,179,0.3);
  border-radius: 6px;
}

.multi-select-option {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 10px 12px;
  border-radius: 12px;
  background: rgba(255,255,255,.03);
  border: 1px solid rgba(255,255,255,.05);
  color: var(--muted);
  cursor: pointer;
}

.multi-select-option input {
  accent-color: #9dffb3;
}

.multi-select-option.hidden {
  display: none;
}

.small-note{
  color:var(--muted);
  line-height:1.65;
  font-size:.9rem;
}

@media (max-width:1100px){
  .layout{grid-template-columns:1fr}
  .control-panel{position:static}
}

@media (max-width:640px){
  .site-header-inner{flex-direction:column;align-items:flex-start}
  .page-shell{padding:24px 18px 48px}
}
"

js_main <- "
const relData = __REL_JSON__;
const relationshipChoices = __REL_CHOICES_JSON__;
const cropChoices = __CROP_CHOICES_JSON__;
const cropGroupChoices = __CROP_GROUP_CHOICES_JSON__;
const climateChoices = __CLIMATE_CHOICES_JSON__;
const climateGroupChoices = __CLIMATE_GROUP_CHOICES_JSON__;

const climatePalette = {
  'Arid': '#FFD400',
  'Continental': '#7E96D1',
  'Oceanic': '#00A88F',
  'Polar': '#FF7A59',
  'Temperate': '#EE6FB5',
  'Tropical': '#7FBF00'
};

const MAX_INDIVIDUAL_FACETS = 5;

const relationshipSelect = document.getElementById('relationshipSelect');
const cropSelect = document.getElementById('cropSelect');
const cropGroupSelect = document.getElementById('cropGroupSelect');
const climateSelect = document.getElementById('climateSelect');
const climateGroupSelect = document.getElementById('climateGroupSelect');
const summaryTypeInputs = document.querySelectorAll('input[name=\"summary_type\"]');
const downloadBtn = document.getElementById('downloadBtn');
const rowCountText = document.getElementById('rowCountText');
const metricCountText = document.getElementById('metricCountText');

function toNum(x){
  if (x === null || x === undefined || x === '') return null;
  const v = Number(x);
  return Number.isFinite(v) ? v : null;
}

function uniqueSorted(arr){
  return Array.from(new Set(arr.filter(x => x !== null && x !== undefined && x !== '')))
    .sort((a,b) => String(a).localeCompare(String(b)));
}

function makeColorMap(values){
  const base = [
    '#FFD400', '#7E96D1', '#00A88F', '#FF7A59', '#EE6FB5',
    '#7FBF00', '#9B8CFF', '#00C2FF', '#FFA600', '#8DD3C7',
    '#FB8072', '#80B1D3', '#FDB462', '#B3DE69', '#BC80BD'
  ];

  const map = {};
  values.forEach((v, i) => {
    map[v] = base[i % base.length];
  });
  return map;
}

function escapeHtml(str){
  return String(str)
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('\"', '&quot;')
    .replaceAll(\"'\", '&#39;');
}

function getRadioValue(name){
  const el = document.querySelector(`input[name=\"${name}\"]:checked`);
  return el ? el.value : null;
}

function createMultiSelect(container, values, selectedValues = [], placeholder = 'Select options'){
  container._values = values.slice();
  container._selected = new Set(selectedValues);

  container.innerHTML = `
    <div class=\"multi-select-trigger\"></div>
    <div class=\"multi-select-dropdown\">
      <input type=\"text\" class=\"multi-select-search\" placeholder=\"Search...\">
      <div class=\"multi-select-actions\">
        <button type=\"button\" class=\"ms-select-all\">Select all</button>
        <button type=\"button\" class=\"ms-clear-all\">Clear all</button>
      </div>
      <div class=\"multi-select-options\"></div>
    </div>
  `;

  const trigger = container.querySelector('.multi-select-trigger');
  const search = container.querySelector('.multi-select-search');
  const optionsWrap = container.querySelector('.multi-select-options');
  const selectAllBtn = container.querySelector('.ms-select-all');
  const clearAllBtn = container.querySelector('.ms-clear-all');

  function renderTrigger(){
    const selected = Array.from(container._selected);
    if (selected.length === 0){
      trigger.innerHTML = `<span class=\"multi-select-placeholder\">${placeholder}</span>`;
      return;
    }

    trigger.innerHTML = selected.map(v => `
      <span class=\"multi-select-chip\" data-value=\"${escapeHtml(v)}\">
        ${escapeHtml(v)}
        <button type=\"button\" data-remove=\"${escapeHtml(v)}\">×</button>
      </span>
    `).join('');
  }

  function renderOptions(){
    optionsWrap.innerHTML = container._values.map(v => `
      <label class=\"multi-select-option\" data-value=\"${escapeHtml(v)}\">
        <input type=\"checkbox\" value=\"${escapeHtml(v)}\" ${container._selected.has(v) ? 'checked' : ''}>
        <span>${escapeHtml(v)}</span>
      </label>
    `).join('');
  }

  function filterOptions(){
    const q = search.value.trim().toLowerCase();
    optionsWrap.querySelectorAll('.multi-select-option').forEach(el => {
      const txt = el.dataset.value.toLowerCase();
      el.classList.toggle('hidden', q && !txt.includes(q));
    });
  }

  trigger.addEventListener('click', (e) => {
    const removeValue = e.target.getAttribute('data-remove');
    if (removeValue){
      container._selected.delete(removeValue);
      renderTrigger();
      renderOptions();
      filterOptions();
      container.dispatchEvent(new Event('change'));
      return;
    }
    container.classList.toggle('open');
    if (container.classList.contains('open')) search.focus();
  });

  optionsWrap.addEventListener('change', (e) => {
    if (e.target.matches('input[type=\"checkbox\"]')){
      const value = e.target.value;
      if (e.target.checked) container._selected.add(value);
      else container._selected.delete(value);
      renderTrigger();
      container.dispatchEvent(new Event('change'));
    }
  });

  search.addEventListener('input', filterOptions);

  selectAllBtn.addEventListener('click', () => {
    container._values.forEach(v => container._selected.add(v));
    renderTrigger();
    renderOptions();
    filterOptions();
    container.dispatchEvent(new Event('change'));
  });

  clearAllBtn.addEventListener('click', () => {
    container._selected.clear();
    renderTrigger();
    renderOptions();
    filterOptions();
    container.dispatchEvent(new Event('change'));
  });

  document.addEventListener('click', (e) => {
    if (!container.contains(e.target)) container.classList.remove('open');
  });

  renderTrigger();
  renderOptions();
  filterOptions();
}

function getSelectedValues(el){
  if (el.classList.contains('multi-select')){
    return Array.from(el._selected || []);
  }
  return Array.from(el.selectedOptions || []).map(o => o.value);
}

function selectedRows(){
  const selectedRelationships = getSelectedValues(relationshipSelect);
  const selectedCrop = getSelectedValues(cropSelect);
  const selectedCropGroup = getSelectedValues(cropGroupSelect);
  const selectedClimate = getSelectedValues(climateSelect);
  const selectedClimateGroup = getSelectedValues(climateGroupSelect);
  const summaryType = getRadioValue('summary_type');

  return relData.filter(d => {
    if (selectedRelationships.length > 0 && !selectedRelationships.includes(d.relationship)) return false;

    if (summaryType === 'individual'){
      if (selectedCrop.length > 0 && !selectedCrop.includes(d.crop)) return false;
      if (selectedClimate.length > 0 && !selectedClimate.includes(d.climate)) return false;
    } else {
      if (selectedCropGroup.length > 0 && !selectedCropGroup.includes(d.crop_group)) return false;
      if (selectedClimateGroup.length > 0 && !selectedClimateGroup.includes(d.climate_group)) return false;
    }
    return true;
  });
}

function updateControlVisibility(){
  const summaryType = getRadioValue('summary_type');
  document.getElementById('individualControls').style.display = summaryType === 'individual' ? 'block' : 'none';
  document.getElementById('groupControls').style.display = summaryType === 'group' ? 'block' : 'none';
}

function updateCounts(){
  const rows = selectedRows();
  const summaryType = getRadioValue('summary_type');
  const nRelationships = getSelectedValues(relationshipSelect).length;
  const nCrops = getSelectedValues(cropSelect).length;

  rowCountText.textContent = `${rows.length} relationship summaries`;

  if (summaryType === 'individual' && nCrops > MAX_INDIVIDUAL_FACETS){
    metricCountText.textContent = `Please select at most ${MAX_INDIVIDUAL_FACETS} vegetation types`;
  } else {
    metricCountText.textContent = `${nRelationships} relationship(s) selected`;
  }
}

function buildFacetData(){
  const rows = selectedRows();
  const summaryType = getRadioValue('summary_type');

  const facetField = summaryType === 'group' ? 'crop_group' : 'crop';
  const climateField = summaryType === 'group' ? 'climate_group' : 'climate';

  return rows.map(d => ({
    facet: d[facetField],
    climate: d[climateField],
    relationship: d.relationship,
    r2: toNum(d.r2),
    beta: toNum(d.slope_std),
    beta_se: toNum(d.se_std),
    n: toNum(d.n)
  }));
}

function makeHorizontalDomains(n){
  const gap = 0.04;
  const width = (1 - gap * (n - 1)) / n;
  const domains = [];
  for (let i = 0; i < n; i++){
    const start = i * (width + gap);
    domains.push([start, start + width]);
  }
  return domains;
}

function renderR2Plot(){
  const plotDiv = document.getElementById('r2Plot');

  if (typeof Plotly === 'undefined'){
    plotDiv.innerHTML = '<div class=\"empty-state\">Plotly did not load.</div>';
    return;
  }

  const rows = buildFacetData();

  if (rows.length === 0){
    plotDiv.innerHTML = '<div class=\"empty-state\">No rows for the current selection.</div>';
    return;
  }

  plotDiv.innerHTML = '';

  const summaryType = getRadioValue('summary_type');
  const preferredFacetOrder = ['Cropland', 'Natural Vegetation', 'Perennial Crops', 'Other'];

  const facets = summaryType === 'group'
    ? preferredFacetOrder.filter(f => rows.some(d => d.facet === f))
    : uniqueSorted(rows.map(d => d.facet));

  if (summaryType === 'individual' && facets.length > MAX_INDIVIDUAL_FACETS){
    plotDiv.innerHTML = `<div class=\"empty-state\">Please select at most ${MAX_INDIVIDUAL_FACETS} vegetation types in individual mode.</div>`;
    return;
  }

  if (facets.length === 0){
    plotDiv.innerHTML = '<div class=\"empty-state\">No panels available for the current selection.</div>';
    return;
  }

  const relationships = getSelectedValues(relationshipSelect);
  const relationshipDisplay = relationships.map(r => r.replace(' ~ ', '<br>'));
  const climates = uniqueSorted(rows.map(d => d.climate));
  const dynamicClimateColors = makeColorMap(climates);
  const domains = makeHorizontalDomains(facets.length);

  const traces = [];
  const annotations = [];
  const legendSeen = new Set();

  facets.forEach((facet, i) => {
    const axisSuffix = i === 0 ? '' : String(i + 1);
    const xaxis = 'x' + axisSuffix;
    const yaxis = 'y' + axisSuffix;

    climates.forEach(clim => {
      const sub = rows.filter(d => d.facet === facet && d.climate === clim);

      const yVals = relationships.map(rel => {
        const row = sub.find(r => r.relationship === rel);
        return row ? row.r2 : null;
      });

      const hover = relationships.map(rel => {
        const row = sub.find(r => r.relationship === rel);
        if (!row) return `<b>${facet}</b><br>${clim}<br>${rel}<br>No data`;
        return `
          <b>${facet}</b><br>
          <b>Climate:</b> ${clim}<br>
          <b>Relationship:</b> ${rel}<br>
          <b>R²:</b> ${row.r2 ?? 'NA'}<br>
          <b>n:</b> ${row.n ?? 'NA'}
        `;
      });

      const showLegend = !legendSeen.has(clim);
      if (showLegend) legendSeen.add(clim);

      traces.push({
        type: 'bar',
        x: relationshipDisplay,
        y: yVals,
        name: clim,
        marker: {color: climatePalette[clim] || dynamicClimateColors[clim] || '#9dffb3'},
        customdata: hover,
        hovertemplate: '%{customdata}<extra></extra>',
        xaxis,
        yaxis,
        showlegend: showLegend,
        offsetgroup: clim,
        legendgroup: clim
      });
    });

    annotations.push({
      text: `<b>${facet}</b>`,
      xref: 'paper',
      yref: 'paper',
      x: (domains[i][0] + domains[i][1]) / 2,
      y: 1.08,
      showarrow: false,
      font: {size: 16, color: '#f6f7f7'}
    });
  });

  const layout = {
    barmode: 'group',
    paper_bgcolor: 'rgba(0,0,0,0)',
    plot_bgcolor: 'rgba(255,255,255,0.015)',
    font: {color: '#f6f7f7'},
    margin: {l: 70, r: 30, t: 70, b: 110},
    legend: {orientation: 'h', y: -0.18},
    annotations,
    height: 520
  };

  facets.forEach((facet, i) => {
    const axisSuffix = i === 0 ? '' : String(i + 1);
    const xaxisKey = 'xaxis' + axisSuffix;
    const yaxisKey = 'yaxis' + axisSuffix;

    layout[xaxisKey] = {
      domain: domains[i],
      title: i === Math.floor(facets.length / 2) ? 'Relationship' : '',
      tickangle: 0,
      tickfont: {size: 12},
      automargin: true
    };

    layout[yaxisKey] = {
      domain: [0, 1],
      title: i === 0 ? 'Coefficient of determination (R²)' : '',
      range: [0, 1],
      gridcolor: 'rgba(255,255,255,0.06)'
    };
  });

  Plotly.newPlot(plotDiv, traces, layout, {
    responsive: true,
    displayModeBar: true
  });
}

function renderSlopePlot(){
  const plotDiv = document.getElementById('slopePlot');

  if (typeof Plotly === 'undefined'){
    plotDiv.innerHTML = '<div class=\"empty-state\">Plotly did not load.</div>';
    return;
  }

  const rows = buildFacetData();

  if (rows.length === 0){
    plotDiv.innerHTML = '<div class=\"empty-state\">No rows for the current selection.</div>';
    return;
  }

  plotDiv.innerHTML = '';

  const summaryType = getRadioValue('summary_type');
  const preferredFacetOrder = ['Cropland', 'Natural Vegetation', 'Perennial Crops', 'Other'];

  const facets = summaryType === 'group'
    ? preferredFacetOrder.filter(f => rows.some(d => d.facet === f))
    : uniqueSorted(rows.map(d => d.facet));

  if (summaryType === 'individual' && facets.length > MAX_INDIVIDUAL_FACETS){
    plotDiv.innerHTML = `<div class=\"empty-state\">Please select at most ${MAX_INDIVIDUAL_FACETS} vegetation types in individual mode.</div>`;
    return;
  }

  if (facets.length === 0){
    plotDiv.innerHTML = '<div class=\"empty-state\">No panels available for the current selection.</div>';
    return;
  }

  const relationships = getSelectedValues(relationshipSelect);
  const relationshipDisplay = relationships.map(r => r.replace(' ~ ', '<br>'));
  const climates = uniqueSorted(rows.map(d => d.climate));
  const dynamicClimateColors = makeColorMap(climates);
  const domains = makeHorizontalDomains(facets.length);

  const traces = [];
  const annotations = [];
  const legendSeen = new Set();

  facets.forEach((facet, i) => {
    const axisSuffix = i === 0 ? '' : String(i + 1);
    const xaxis = 'x' + axisSuffix;
    const yaxis = 'y' + axisSuffix;

    climates.forEach(clim => {
      const sub = rows.filter(d => d.facet === facet && d.climate === clim);

      const yVals = relationships.map(rel => {
        const row = sub.find(r => r.relationship === rel);
        return row ? row.beta : null;
      });

      const errVals = relationships.map(rel => {
        const row = sub.find(r => r.relationship === rel);
        return row ? row.beta_se : null;
      });

      const hover = relationships.map(rel => {
        const row = sub.find(r => r.relationship === rel);
        if (!row) return `<b>${facet}</b><br>${clim}<br>${rel}<br>No data`;
        return `
          <b>${facet}</b><br>
          <b>Climate:</b> ${clim}<br>
          <b>Relationship:</b> ${rel}<br>
          <b>Standardized slope (β):</b> ${row.beta ?? 'NA'}<br>
          <b>SE:</b> ${row.beta_se ?? 'NA'}<br>
          <b>n:</b> ${row.n ?? 'NA'}
        `;
      });

      const showLegend = !legendSeen.has(clim);
      if (showLegend) legendSeen.add(clim);

      traces.push({
        type: 'bar',
        x: relationshipDisplay,
        y: yVals,
        name: clim,
        marker: {color: climatePalette[clim] || dynamicClimateColors[clim] || '#9dffb3'},
        customdata: hover,
        hovertemplate: '%{customdata}<extra></extra>',
        xaxis,
        yaxis,
        showlegend: showLegend,
        offsetgroup: clim,
        legendgroup: clim,
        error_y: {
          type: 'data',
          array: errVals,
          visible: true,
          color: '#dce6e0',
          thickness: 1.2,
          width: 4
        }
      });
    });

    annotations.push({
      text: `<b>${facet}</b>`,
      xref: 'paper',
      yref: 'paper',
      x: (domains[i][0] + domains[i][1]) / 2,
      y: 1.08,
      showarrow: false,
      font: {size: 16, color: '#f6f7f7'}
    });
  });

  const validLower = rows
    .map(d => (d.beta !== null && d.beta_se !== null) ? d.beta - d.beta_se : d.beta)
    .filter(v => v !== null);
  const validUpper = rows
    .map(d => (d.beta !== null && d.beta_se !== null) ? d.beta + d.beta_se : d.beta)
    .filter(v => v !== null);

  const yMin = validLower.length ? Math.min(...validLower) : -1;
  const yMax = validUpper.length ? Math.max(...validUpper) : 1;
  const pad = Math.max(0.08, (yMax - yMin) * 0.12);

  const layout = {
    barmode: 'group',
    paper_bgcolor: 'rgba(0,0,0,0)',
    plot_bgcolor: 'rgba(255,255,255,0.015)',
    font: {color: '#f6f7f7'},
    margin: {l: 70, r: 30, t: 70, b: 110},
    legend: {orientation: 'h', y: -0.18},
    annotations,
    height: 520,
    shapes: [{
      type: 'line',
      xref: 'paper',
      yref: 'y',
      x0: 0,
      x1: 1,
      y0: 0,
      y1: 0,
      line: {color: 'rgba(255,255,255,0.5)', width: 1.2, dash: 'dash'}
    }]
  };

  facets.forEach((facet, i) => {
    const axisSuffix = i === 0 ? '' : String(i + 1);
    const xaxisKey = 'xaxis' + axisSuffix;
    const yaxisKey = 'yaxis' + axisSuffix;

    layout[xaxisKey] = {
      domain: domains[i],
      title: i === Math.floor(facets.length / 2) ? 'Relationship' : '',
      tickangle: 0,
      tickfont: {size: 12},
      automargin: true
    };

    layout[yaxisKey] = {
      domain: [0, 1],
      title: i === 0 ? 'Standardized slope (β) ± SE' : '',
      range: [yMin - pad, yMax + pad],
      gridcolor: 'rgba(255,255,255,0.06)'
    };
  });

  Plotly.newPlot(plotDiv, traces, layout, {
    responsive: true,
    displayModeBar: true
  });
}

function downloadFilteredData(){
  const rows = selectedRows();
  if (rows.length === 0) return;

  const headers = Object.keys(rows[0]);
  const escapeCsv = value => {
    if (value === null || value === undefined) return '';
    const str = String(value);
    if (/[\",\\n]/.test(str)) return '\"' + str.replace(/\"/g, '\"\"') + '\"';
    return str;
  };

  const csv = [
    headers.join(','),
    ...rows.map(row => headers.map(h => escapeCsv(row[h])).join(','))
  ].join('\\n');

  const blob = new Blob([csv], {type: 'text/csv;charset=utf-8;'});
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'relationship_summary_filtered.csv';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

function refreshAll(){
  updateControlVisibility();
  updateCounts();
  renderR2Plot();
  renderSlopePlot();
}

function initControls(){
  createMultiSelect(relationshipSelect, relationshipChoices, relationshipChoices, 'Select relationship(s)');
  createMultiSelect(cropSelect, cropChoices, [], 'Select vegetation type(s)');
  createMultiSelect(cropGroupSelect, cropGroupChoices, cropGroupChoices, 'Select vegetation group(s)');
  createMultiSelect(climateSelect, climateChoices, [], 'Select climate(s)');
  createMultiSelect(climateGroupSelect, climateGroupChoices, climateGroupChoices, 'Select climate group(s)');

  relationshipSelect.addEventListener('change', refreshAll);
  cropSelect.addEventListener('change', refreshAll);
  cropGroupSelect.addEventListener('change', refreshAll);
  climateSelect.addEventListener('change', refreshAll);
  climateGroupSelect.addEventListener('change', refreshAll);
  summaryTypeInputs.forEach(el => el.addEventListener('change', refreshAll));
  downloadBtn.addEventListener('click', downloadFilteredData);
}

document.addEventListener('DOMContentLoaded', function(){
  initControls();
  refreshAll();
});
"

js_main <- gsub("__REL_JSON__", rel_json, js_main, fixed = TRUE)
js_main <- gsub("__REL_CHOICES_JSON__", relationship_choices_json, js_main, fixed = TRUE)
js_main <- gsub("__CROP_CHOICES_JSON__", crop_choices_json, js_main, fixed = TRUE)
js_main <- gsub("__CROP_GROUP_CHOICES_JSON__", crop_group_choices_json, js_main, fixed = TRUE)
js_main <- gsub("__CLIMATE_CHOICES_JSON__", climate_choices_json, js_main, fixed = TRUE)
js_main <- gsub("__CLIMATE_GROUP_CHOICES_JSON__", climate_group_choices_json, js_main, fixed = TRUE)

page <- tags$html(
  lang = "en",
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("SIF–GPP–NDVI Relationships | SIF Signature Explorer"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML(css))
  ),
  tags$body(
    tags$header(
      class = "site-header",
      tags$div(
        class = "site-header-inner",
        tags$div(class = "site-brand", "SIF Signature Explorer"),
        tags$nav(
          class = "site-nav",
          tags$a(href = "index.html", "Home"),
          tags$a(href = "learn.html", "Learn"),
          tags$a(href = "explorer.html", "Explorer"),
          tags$a(href = "relationships.html", class = "active", "Relationships"),
          tags$a(href = "about.html", "About")
        )
      )
    ),
    
    tags$main(
      class = "page-shell",
      
      tags$section(
        class = "hero",
        tags$div(class = "eyebrow", "SIF–GPP–NDVI Relationships"),
        tags$h1(
          class = "hero-title",
          "Compare how SIF, GPP, and NDVI relate across vegetation and climate groups"
        ),
        tags$p(
          class = "hero-lead",
          "This page summarizes relationships between SIF, GPP, and NDVI across vegetation types and climate zones. ",
          "Use it to compare how strongly each relationship holds, and whether the direction and strength of these links change between groups. ",
          "Start with the grouped view for a broad overview, or switch to individual vegetation types and climates for more specific comparisons."
        ),
        tags$div(
          class = "toolbar",
          tags$div(class = "status-pill", tags$span(id = "rowCountText", "0 relationship summaries")),
          tags$div(class = "status-pill", tags$span(id = "metricCountText", "0 relationship(s) selected"))
        )
      ),
      
      tags$section(
        class = "layout",
        
        tags$aside(
          class = "control-panel",
          
          tags$div(
            class = "control-group",
            tags$label(class = "control-label", "Relationship(s)"),
            tags$div(id = "relationshipSelect", class = "multi-select"),
            tags$div(
              class = "control-help",
              "By default, all three relationships are selected so the page opens like a summary figure."
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$div(class = "control-label", "View level"),
            tags$div(
              class = "radio-stack",
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "summary_type", value = "individual"),
                tags$span("Individual vegetation types / climates")
              ),
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "summary_type", value = "group", checked = "checked"),
                tags$span("Vegetation groups / climate groups")
              )
            )
          ),
          
          tags$div(
            id = "individualControls",
            style = "display:none;",
            tags$div(
              class = "control-group",
              tags$label(class = "control-label", "Vegetation type(s)"),
              tags$div(id = "cropSelect", class = "multi-select"),
              tags$div(class = "control-help", "Filter specific vegetation types. Maximum 5 at a time.")
            ),
            tags$div(
              class = "control-group",
              tags$label(class = "control-label", "Climate(s)"),
              tags$div(id = "climateSelect", class = "multi-select"),
              tags$div(class = "control-help", "Filter specific climates.")
            )
          ),
          
          tags$div(
            id = "groupControls",
            tags$div(
              class = "control-group",
              tags$label(class = "control-label", "Vegetation group(s)"),
              tags$div(id = "cropGroupSelect", class = "multi-select"),
              tags$div(class = "control-help", "By default, all vegetation groups are selected.")
            ),
            tags$div(
              class = "control-group",
              tags$label(class = "control-label", "Climate group(s)"),
              tags$div(id = "climateGroupSelect", class = "multi-select"),
              tags$div(class = "control-help", "By default, all climate groups are selected.")
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$button(
              id = "downloadBtn",
              class = "btn btn-primary",
              style = "width:100%;",
              "Download filtered data"
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$p(
              class = "small-note",
              "If using these data or outputs, please cite Mamić et al. (2025), ",
              tags$em("Remote Sensing Applications: Society and Environment"),
              ", 40, 101760. ",
              tags$a(
                href = "https://doi.org/10.1016/j.rsase.2025.101760",
                target = "_blank",
                style = "color: var(--accent);",
                "DOI"
              ),
              ". For questions or reuse, contact ",
              tags$a(
                href = "mailto:luka.mamic@unipd.it",
                style = "color: var(--accent);",
                "luka.mamic@unipd.it"
              ),
              "."
            )
          )
        ),
        
        tags$div(
          class = "panel-stack",
          
          tags$section(
            class = "data-panel",
            tags$div(
              class = "panel-header",
              tags$h2(class = "panel-title", "Explained variation (R²)")
            ),
            tags$p(
              class = "panel-note",
              "R² compares how strongly SIF, NDVI, and GPP are linked across the selected vegetation and climate categories."            ),
            tags$div(id = "r2Plot")
          ),
          
          tags$section(
            class = "data-panel",
            tags$div(
              class = "panel-header",
              tags$h2(class = "panel-title", "Relationship strength (β ± SE)")
            ),
            tags$p(
              class = "panel-note",
              "Standardized slope (β) compares the direction and relative strength of each relationship. Error bars show standard error."            ),
            tags$div(id = "slopePlot")
          )
        )
      )
    ),
    
    tags$script(src = "https://cdn.plot.ly/plotly-2.30.0.min.js"),
    tags$script(HTML(js_main)),
    tags$script(HTML("
document.addEventListener('DOMContentLoaded', function() {
  requestAnimationFrame(() => {
    document.body.classList.add('page-loaded');
  });

  const internalLinks = document.querySelectorAll('a[href$=\".html\"]');
  internalLinks.forEach(link => {
    link.addEventListener('click', function(e) {
      const href = link.getAttribute('href');
      if (!href) return;

      e.preventDefault();
      document.body.classList.remove('page-loaded');

      setTimeout(() => {
        window.location.href = href;
      }, 220);
    });
  });
});
"))
  )
)

save_html(page, file = "docs/relationships.html", background = "black")
message("Interactive relationships page written to docs/relationships.html")

