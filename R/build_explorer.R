# R/build_explorer.R
# Builds a fully interactive static explorer.html for GitHub Pages

required_packages <- c("htmltools", "readr", "dplyr", "jsonlite", "purrr")
to_install <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)

library(htmltools)
library(readr)
library(dplyr)
library(jsonlite)
library(purrr)

dir.create("docs", recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# =========================================================
# Paths
# =========================================================
sif_file <- "data_21-04-2026.csv"
confidence_file <- "PointsForSignatures_March2025.csv"

# =========================================================
# Load data
# =========================================================
sif_signature <- read_csv(sif_file, show_col_types = FALSE)
confidence_data <- read_csv(confidence_file, show_col_types = FALSE)

# =========================================================
# Helper functions
# =========================================================
smooth_by_group <- function(data, input_col, output_col, span = 0.2) {
  data %>%
    group_by(crop, climate) %>%
    arrange(week, .by_group = TRUE) %>%
    group_modify(~{
      df <- .x
      
      if (!(input_col %in% names(df))) {
        df[[output_col]] <- NA_real_
        return(df)
      }
      
      valid_idx <- which(!is.na(df[[input_col]]) & !is.na(df$week))
      
      if (length(valid_idx) < 4 || length(unique(df$week[valid_idx])) < 4) {
        df[[output_col]] <- df[[input_col]]
        return(df)
      }
      
      fit <- tryCatch(
        loess(
          formula = as.formula(paste(input_col, "~ week")),
          data = df[valid_idx, ],
          span = span,
          na.action = na.exclude,
          control = loess.control(surface = "direct")
        ),
        error = function(e) NULL
      )
      
      if (is.null(fit)) {
        df[[output_col]] <- df[[input_col]]
      } else {
        pred <- tryCatch(
          predict(fit, newdata = df),
          error = function(e) rep(NA_real_, nrow(df))
        )
        df[[output_col]] <- ifelse(is.na(pred), df[[input_col]], pred)
      }
      
      df
    }) %>%
    ungroup()
}

ensure_smoothed_column <- function(data, raw_col, smooth_col) {
  if (!(smooth_col %in% names(data)) && raw_col %in% names(data)) {
    data <- smooth_by_group(data, raw_col, smooth_col)
  }
  data
}

# =========================================================
# Add smoothed columns missing from source data
# =========================================================
sif_signature <- ensure_smoothed_column(sif_signature, "mean_ndvi", "smooth_ndvi")
sif_signature <- ensure_smoothed_column(sif_signature, "se_ndvi",   "smooth_ndvi_se")
sif_signature <- ensure_smoothed_column(sif_signature, "sd_ndvi",   "smooth_ndvi_sd")

sif_signature <- ensure_smoothed_column(sif_signature, "mean_lai",  "smooth_lai")
sif_signature <- ensure_smoothed_column(sif_signature, "se_lai",    "smooth_lai_se")
sif_signature <- ensure_smoothed_column(sif_signature, "sd_lai",    "smooth_lai_sd")

sif_signature <- ensure_smoothed_column(sif_signature, "mean_fpar", "smooth_fpar")
sif_signature <- ensure_smoothed_column(sif_signature, "se_fpar",   "smooth_fpar_se")
sif_signature <- ensure_smoothed_column(sif_signature, "sd_fpar",   "smooth_fpar_sd")

sif_signature <- ensure_smoothed_column(sif_signature, "mean_gpp",  "smooth_gpp")
sif_signature <- ensure_smoothed_column(sif_signature, "se_gpp",    "smooth_gpp_se")
sif_signature <- ensure_smoothed_column(sif_signature, "sd_gpp",    "smooth_gpp_sd")

sif_signature <- ensure_smoothed_column(sif_signature, "Gs_fromGPP", "smooth_gs")

# =========================================================
# Confidence summary
# =========================================================
mean_confidence <- confidence_data %>%
  group_by(crop, Climate) %>%
  summarise(
    mean_confidence_percentage = round(mean(confidence, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(crop, Climate)

# =========================================================
# Colors
# =========================================================
all_climates <- sort(unique(c(
  as.character(sif_signature$climate),
  as.character(confidence_data$Climate)
)))

palette_vals <- grDevices::hcl.colors(length(all_climates), palette = "Temps")
climate_colors <- setNames(as.character(palette_vals), all_climates)

# =========================================================
# Metadata for variables
# =========================================================
variable_map <- list(
  SIF  = list(mean_raw = "mean_sif",   mean_smooth = "smooth_sif",  se_raw = "se_sif",   se_smooth = "smooth_se",       sd_raw = "sd_sif",   sd_smooth = "smooth_sd"),
  NDVI = list(mean_raw = "mean_ndvi",  mean_smooth = "smooth_ndvi", se_raw = "se_ndvi",  se_smooth = "smooth_ndvi_se",  sd_raw = "sd_ndvi",  sd_smooth = "smooth_ndvi_sd"),
  LAI  = list(mean_raw = "mean_lai",   mean_smooth = "smooth_lai",  se_raw = "se_lai",   se_smooth = "smooth_lai_se",   sd_raw = "sd_lai",   sd_smooth = "smooth_lai_sd"),
  fPAR = list(mean_raw = "mean_fpar",  mean_smooth = "smooth_fpar", se_raw = "se_fpar",  se_smooth = "smooth_fpar_se",  sd_raw = "sd_fpar",  sd_smooth = "smooth_fpar_sd"),
  GPP  = list(mean_raw = "mean_gpp",   mean_smooth = "smooth_gpp",  se_raw = "se_gpp",   se_smooth = "smooth_gpp_se",   sd_raw = "sd_gpp",   sd_smooth = "smooth_gpp_sd"),
  Gs   = list(mean_raw = "Gs_fromGPP", mean_smooth = "smooth_gs",   se_raw = NA,         se_smooth = NA,                sd_raw = NA,         sd_smooth = NA)
)

# =========================================================
# Prepare JSON
# =========================================================
signature_json <- toJSON(
  sif_signature,
  dataframe = "rows",
  auto_unbox = TRUE,
  na = "null",
  digits = 10
)

confidence_json <- toJSON(
  confidence_data,
  dataframe = "rows",
  auto_unbox = TRUE,
  na = "null",
  digits = 10
)

mean_conf_json <- toJSON(
  mean_confidence,
  dataframe = "rows",
  auto_unbox = TRUE,
  na = "null",
  digits = 10
)

climate_colors_json <- toJSON(
  unname(climate_colors),
  auto_unbox = TRUE
)

climate_names_json <- toJSON(
  names(climate_colors),
  auto_unbox = TRUE
)

variable_map_json <- toJSON(
  variable_map,
  auto_unbox = TRUE,
  na = "null"
)

crop_choices <- sort(unique(sif_signature$crop))
crop_choices_json <- toJSON(crop_choices, auto_unbox = TRUE)

# =========================================================
# CSS
# =========================================================
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
  --max-width:1380px;
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

.explorer-hero{
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

.explorer-title{
  margin:0;
  font-size:clamp(2.1rem, 4.8vw, 4.3rem);
  line-height:.98;
  letter-spacing:-.04em;
  max-width:980px;
}

.explorer-lead{
  margin:18px 0 0;
  color:var(--muted);
  line-height:1.8;
  max-width:900px;
  font-size:1.03rem;
}

.toolbar{
  margin-top:26px;
  display:flex;
  gap:12px;
  flex-wrap:wrap;
}

.btn{
  display:inline-flex;
  align-items:center;
  justify-content:center;
  min-width:170px;
  padding:14px 20px;
  border-radius:999px;
  font-size:.96rem;
  font-weight:600;
  transition:transform .2s ease, box-shadow .2s ease, background .2s ease, border-color .2s ease;
  cursor:pointer;
  border:none;
}

.btn:hover{transform:translateY(-1px)}

.btn-primary{
  color:#021108;
  background:linear-gradient(135deg, var(--accent), var(--accent-strong));
  box-shadow:0 10px 30px rgba(114,245,154,.22);
}

.btn-secondary{
  color:var(--text);
  border:1px solid rgba(157,255,179,.18);
  background:rgba(255,255,255,.03);
}

.explorer-layout{
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

.control-select,
.control-input{
  width:100%;
  padding:13px 14px;
  border-radius:16px;
  border:1px solid var(--border-soft);
  background:rgba(255,255,255,.04);
  color:var(--text);
  outline:none;
}

.control-select[multiple]{
  min-height:140px;
}

.control-select option{
  background:#08120c;
  color:#ffffff;
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

#plotWrap{
  min-height:430px;
}

#plot{
  width:100%;
  min-height:420px;
}

#map{
  width:100%;
  min-height:410px;
  border-radius:20px;
  overflow:hidden;
  border:1px solid rgba(255,255,255,.06);
}

#confidenceTable{
  width:100%;
  border-collapse:collapse;
  font-size:.95rem;
}

#confidenceTable thead th{
  text-align:left;
  padding:12px 10px;
  border-bottom:1px solid rgba(255,255,255,.09);
  color:var(--accent);
  font-weight:600;
}

#confidenceTable tbody td{
  padding:12px 10px;
  border-bottom:1px solid rgba(255,255,255,.05);
  color:var(--muted);
}

#confidenceTable tbody tr:hover td{
  color:var(--text);
  background:rgba(255,255,255,.02);
}

.table-scroll{
  overflow:auto;
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

.small-note{
  color:var(--muted);
  line-height:1.65;
  font-size:.9rem;
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
  border: 1px solid rgba(157,255,179,0.14);
  box-shadow: var(--shadow);

  display: none;

  /* NEW */
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

.multi-select-options::-webkit-scrollbar {
  width: 6px;
}

.multi-select-options::-webkit-scrollbar-thumb {
  background: rgba(157,255,179,0.3);
  border-radius: 6px;
}

@media (max-width:1100px){
  .explorer-layout{grid-template-columns:1fr}
  .control-panel{position:static}
}

@media (max-width:640px){
  .site-header-inner{flex-direction:column;align-items:flex-start}
  .page-shell{padding:24px 18px 48px}
  
}
"

# =========================================================
# JavaScript
# =========================================================
js_main <- "
const signatureData = __SIGNATURE_JSON__;
const confidenceData = __CONFIDENCE_JSON__;
const meanConfidence = __MEAN_CONF_JSON__;
const climateNames = __CLIMATE_NAMES_JSON__;
const climateColorValues = __CLIMATE_COLORS_JSON__;
const cropChoices = __CROP_CHOICES_JSON__;
const variableMap = __VARIABLE_MAP_JSON__;

const climateColors = {};
climateNames.forEach((name, i) => { climateColors[name] = climateColorValues[i]; });

const monthTicks = [1,5,9,13,17,22,26,31,35,40,44,48];
const monthLabels = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'];

const cropSelect = document.getElementById('cropSelect');
const climateSelect = document.getElementById('climateSelect');
const extraVarsSelect = document.getElementById('extraVars');
const smoothChoiceInputs = document.querySelectorAll('input[name=\"smooth_choice\"]');
const variabilityInputs = document.querySelectorAll('input[name=\"variability_choice\"]');
const downloadBtn = document.getElementById('downloadBtn');
const rowCountText = document.getElementById('rowCountText');
const comboCountText = document.getElementById('comboCountText');

function toNum(x){
  if (x === null || x === undefined || x === '') return null;
  const v = Number(x);
  return Number.isFinite(v) ? v : null;
}

function uniqueSorted(arr){
  return Array.from(new Set(arr.filter(x => x !== null && x !== undefined))).sort((a,b) => String(a).localeCompare(String(b)));
}

function createMultiSelect(container, values, selectedValues = [], placeholder = 'Select options', maxSelected = null){
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
  const dropdown = container.querySelector('.multi-select-dropdown');
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

      if (e.target.checked){
        if (maxSelected !== null && container._selected.size >= maxSelected){
          e.target.checked = false;
          return;
        }
        container._selected.add(value);
      } else {
        container._selected.delete(value);
      }

      renderTrigger();
      renderOptions();
      filterOptions();
      container.dispatchEvent(new Event('change'));
    }
  });

  search.addEventListener('input', filterOptions);

  selectAllBtn.addEventListener('click', () => {
    if (maxSelected === null){
      container._values.forEach(v => container._selected.add(v));
    } else {
      container._selected = new Set(container._values.slice(0, maxSelected));
    }
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

function updateMultiSelect(container, values, selectedValues = [], placeholder = 'Select options'){
  createMultiSelect(container, values, selectedValues, placeholder);
}

function getSelectedValues(el){
  if (el.classList.contains('multi-select')){
    return Array.from(el._selected || []);
  }
  return Array.from(el.selectedOptions || []).map(o => o.value);
}

function setMultiSelectOptions(el, values, selectedValues){
  if (el.classList.contains('multi-select')){
    updateMultiSelect(el, values, selectedValues, 'Select options');
    return;
  }

  const selectedSet = new Set(selectedValues || []);
  el.innerHTML = '';
  values.forEach(v => {
    const opt = document.createElement('option');
    opt.value = v;
    opt.textContent = v;
    if (selectedSet.has(v)) opt.selected = true;
    el.appendChild(opt);
  });
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

function getAvailableClimates(selectedCrops){
  return uniqueSorted(
    signatureData
      .filter(d => selectedCrops.includes(d.crop))
      .map(d => d.climate)
  );
}

function groupKey(crop, climate){
  return `${crop}|||${climate}`;
}

function normalizeToSifRange(values, sifValues){
  const xVals = values.filter(v => Number.isFinite(v));
  const sVals = sifValues.filter(v => Number.isFinite(v));

  if (xVals.length === 0 || sVals.length === 0){
    return values.map(() => null);
  }

  const xMin = Math.min(...xVals);
  const xMax = Math.max(...xVals);
  const sMin = Math.min(...sVals);
  const sMax = Math.max(...sVals);

  if (!Number.isFinite(xMin) || !Number.isFinite(xMax) || !Number.isFinite(sMin) || !Number.isFinite(sMax)){
    return values.map(() => null);
  }

  if ((xMax - xMin) === 0){
    const mid = (sMin + sMax) / 2;
    return values.map(v => Number.isFinite(v) ? mid : null);
  }

  return values.map(v => {
    if (!Number.isFinite(v)) return null;
    return ((v - xMin) / (xMax - xMin)) * (sMax - sMin) + sMin;
  });
}

function normalizeWithVariability(values, variability, sifReference){
  const valueNorm = normalizeToSifRange(values, sifReference);

  if (!variability.some(v => Number.isFinite(v))){
    return {
      value: valueNorm,
      ymin: values.map(() => null),
      ymax: values.map(() => null)
    };
  }

  const rawYMin = values.map((v, i) => Number.isFinite(v) && Number.isFinite(variability[i]) ? v - variability[i] : null);
  const rawYMax = values.map((v, i) => Number.isFinite(v) && Number.isFinite(variability[i]) ? v + variability[i] : null);

  return {
    value: valueNorm,
    ymin: normalizeToSifRange(rawYMin, sifReference),
    ymax: normalizeToSifRange(rawYMax, sifReference)
  };
}

function formatNum(x, digits = 4){
  return Number.isFinite(x) ? Number(x).toFixed(digits) : 'NA';
}

function selectedSignatureRows(){
  const crops = getSelectedValues(cropSelect);
  const climates = getSelectedValues(climateSelect);
  return signatureData.filter(d => crops.includes(d.crop) && climates.includes(d.climate));
}

function selectedConfidenceRows(){
  const crops = getSelectedValues(cropSelect);
  const climates = getSelectedValues(climateSelect);
  return confidenceData.filter(d => crops.includes(d.crop) && climates.includes(d.Climate));
}

function selectedMeanConfidenceRows(){
  const crops = getSelectedValues(cropSelect);
  const climates = getSelectedValues(climateSelect);
  return meanConfidence.filter(d => crops.includes(d.crop) && climates.includes(d.Climate));
}

function buildPlotDataset(){
  const rows = selectedSignatureRows();
  const smoothChoice = getRadioValue('smooth_choice');
  const variabilityChoice = getRadioValue('variability_choice');
  const extraVars = getSelectedValues(extraVarsSelect);
  const selectedVars = ['SIF', ...extraVars];

  const comboMap = new Map();

  rows.forEach(row => {
    const key = groupKey(row.crop, row.climate);
    if (!comboMap.has(key)) comboMap.set(key, []);
    comboMap.get(key).push(row);
  });

  const out = [];

  comboMap.forEach((comboRows) => {
    comboRows.sort((a,b) => toNum(a.week) - toNum(b.week));

    const sifInfo = variableMap['SIF'];
    const sifValueCol = smoothChoice === 'smooth' ? sifInfo.mean_smooth : sifInfo.mean_raw;
    const sifValues = comboRows.map(r => toNum(r[sifValueCol]));

    selectedVars.forEach(varLabel => {
      const info = variableMap[varLabel];
      const valueCol = smoothChoice === 'smooth' ? info.mean_smooth : info.mean_raw;
      const varCol =
        variabilityChoice === 'se' ? (smoothChoice === 'smooth' ? info.se_smooth : info.se_raw) :
        variabilityChoice === 'sd' ? (smoothChoice === 'smooth' ? info.sd_smooth : info.sd_raw) :
        null;

      const rawValues = comboRows.map(r => toNum(r[valueCol]));
      const rawVar = comboRows.map(r => varCol ? toNum(r[varCol]) : null);

      let value = rawValues.slice();
      let ymin = rawValues.map((v, i) => (Number.isFinite(v) && Number.isFinite(rawVar[i])) ? v - rawVar[i] : null);
      let ymax = rawValues.map((v, i) => (Number.isFinite(v) && Number.isFinite(rawVar[i])) ? v + rawVar[i] : null);

      if (varLabel !== 'SIF'){
        const normRes = normalizeWithVariability(rawValues, rawVar, sifValues);
        value = normRes.value;
        ymin = normRes.ymin;
        ymax = normRes.ymax;
      }

      comboRows.forEach((row, i) => {
        out.push({
          crop: row.crop,
          climate: row.climate,
          week: toNum(row.week),
          variable: varLabel,
          raw_value: rawValues[i],
          raw_variability: rawVar[i],
          value: value[i],
          ymin: ymin[i],
          ymax: ymax[i]
        });
      });
    });
  });

  return out.filter(d => Number.isFinite(d.week));
}

function renderPlotError(message){
  const plotDiv = document.getElementById('plot');
  plotDiv.innerHTML = `<div class=\"empty-state\"><div><strong>Plot error</strong><br>${message}</div></div>`;
}

function renderPlot(){
  const plotDiv = document.getElementById('plot');

  if (typeof Plotly === 'undefined'){
    renderPlotError('Plotly did not load.');
    return;
  }

  try {
    const data = buildPlotDataset();

    if (data.length === 0){
      plotDiv.innerHTML = '<div class=\"empty-state\">No data available for the current selection.</div>';
      return;
    }

    plotDiv.innerHTML = '';

    const crops = uniqueSorted(data.map(d => d.crop));
    const extraVars = getSelectedValues(extraVarsSelect);
    const dashMap = {SIF: 'solid'};
    if (extraVars[0]) dashMap[extraVars[0]] = 'dash';
    if (extraVars[1]) dashMap[extraVars[1]] = 'dot';

    const traces = [];
    const annotations = [];
    const rowsN = crops.length;
    const legendSeen = new Set();

    function axisRef(base, idx){
      return idx === 0 ? base : `${base}${idx + 1}`;
    }

    crops.forEach((crop, idx) => {
      const suffix = idx === 0 ? '' : String(idx + 1);
      const xref = `x${suffix}`;
      const yref = `y${suffix}`;
      const cropData = data.filter(d => d.crop === crop);

      const comboKeys = uniqueSorted(cropData.map(d => `${d.climate}|||${d.variable}`));

      comboKeys.forEach(key => {
        const [climate, variable] = key.split('|||');
        const s = cropData
          .filter(d => d.climate === climate && d.variable === variable)
          .sort((a,b) => a.week - b.week);

        if (s.length === 0) return;

        const color = climateColors[climate] || '#9dffb3';
        const legendKey = `${climate}__${variable}`;
        const showLegend = !legendSeen.has(legendKey);
        if (showLegend) legendSeen.add(legendKey);

        const hoverText = s.map(d => {
          if (variable === 'SIF'){
            return `<b>Crop:</b> ${d.crop}<br><b>Climate:</b> ${d.climate}<br><b>Variable:</b> ${d.variable}<br><b>Week:</b> ${d.week}<br><b>Value:</b> ${formatNum(d.raw_value)}${
              d.raw_variability !== null ? `<br><b>${getRadioValue('variability_choice').toUpperCase()}:</b> ${formatNum(d.raw_variability)}` : ''
            }`;
          }
          return `<b>Crop:</b> ${d.crop}<br><b>Climate:</b> ${d.climate}<br><b>Variable:</b> ${d.variable}<br><b>Week:</b> ${d.week}<br><b>Original value:</b> ${formatNum(d.raw_value)}<br><b>Normalized to SIF range:</b> ${formatNum(d.value)}${
            d.raw_variability !== null ? `<br><b>Original ${getRadioValue('variability_choice').toUpperCase()}:</b> ${formatNum(d.raw_variability)}` : ''
          }`;
        });

        if (getRadioValue('variability_choice') !== 'none' && s.some(d => d.ymin !== null && d.ymax !== null)) {
          traces.push({
            x: s.map(d => d.week),
            y: s.map(d => d.ymin),
            mode: 'lines',
            line: {width: 0},
            hoverinfo: 'skip',
            showlegend: false,
            xaxis: xref,
            yaxis: yref
          });

          traces.push({
            x: s.map(d => d.week),
            y: s.map(d => d.ymax),
            mode: 'lines',
            line: {width: 0},
            fill: 'tonexty',
            fillcolor: variable === 'SIF' ? hexToRgba(color, 0.18) : hexToRgba(color, 0.08),
            hoverinfo: 'skip',
            showlegend: false,
            xaxis: xref,
            yaxis: yref
          });
        }

        traces.push({
          x: s.map(d => d.week),
          y: s.map(d => d.value),
          mode: 'lines',
          name: `${climate} · ${variable}`,
          legendgroup: `${climate} · ${variable}`,
          showlegend: showLegend,
          text: hoverText,
          hovertemplate: '%{text}<extra></extra>',
          line: {
            color: color,
            width: 2.2,
            dash: dashMap[variable] || 'solid'
          },
          xaxis: xref,
          yaxis: yref
        });
      });

      const top = 1 - (idx / rowsN);
      annotations.push({
        text: `<b>${crop}</b>`,
        xref: 'paper',
        yref: 'paper',
        x: 0,
        y: top - 0.02,
        xanchor: 'left',
        yanchor: 'top',
        showarrow: false,
        font: {size: 15, color: '#f6f7f7'}
      });
    });

    const layout = {
      paper_bgcolor: 'rgba(0,0,0,0)',
      plot_bgcolor: 'rgba(255,255,255,0.015)',
      font: {color: '#f6f7f7'},
      legend: {orientation: 'h', y: -0.12, x: 0},
      margin: {l: 70, r: 30, t: 30, b: 90},
      height: Math.max(440, 320 * crops.length),
      annotations: annotations
    };

    crops.forEach((crop, idx) => {
      const axisKeyX = idx === 0 ? 'xaxis' : `xaxis${idx + 1}`;
      const axisKeyY = idx === 0 ? 'yaxis' : `yaxis${idx + 1}`;

      const yTop = 1 - (idx / crops.length) - 0.06;
      const yBottom = 1 - ((idx + 1) / crops.length) + 0.03;

      layout[axisKeyX] = {
        domain: [0, 1],
        anchor: axisRef('y', idx),
        tickmode: 'array',
        tickvals: monthTicks,
        ticktext: monthLabels,
        title: idx === crops.length - 1 ? 'Month' : '',
        showgrid: false,
        zeroline: false,
        color: '#dce6e0'
      };

      layout[axisKeyY] = {
        domain: [yBottom, yTop],
        anchor: axisRef('x', idx),
        title: idx === 0 ? 'SIF 743nm (mW/m²/sr/nm)' : '',
        gridcolor: 'rgba(255,255,255,0.06)',
        zeroline: false,
        color: '#dce6e0'
      };
    });

    Plotly.newPlot(plotDiv, traces, layout, {
      responsive: true,
      displayModeBar: true
    });

  } catch (err) {
    console.error(err);
    renderPlotError(err.message || 'Unknown Plotly error');
  }
}

function hexToRgba(hex, alpha){
  const clean = String(hex).replace('#','');
  const bigint = parseInt(clean, 16);
  if (!Number.isFinite(bigint)) return `rgba(157,255,179,${alpha})`;
  const r = (bigint >> 16) & 255;
  const g = (bigint >> 8) & 255;
  const b = bigint & 255;
  return `rgba(${r},${g},${b},${alpha})`;
}

let map;
let markerLayer;

function renderMap(){
  const rows = selectedConfidenceRows();

  if (typeof L === 'undefined') return;

  if (!map){
    map = L.map('map', {zoomControl: true});
    L.tileLayer('https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png', {
      attribution: '&copy; OpenStreetMap &copy; CARTO'
    }).addTo(map);
    markerLayer = L.layerGroup().addTo(map);
  }

  markerLayer.clearLayers();

  if (rows.length === 0){
    map.setView([39, -98], 4);
    return;
  }

  const bounds = [];

  rows.forEach(d => {
    const lat = toNum(d.latitude);
    const lng = toNum(d.longitude);
    if (!Number.isFinite(lat) || !Number.isFinite(lng)) return;

    const climate = d.Climate;
    const color = climateColors[climate] || '#9dffb3';

    const marker = L.circleMarker([lat, lng], {
      radius: 5,
      color: color,
      weight: 1,
      opacity: 0.95,
      fillColor: color,
      fillOpacity: 0.82
    }).bindPopup(
      `<b>Vegetation type:</b> ${d.crop}<br>` +
      `<b>Climate:</b> ${d.Climate}<br>` +
      `<b>Confidence:</b> ${formatNum(toNum(d.confidence), 1)}%`
    );

    markerLayer.addLayer(marker);
    bounds.push([lat, lng]);
  });

  if (bounds.length > 0){
    map.fitBounds(bounds, {padding: [20, 20]});
  } else {
    map.setView([39, -98], 4);
  }
}

function renderTable(){
  const rows = selectedMeanConfidenceRows();
  const container = document.getElementById('tableContainer');

  if (rows.length === 0){
    container.innerHTML = '<div class=\"empty-state\">No confidence rows for the current selection.</div>';
    return;
  }

  rows.sort((a,b) => {
    const c1 = String(a.crop).localeCompare(String(b.crop));
    if (c1 !== 0) return c1;
    return String(a.Climate).localeCompare(String(b.Climate));
  });

  const html = `
    <table id=\"confidenceTable\">
      <thead>
        <tr>
          <th>Vegetation type</th>
          <th>Climate</th>
          <th>Mean confidence percentage</th>
        </tr>
      </thead>
      <tbody>
        ${rows.map(r => `
          <tr>
            <td>${r.crop}</td>
            <td>${r.Climate}</td>
            <td>${Number(r.mean_confidence_percentage).toFixed(2)}</td>
          </tr>
        `).join('')}
      </tbody>
    </table>
  `;

  container.innerHTML = `<div class=\"table-scroll\">${html}</div>`;
}

function updateCounts(){
  const rows = selectedSignatureRows();
  const combos = uniqueSorted(rows.map(d => groupKey(d.crop, d.climate)));
  const crops = getSelectedValues(cropSelect);
  const climates = getSelectedValues(climateSelect);

  rowCountText.textContent = `${rows.length} weekly records · ${crops.length} vegetation type(s) selected`;
  comboCountText.textContent = `${combos.length} crop–climate combinations · ${climates.length} climate(s) selected`;
}

function refreshClimateOptions(){
  const selectedCrops = getSelectedValues(cropSelect);
  const available = getAvailableClimates(selectedCrops);
  const current = getSelectedValues(climateSelect).filter(v => available.includes(v));
  const nextSelection = current.length > 0 ? current : available;
  updateMultiSelect(climateSelect, available, nextSelection, 'Select climate(s)');
}

function refreshAll(){
  updateCounts();
  renderPlot();
  renderMap();
  renderTable();
}

function enforceExtraVarLimit(){
  const selected = getSelectedValues(extraVarsSelect);
  if (selected.length <= 2) return;
  const keep = selected.slice(0, 2);
  Array.from(extraVarsSelect.options).forEach(opt => {
    opt.selected = keep.includes(opt.value);
  });
}

function downloadFilteredData(){
  const rows = selectedSignatureRows();
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
  a.download = 'SIF_data_selected_vegetation_types_climates.csv';
  document.body.appendChild(a);
  a.click();
  document.body.removeChild(a);
  URL.revokeObjectURL(url);
}

function initControls(){
  const defaultCrops = ['Apples', 'Blueberries'].filter(v => cropChoices.includes(v));
  createMultiSelect(cropSelect, cropChoices, defaultCrops, 'Select vegetation type(s)');
  refreshClimateOptions();

  const extraChoices = ['NDVI','LAI','fPAR','GPP','Gs'];
  createMultiSelect(extraVarsSelect, extraChoices, [], 'Select up to two comparison lines', 2);

  cropSelect.addEventListener('change', () => {
    refreshClimateOptions();
    refreshAll();
  });

  climateSelect.addEventListener('change', refreshAll);

  extraVarsSelect.addEventListener('change', () => {
    enforceExtraVarLimit();
    refreshAll();
  });

  smoothChoiceInputs.forEach(el => el.addEventListener('change', refreshAll));
  variabilityInputs.forEach(el => el.addEventListener('change', refreshAll));

  downloadBtn.addEventListener('click', downloadFilteredData);
}

initControls();
refreshAll();
"

# =========================================================
# Inject JSON into JavaScript
# =========================================================
js_main <- gsub("__SIGNATURE_JSON__", signature_json, js_main, fixed = TRUE)
js_main <- gsub("__CONFIDENCE_JSON__", confidence_json, js_main, fixed = TRUE)
js_main <- gsub("__MEAN_CONF_JSON__", mean_conf_json, js_main, fixed = TRUE)
js_main <- gsub("__CLIMATE_NAMES_JSON__", climate_names_json, js_main, fixed = TRUE)
js_main <- gsub("__CLIMATE_COLORS_JSON__", climate_colors_json, js_main, fixed = TRUE)
js_main <- gsub("__CROP_CHOICES_JSON__", crop_choices_json, js_main, fixed = TRUE)
js_main <- gsub("__VARIABLE_MAP_JSON__", variable_map_json, js_main, fixed = TRUE)

# =========================================================
# HTML page
# =========================================================
page <- tags$html(
  lang = "en",
  
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$title("Explorer | SIF Signature Explorer"),
    
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
      rel = "stylesheet"
    ),
    
    # Leaflet CSS
    tags$link(
      rel = "stylesheet",
      href = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css",
      integrity = "sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=",
      crossorigin = ""
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
          tags$a(href = "explorer.html", class = "active", "Explorer"),
          tags$a(href = "relationships.html", "Relationships"),
          tags$a(href = "about.html", "About")
        )
      )
    ),
    
    tags$main(
      class = "page-shell",
      
      tags$section(
        class = "explorer-hero",
        tags$div(class = "eyebrow", "Interactive seasonal signatures"),
        tags$h1(
          class = "explorer-title",
          "SIF signatures across vegetation types and climate zones"
        ),
        tags$p(
          class = "explorer-lead",
          "Explore seasonal SIF signatures across vegetation types and climate zones in the contiguous United States. ",
          "Use the controls to compare weekly or smoothed SIF trajectories, add normalized vegetation-function indicators, ",
          "inspect used locations to calculate these signatures, and download the selected data for further analysis."
        ),
        tags$div(
          class = "toolbar",
          tags$div(class = "status-pill", tags$span(id = "rowCountText", "0 weekly records")),
          tags$div(class = "status-pill", tags$span(id = "comboCountText", "0 crop–climate combinations"))
        )
      ),
      
      tags$section(
        class = "explorer-layout",
        
        tags$aside(
          class = "control-panel",
          
          tags$div(
            class = "control-group",
            tags$label(class = "control-label", "Select vegetation type(s)"),
            tags$div(id = "cropSelect", class = "multi-select"),
            tags$div(
              class = "control-help",
              "Search, select, remove chips, or clear all."
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$label(class = "control-label", "Select climate(s)"),
            tags$div(id = "climateSelect", class = "multi-select"),
            tags$div(
              class = "control-help",
              "Climate options update automatically from the selected vegetation types."
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$div(id = "extraVars", class = "multi-select"),
            tags$div(
              class = "control-help",
              "They are normalized to the SIF range for visual comparison. Hover over lines to read original values."
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$div(class = "control-label", "Choose SIF type"),
            tags$div(
              class = "radio-stack",
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "smooth_choice", value = "raw"),
                tags$span("Weekly averages")
              ),
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "smooth_choice", value = "smooth",checked = "checked"),
                tags$span("Smoothed weekly averages")
              )
            )
          ),
          
          tags$div(
            class = "control-group",
            tags$div(class = "control-label", "Choose variability"),
            tags$div(
              class = "radio-stack",
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "variability_choice", value = "none"),
                tags$span("None")
              ),
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "variability_choice", value = "se", checked = "checked"),
                tags$span("Standard Error (SE)")
              ),
              tags$label(
                class = "option-chip",
                tags$input(type = "radio", name = "variability_choice", value = "sd"),
                tags$span("Standard Deviation (SD)")
              )
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
              tags$h2(class = "panel-title", "Seasonal signature plot")
            ),
            tags$p(
              class = "panel-note",
              "Solid lines show SIF. Additional comparison variables are normalized to the SIF range so their seasonal dynamics can be compared directly."
            ),
            tags$div(
              id = "plotWrap",
              tags$div(id = "plot")
            )
          ),
          
          tags$section(
            class = "data-panel",
            tags$div(
              class = "panel-header",
              tags$h2(class = "panel-title", "Map of confidence points")
            ),
            tags$p(
              class = "panel-note",
              "These points show the selected locations used to derive vegetation–climate signatures, colored by climate."
            ),
            tags$div(id = "map")
          ),
          
          tags$section(
            class = "data-panel",
            tags$div(
              class = "panel-header",
              tags$h2(class = "panel-title", "Confidence summary")
            ),
            tags$p(
              class = "panel-note",
              "Mean confidence percentage summarizes how homogeneous the selected SIF pixels are within each vegetation type and climate zone."
            ),
            tags$div(id = "tableContainer")
          )
        )
      )
    ),
    
    # Plotly
    tags$script(src = "https://cdn.plot.ly/plotly-2.30.0.min.js"),
    
    
    # Leaflet JS
    tags$script(
      src = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js",
      integrity = "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=",
      crossorigin = ""
    ),
    
    # Main JS
    tags$script(HTML(js_main)),
    
    # Page transition JS
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

# =========================================================
# Save output
# =========================================================
save_html(page, file = "docs/explorer.html", background = "black")

message("Interactive explorer written to docs/explorer.html")

