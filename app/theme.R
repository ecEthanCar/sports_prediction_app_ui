# ==============================================================================
# theme.R  -- visual design for the app shell
#
# Direction: matchday programme and stadium scoreboard.
#   ink        deep floodlit navy, used for the header, sidebar and scoreboard
#   paper      cool near-white page, so the green plots stay the warmest thing
#   floodlight amber accent, used once per screen and nowhere else
#   turf       deep teal for active states
#
# Type: Barlow Condensed (headings, jersey lettering), IBM Plex Sans (body),
# IBM Plex Mono (every figure in the app).
# ==============================================================================

app_theme <- function() {
  bslib::bs_theme(
    version      = 5,
    bg           = "#F7F8FA",
    fg           = "#0F2233",
    primary      = "#1E7A5A",
    secondary    = "#5A6B7B",
    base_font    = bslib::font_google("IBM Plex Sans"),
    heading_font = bslib::font_google("Barlow Condensed"),
    code_font    = bslib::font_google("IBM Plex Mono"),
    "border-radius" = "0.5rem"
  )
}

app_css <- function() {
"
:root {
  --ink:        #0F2233;
  --ink-soft:   #1B3A54;
  --paper:      #F7F8FA;
  --card:       #FFFFFF;
  --floodlight: #F2B705;
  --turf:       #1E7A5A;
  --slate:      #5A6B7B;
  --line:       #E3E7EC;
}

body { background: var(--paper); }

/* ---- header ---------------------------------------------------------- */

.app-header {
  background: var(--ink);
  color: #FFFFFF;
  border-radius: 0.5rem;
  padding: 1.5rem 1.75rem 1.4rem;
  margin-bottom: 1.5rem;
  border-bottom: 3px solid var(--floodlight);
}
.app-header h1 {
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-weight: 700;
  font-size: 2.35rem;
  letter-spacing: 0.01em;
  text-transform: uppercase;
  margin: 0;
  line-height: 1.05;
}
.app-header p {
  color: #A8BACB;
  font-size: 0.95rem;
  margin: 0.35rem 0 0;
}

/* ---- sidebar --------------------------------------------------------- */

.well, .card {
  background: var(--card);
  border: 1px solid var(--line);
  box-shadow: 0 1px 2px rgba(15, 34, 51, 0.04);
}
.sidebar-panel .eyebrow {
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 0.9rem;
  font-weight: 600;
  letter-spacing: 0.14em;
  text-transform: uppercase;
  color: var(--slate);
  padding-bottom: 0.35rem;
  border-bottom: 2px solid var(--line);
  margin: 0 0 0.9rem;
}
.sidebar-panel .eyebrow + .shiny-input-container { margin-top: 0; }
.sidebar-panel .form-group { margin-bottom: 0.9rem; }
.sidebar-panel label {
  font-size: 0.78rem;
  font-weight: 600;
  letter-spacing: 0.04em;
  text-transform: uppercase;
  color: var(--slate);
  margin-bottom: 0.3rem;
}
.sidebar-panel hr { border-color: var(--line); margin: 1.4rem 0; }
.sidebar-panel .help-block, .sidebar-panel .form-text {
  font-size: 0.82rem;
  color: var(--slate);
  line-height: 1.5;
}

/* the vs rule between the two team pickers */
.vs-rule {
  display: flex;
  align-items: center;
  gap: 0.6rem;
  margin: 0.1rem 0 0.9rem;
  color: var(--slate);
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 0.85rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
}
.vs-rule::before, .vs-rule::after {
  content: '';
  flex: 1;
  height: 1px;
  background: var(--line);
}

/* ---- inputs ---------------------------------------------------------- */

.selectize-input, .form-select, .form-control {
  border: 1px solid var(--line) !important;
  border-radius: 0.4rem !important;
  box-shadow: none !important;
  font-size: 0.95rem;
}
.selectize-input.focus, .form-select:focus, .form-control:focus {
  border-color: var(--turf) !important;
  box-shadow: 0 0 0 3px rgba(30, 122, 90, 0.15) !important;
}
.selectize-dropdown .active { background: rgba(30, 122, 90, 0.10); color: var(--ink); }

/* ---- tabs ------------------------------------------------------------ */

.nav-tabs { border-bottom: 2px solid var(--line); gap: 0.15rem; }
.nav-tabs .nav-link {
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 1.05rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--slate);
  border: none;
  border-bottom: 3px solid transparent;
  border-radius: 0;
  padding: 0.55rem 1.1rem;
}
.nav-tabs .nav-link:hover { color: var(--ink); background: rgba(15, 34, 51, 0.03); }
.nav-tabs .nav-link.active {
  color: var(--ink);
  background: transparent;
  border-bottom-color: var(--floodlight);
}

/* ---- scoreboard (signature element) ---------------------------------- */

.fixture-bar {
  background: var(--ink);
  border-radius: 0.5rem;
  border-bottom: 3px solid var(--floodlight);
  padding: 1rem 1.5rem 0.9rem;
  margin-bottom: 1.25rem;
  display: grid;
  grid-template-columns: 1fr auto 1fr;
  align-items: center;
  gap: 1rem;
}
.fixture-context {
  grid-column: 1 / -1;
  text-align: center;
  font-size: 0.72rem;
  letter-spacing: 0.18em;
  text-transform: uppercase;
  color: #7B93A8;
  margin-bottom: 0.5rem;
}
.fixture-team {
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 1.5rem;
  font-weight: 700;
  letter-spacing: 0.02em;
  text-transform: uppercase;
  color: #FFFFFF;
  line-height: 1.1;
}
.fixture-role {
  font-size: 0.7rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: #7B93A8;
  margin-top: 0.15rem;
}
.fixture-side--away { text-align: right; }
.fixture-score { text-align: center; white-space: nowrap; }
.fixture-score .xg {
  font-family: 'IBM Plex Mono', ui-monospace, monospace;
  font-size: 1.7rem;
  font-weight: 500;
  color: var(--floodlight);
}
.fixture-score .xg-sep {
  font-size: 0.7rem;
  letter-spacing: 0.16em;
  text-transform: uppercase;
  color: #7B93A8;
  margin: 0 0.7rem;
  vertical-align: 0.35rem;
}

/* ---- content cards --------------------------------------------------- */

.panel-card {
  background: var(--card);
  border: 1px solid var(--line);
  border-left: 3px solid var(--turf);
  border-radius: 0.4rem;
  padding: 1rem 1.15rem;
  margin-top: 1rem;
}
.panel-card--result { border-left-color: var(--floodlight); }
.panel-card h5 {
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 1.05rem;
  font-weight: 600;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--slate);
  margin: 0 0 0.7rem;
}
.panel-card ul { margin: 0; padding-left: 1.1rem; }
.panel-card li { margin-bottom: 0.3rem; }
.panel-card .num {
  font-family: 'IBM Plex Mono', ui-monospace, monospace;
  font-weight: 500;
  color: var(--ink);
}
.panel-card .muted { color: var(--slate); }

.section-title {
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 1.3rem;
  font-weight: 600;
  letter-spacing: 0.06em;
  text-transform: uppercase;
  color: var(--ink);
  margin: 0 0 0.75rem;
}

/* ---- tables ---------------------------------------------------------- */

.table, .shiny-table {
  --bs-table-bg: var(--card);
  border: 1px solid var(--line);
  border-radius: 0.4rem;
  overflow: hidden;
  font-size: 0.92rem;
}
.table > thead th, .shiny-table > thead th {
  background: var(--ink);
  color: #FFFFFF;
  font-family: 'Barlow Condensed', 'Arial Narrow', sans-serif;
  font-size: 0.9rem;
  font-weight: 600;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  border: none;
  padding: 0.6rem 0.8rem;
}
.table > tbody td, .shiny-table > tbody td {
  padding: 0.5rem 0.8rem;
  border-color: var(--line);
  vertical-align: middle;
}
/* numbers are data: set them in mono, right aligned */
.table > tbody td:not(:first-child),
.shiny-table > tbody td:not(:first-child),
.table > thead th:not(:first-child),
.shiny-table > thead th:not(:first-child) {
  font-family: 'IBM Plex Mono', ui-monospace, monospace;
  text-align: right;
  font-variant-numeric: tabular-nums;
}
.table > tbody tr:hover td { background: rgba(30, 122, 90, 0.05); }

/* ---- model summary --------------------------------------------------- */

pre {
  background: var(--ink);
  color: #D8E2EC;
  border: none;
  border-radius: 0.4rem;
  padding: 1.1rem 1.25rem;
  font-family: 'IBM Plex Mono', ui-monospace, monospace;
  font-size: 0.82rem;
  line-height: 1.5;
  max-height: 70vh;
}

/* ---- states ---------------------------------------------------------- */

.shiny-output-error-validation {
  color: var(--slate);
  font-size: 0.95rem;
  padding: 1.5rem 0;
}
.recalculating { opacity: 0.35; transition: opacity 0.15s ease; }

a:focus-visible, button:focus-visible, .nav-link:focus-visible,
.selectize-input.focus, select:focus-visible {
  outline: 2px solid var(--floodlight);
  outline-offset: 2px;
}

/* ---- documentation panel -------------------------------------------------- */
.doc-block { max-width: 62ch; margin-bottom: 30px; }
.doc-block:last-of-type { margin-bottom: 22px; }
.doc-h {
  font-size: 1.05rem; font-weight: 600;
  letter-spacing: 0.06em; text-transform: uppercase;
  margin: 0 0 10px; color: var(--ink);
}
.doc-block p { font-size: 0.92rem; line-height: 1.62; margin: 0 0 10px; }
.doc-note {
  font-size: 0.84rem !important; color: var(--slate);
  border-left: 2px solid var(--line); padding-left: 12px; margin-top: 12px !important;
}
.doc-fit {
  font-size: 0.8rem !important; color: var(--slate);
  letter-spacing: 0.03em; margin-top: 4px !important;
}

/* the ladders are the centrepiece: two side by side, stacking on narrow screens */
.ladder-row {
  display: grid; grid-template-columns: 1fr 1fr; gap: 16px;
  max-width: none; margin: 16px 0 4px;
}
.ladder { border: 1px solid var(--line); border-radius: 8px; overflow: hidden; }
.lad-head {
  background: var(--ink); color: #fff;
  font-family: var(--bs-headings-font-family);
  font-size: 0.95rem; font-weight: 600; letter-spacing: 0.05em;
  text-transform: uppercase; padding: 9px 14px;
}
.ladder table { width: 100%; margin: 0; }
.ladder td { padding: 9px 14px; border-bottom: 1px solid #EEF1F4; vertical-align: top; }
.ladder tr:last-child td { border-bottom: none; }
.lad-label { font-size: 0.86rem; }
.lad-note { display: block; font-size: 0.73rem; color: var(--slate); margin-top: 2px; }
.lad-factor {
  font-family: var(--bs-font-monospace); font-size: 0.82rem;
  color: var(--slate); text-align: right; white-space: nowrap;
}
.lad-run {
  font-family: var(--bs-font-monospace); font-size: 0.9rem;
  text-align: right; font-weight: 600; white-space: nowrap;
}
.lad-foot {
  background: #FAFBFC; border-top: 1px solid var(--line);
  padding: 9px 14px; font-size: 0.85rem; font-weight: 600;
  color: var(--turf); text-align: right;
}

.term-table { width: auto; min-width: 340px; margin: 6px 0 4px; font-size: 0.86rem; }
.term-table th {
  font-size: 0.68rem; letter-spacing: 0.07em; text-transform: uppercase;
  color: var(--slate); font-weight: 600; padding: 0 18px 6px 0;
  border-bottom: 2px solid var(--line);
}
.term-table td { padding: 7px 18px 7px 0; border-bottom: 1px solid #EEF1F4; }
.term-table td:first-child, .term-table td:last-child {
  font-family: var(--bs-font-monospace); font-variant-numeric: tabular-nums;
}

.doc-details {
  border: 1px solid var(--line); border-radius: 8px;
  padding: 0; max-width: 62ch; background: #FAFBFC;
}
.doc-details > summary {
  cursor: pointer; padding: 12px 16px;
  font-size: 0.86rem; font-weight: 600; color: var(--ink-soft);
  list-style: none;
}
.doc-details > summary::-webkit-details-marker { display: none; }
.doc-details > summary::before { content: '+ '; color: var(--floodlight); font-weight: 700; }
.doc-details[open] > summary::before { content: '- '; }
.doc-details[open] > summary { border-bottom: 1px solid var(--line); }
.doc-details-body { padding: 14px 16px 4px; }
.doc-details-body p { font-size: 0.86rem; line-height: 1.6; }
.doc-details-body pre {
  background: #fff; border: 1px solid var(--line); border-radius: 6px;
  padding: 12px 14px; font-size: 0.78rem; margin-bottom: 12px;
}

@media (max-width: 900px) {
  .ladder-row { grid-template-columns: 1fr; }
}

/* ---- MathJax ---------------------------------------------------------------
   The preview span holds the raw TeX until typesetting finishes. If a node is
   typeset twice it can linger and overlap the rendered math, so hide it. */
.MathJax_Preview { display: none !important; }

/* Prose is capped for readability; equations are not, so they can be wider
   than the text column and scroll rather than clip. */
.doc-block { max-width: none; }
.doc-block > p, .doc-block > .term-table, .doc-details-body > p { max-width: 62ch; }

.eq {
  margin: 22px 0;
  padding: 4px 2px;
  text-align: center;
  overflow-x: auto; overflow-y: hidden;
  max-width: 100%;
}
.eq .MathJax_Display, .eq mjx-container[display] {
  margin: 0 !important;
  min-width: 0 !important;
}
mjx-container[display], .MathJax_Display { overflow-x: auto; overflow-y: hidden; }

/* Inline math needs a little air, or it collides with the words either side. */
.doc-block .MathJax, .doc-details-body .MathJax,
.doc-block mjx-container:not([display]),
.doc-details-body mjx-container:not([display]) {
  margin: 0 0.14em;
  color: var(--ink);
}

/* symbol column tying each ladder row to its term in the equation */
.lad-sym {
  text-align: center; white-space: nowrap;
  color: var(--slate); font-size: 0.88rem;
  width: 1%; padding-left: 4px !important; padding-right: 4px !important;
}
.lad-sym .MathJax, .lad-sym mjx-container { margin: 0 !important; }


/* ---- one-line lambda ladder ------------------------------------------------
   Sits under the fixture bar. Same arithmetic as the ladder table on the
   documentation tab, compressed to a single row. Wraps rather than scrolls,
   because a broken formula still reads left to right. */
.lambda-line {
  display: flex; flex-wrap: wrap; align-items: baseline;
  justify-content: center; gap: 6px 10px;
  padding: 12px 14px; margin: 0 0 18px;
  border-top: 1px solid var(--line); border-bottom: 1px solid var(--line);
}
.lam-term { display: inline-flex; flex-direction: column; align-items: center; }
.lam-num {
  font-family: var(--bs-font-monospace);
  font-size: 0.95rem; font-weight: 600; line-height: 1.15;
  font-variant-numeric: tabular-nums; color: var(--ink);
}
.lam-lab {
  font-size: 0.66rem; letter-spacing: 0.05em; text-transform: uppercase;
  color: var(--slate); margin-top: 2px; white-space: nowrap;
}
.lam-op { color: var(--slate); font-size: 0.9rem; padding-bottom: 12px; }
.lam-term--total .lam-num { color: var(--turf); font-size: 1.05rem; }
.lam-term--total .lam-lab { color: var(--turf); }

@media (max-width: 700px) {
  .lambda-line { font-size: 0.9em; gap: 4px 8px; }
  .lam-lab { font-size: 0.6rem; }
}

@media (prefers-reduced-motion: reduce) {
  * { transition: none !important; animation: none !important; }
}

@media (max-width: 768px) {
  .app-header h1 { font-size: 1.7rem; }
  .fixture-bar { grid-template-columns: 1fr; text-align: center; }
  .fixture-side--away { text-align: center; }
}
"
}
