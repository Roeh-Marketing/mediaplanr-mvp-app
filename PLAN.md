# mediaplanr-mvp-app — build plan

A Shiny app for **building, editing, and comparing media plans**, powered by the
`mediaplanr` package. Sibling to `mrmopt-mvp-app`: same shape (bslib navbar,
`_brand.yml` theming, per-panel UI/server modules, an `ellmer` agent with typed
tools), different subject.

Where `mrmopt-mvp-app` is about *modelling* (fit response curves, optimize a
mix), this app is about *intent*: get a plan in, reshape it, fork scenarios off
it, and see how they differ.

## Scope boundary

The app owns I/O, layout, and interaction. **All plan semantics live in
`mediaplanr`** — validation, grain, line items, lineage, scenario derivation,
comparison. The app never reimplements them, and it does no modelling at all;
forecasting and optimization stay in `mrmopt` and are out of scope here.

## The flow

```
  upload / sample          edit                    fork                compare
  ────────────────   ─────────────────   ──────────────────   ───────────────────
  csv or xlsx    →   editable grid   →   build_scenario()  →  summary + cell table
  column mapping     (excel-like)        (named, status)      4 charts
  media_plan_from_df chat ops            lineage tracked      xlsx round-trip
        ↑                                                            │
        └──────────────── same workbook format ──────────────────────┘
```

## Pages

| Page | Purpose |
|---|---|
| **Welcome** | What the app does and the four concepts that matter (plan = intent, line item, scenario, status). One click to load the sample. |
| **Plan** | Upload csv/xlsx or load sample → map columns to grain/week/spend → set metadata → build the base `MediaPlan`. Validation surfaced inline. |
| **Scenarios** | The workbench. Editable grid (line items × weeks, Excel-style), a quick-op panel, and an LLM chat — three ways to change spend. Accumulated edits become a named scenario. |
| **Compare** | Summary table, per-cell table, and four charts across selected scenarios. |
| **Export** | A workbook (one sheet per scenario) that re-uploads cleanly, plus xlsx/CSV of the comparison or a single scenario. |

## Key design decisions

**1. Wide grid, long data.** `MediaPlan@data` is long (one row per line item ×
week). Planners think in a grid: line items down, weeks across. The Scenarios
page pivots to wide for display and maps edits back to long. This is the single
biggest UX decision and the reason the grid feels like a spreadsheet.

**2. Editing is built on `reactable`, not `DT`.** `DT` and `rhandsontable` are
not installed, and adding a dependency for one widget is the wrong trade when
`reactable` can do it: cells render as `<input type="number">` and a delegated
listener in `www/grid.js` pushes changes back through
`Shiny.setInputValue()`. One JS file, no new R dependency, and it matches the
sibling app's table library.

**3. Edits accumulate, then commit.** Typing in a cell does **not** create a
scenario — it stages a pending edit. `build_scenario()` is called once, on
"Save as scenario". Otherwise every keystroke would mint a plan id and a lineage
chain hundreds deep.

**4. Three input modes, one code path.** The grid, the quick-op panel, and the
chat all funnel into `build_scenario(edits=)`. The grid produces a named vector
(absolute values); the panel and the chat produce operations
(`target` + `set`/`scale`/`delta`/`total`). The package already accepts all
three shapes, so the app adds no arithmetic of its own — which matters most for
the chat, where an LLM doing the maths is the failure mode.

**5. Status is the scenario workflow.** Every scenario carries
`in development` / `to review` / `approved`, sourced from
`mediaplanr::status_levels()` so the dropdown and the validator can never
disagree. Derived scenarios reset to `in development` — the package enforces
that an approved plan's child is not itself approved.

**6. Charts are static ggplot.** `plotly` is not installed; static plots match
the sibling app and are enough to see a difference. Colours come from
`_brand.yml`.

**7. Excel round-trips.** A workbook uploads with **one sheet per scenario** —
filename becomes the plan `name`, sheet name becomes the `nickname`, exactly the
convention the package is designed around. Export writes that same shape back,
so the loop closes: export → edit in Excel → re-upload → the scenarios return.
Import and export are two ends of one format rather than two features, and the
round trip is verified lossless (spend to the cent, `week` still a `Date`).

## Layout

```
mediaplanr-mvp-app/
  app.R                     # entry: libraries, sources, shinyApp()
  DESCRIPTION
  README.md
  PLAN.md                   # this file
  _brand.yml                # shared Ro-eh brand spec
  R/
    app_helpers.R           # formatting, column detection, empty states
    sample_data.R           # the built-in demo plan
    plan_store.R            # session registry: base plan + scenarios
    plan_io.R               # csv / multi-sheet xlsx readers
    grid_edit.R             # wide<->long pivot + editable reactable
    plan_charts.R           # the four comparison charts
    tool_wrappers.R         # plain functions the agent calls (.ok/.err)
    ellmer_tools.R          # typed tool defs + make_agent()
    ui.R / server.R
    ui_panels/     panel_{welcome,plan,scenarios,compare,export}.R
    server_panels/ server_{welcome,plan,scenarios,compare,export}.R
  prompts/system_prompt.md
  www/grid.js               # cell-edit shim
  data/sample_media_plan.csv
```

## Agent tools

Each wraps a `tool_wrappers.R` function returning `.ok()` / `.err()`, following
the sibling app.

| Tool | Maps to |
|---|---|
| `describe_plan` | grain, line items, weeks, totals of the active plan |
| `list_scenarios` | the set, with status and totals |
| `apply_edits` | `build_scenario(edits = <ops>)` — the important one |
| `compare_plans` | `compare_scenarios(level=)` |
| `roll_up_plan` | `roll_up()` to a coarser grain |
| `set_scenario_status` | status transitions |
| `plot_comparison` | a chart, returned as an inline image |

The `apply_edits` tool takes the operation list directly, so "increase Search by
20% in April" becomes `{target:{channel:"Search"}, scale:1.2}` and R does the
arithmetic exactly.

## Out of scope

Forecasting, optimization, model fitting (that is `mrmopt-mvp-app`);
authentication; persistence between sessions; multi-user state.
