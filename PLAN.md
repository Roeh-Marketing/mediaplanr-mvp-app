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
  BUILD                    EDIT                    EDIT                REVIEW
  sample / upload          edit                    fork                view
  ────────────────   ─────────────────   ──────────────────   ───────────────────
  csv or xlsx    →   editable grid   →   build_scenario()  →  one plan, or all
  column mapping     (excel-like)        (named)              summary + cell table
  media_plan_from_df chat ops            lineage tracked      4 charts, status
        ↑                                                            │
        └──────────────── same workbook format ──────────────────────┘
```

## Pages

| Page | Purpose |
|---|---|
| **Welcome** | What the app does and the four concepts that matter (plan = intent, line item, scenario, status). One click to load the sample. |
| **Build** | Two subtabs. *From a sample*: load, preview, build — no mapping, because a sample knows its own columns. *From a file*: upload csv/xlsx → map columns → build. Both fill the **registry** below the subtabs: one row per scenario with its metadata and an inline status editor. |
| **Edit** | The workbench. Editable grid (line items × weeks, Excel-style), a quick-op panel, and the assistant — three ways to change spend. Cells backed by a subplan are locked. Accumulated edits become a named scenario. |
| **Review** | Three subtabs. *Plan*: one plan whole — details, KPIs, a read-only grid, flighting, line items — and the status control. *Compare*: summary table, per-cell table and four charts across selected scenarios, plus an interactive "how spend moved". *Structure*: where each scenario came from (lineage), and what hangs beneath a plan (subplan tree, sunburst, tree table). |
| **Export** | A workbook (one sheet per scenario) that re-uploads cleanly, JSON for the whole set (the only lossless option once a plan has subplans), plus xlsx/CSV of the comparison or a single scenario. |

**Build and Edit change things; Review and Export read them.** The one thing
Review changes is status, which is deliberate: it used to be settable only at
build time and at save time — that is, only *before* you could see what you were
approving.

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

**5. Status is the scenario workflow, and it is set where the plan is visible.**
Every scenario carries `in development` / `to review` / `approved`, sourced from
`mediaplanr::status_levels()` so the dropdown and the validator can never
disagree. A plan is *born* `in development` — neither build route nor the save
form offers a status — and is promoted on Review → Plan, or inline in the
registry. Both go through `store_set_status()`, which is also what the agent's
`set_scenario_status` tool calls, so the UI and the assistant cannot disagree
about what approving means.

**5b. Each build route reads a SPEC, never `input$map_*` directly.** A navset
renders every panel into the DOM, so the two routes cannot share input ids, and
the sample route deliberately renders no mode / week / spend picker at all --
reading those inputs there would silently return `NULL` and take the wrong
branch. A sample therefore ships its own mapping as `uploaded()$defaults`, and
`do_build()` merges that with whichever controls the route actually shows.

**6. Two chart libraries, each for what it is good at.** This started as
"charts are static ggplot, because `plotly` is not installed". That reasoning
expired when `mediaplanr.viz` shipped: the companion builds Apache ECharts
specs from the projections `mediaplanr` already exports (`lineage()`,
`subplan_map()`, `ownership_map()`, `compare_scenarios()`), so a chart drawn
from one cannot drift from the package that defines it.

The split now:

* **ggplot**, in `plan_charts.R`, for the four comparison charts and for
  anything the *assistant* draws. A tool result has to be an image — an ECharts
  spec cannot be pasted into a chat turn — so `.plot_content()` keeps needing a
  PNG.
* **`mediaplanr.viz`**, for structure and movement: lineage, the subplan tree,
  the sunburst, and "how spend moved" on Compare. These are interactive because
  following one line item through a dense chart is what hover is for.

The companion's charts are not replacements for the four ggplots — they answer
different questions — so both stay.

**6b. A subplan is held, not copied — so the parent's cell is read-only.**
`attach_subplan()` replaces the parent's rows for a cell with the subplan's
rollup and locks them; `build_scenario()` then refuses that cell. The Edit grid
reads `@subplans` and renders those cells as values rather than inputs, with a
lock on the line item. Offering an input the package is going to reject is a
worse failure than not offering one.

Two consequences the app had to be taught:

* **Metadata edits go through `revise()`.** `store_set_status()` used to
  hand-rebuild a `MediaPlan` from a list of slots, which silently dropped
  `@subplans` and reset `@revision` to 1 — so approving a topline deleted the
  detail plans hanging off it. `revise()` copies the whole plan and changes only
  what it is asked to. Never enumerate slots.
* **A subplan is authored through the Design form, seeded from the parent.**
  `subplan_seed_recipe()` takes the parent's line item grain, the cell's own
  money and the parent's calendar, so what gets built is guaranteed to attach —
  rather than asking a planner to retype the cell and hoping it matches. Build
  then attaches instead of replacing the session, and says so in a banner first.

**6c. xlsx is lossy once a plan has structure; JSON is not.** A sheet is a
table and a table cannot hold a tree. `plan_to_json()` writes the whole set at
schema 2 and round-trips subplans, so it is the export offered whenever a
scenario is a topline — and the Export page names which ones would be flattened
rather than letting that be discovered on re-upload.

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
    app_helpers.R           # formatting, column detection, empty states,
                            #   the KPI strip and the registry table
    sample_data.R           # the built-in demo plan
    plan_store.R            # session registry: base plan + scenarios
    plan_io.R               # csv / multi-sheet xlsx readers
    grid_edit.R             # wide<->long pivot + editable reactable
    plan_charts.R           # the four comparison charts
    tool_wrappers.R         # plain functions the agent calls (.ok/.err)
    ellmer_tools.R          # typed tool defs + make_agent()
    ui.R / server.R
    ui_panels/     panel_{welcome,build,scenarios,review,preview,compare,export}.R
    server_panels/ server_{welcome,build,scenarios,preview,compare,export}.R
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
