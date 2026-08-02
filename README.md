# mediaplanr-mvp-app

A Shiny workbench for **building, editing and comparing media plans**, powered
by the [`mediaplanr`](../mediaplanr) package.

Sibling to `mrmopt-mvp-app`, which does the modelling. This one does intent:
get a plan in, reshape it, fork scenarios, see how they differ. No forecasting,
no budget optimisation.

## Run it

```r
# mediaplanr must be installed:
#   R CMD INSTALL ../mediaplanr
shiny::runApp(".")
```

The assistant needs `ANTHROPIC_API_KEY`. Without it the chat says so and
everything else works normally.

## The flow

1. **Plan** — upload a CSV/Excel plan or load the sample, map the columns to
   grain / week / spend, add the plan's details, build it.
2. **Scenarios** — change spend three ways: type in the grid, stage a quick
   operation, or ask the assistant. Save the result as a named scenario.
3. **Compare** — summary table, per-cell table, and four charts.
4. **Export** — a workbook that re-uploads cleanly, or xlsx/CSV of the
   comparison and individual scenarios.

## Three ways to edit, one code path

| Where | Produces | Good for |
|---|---|---|
| The grid | absolute cell values | "this week for this partner is 45,000" |
| Quick operation | an operation | "scale all TV by 0.7" |
| The assistant | an operation | "move 50k from TV into Social in May" |

All three end up in `mediaplanr::build_scenario(edits=)`. The app does no
arithmetic of its own — which matters most for the assistant, where a model
doing the sums is the way you get a confidently wrong budget.

Grid edits and staged operations **accumulate** and only become a scenario when
you press Save. Otherwise every keystroke would mint a plan and a lineage chain
hundreds deep.

## Notable implementation details

**The grid is a `reactable` with `<input>` cells.** `DT` and `rhandsontable`
aren't installed, and adding a dependency for one widget isn't worth it.
`www/grid.js` delegates change events from `document` — reactable re-creates
cell nodes on every render, so per-input listeners would silently die.

**Cells are keyed by line item + week**, never by row index, so an edit survives
sorting, filtering and re-rendering.

**Sticky columns are opaque.** A translucent frozen column lets the scrolling
body show through it, which reads as overlapping text.

**Excel round-trips.** A workbook loads with the filename as the plan's formal
name and each sheet as a scenario (sheet name → nickname). Export writes that
same shape back, so you can hand a colleague a workbook, let them edit it in
Excel, and upload it straight back. Verified lossless: spend to the cent, and
`week` survives as a `Date`.

## Layout

```
app.R                     entry point
R/
  app_helpers.R           formatting, column guessing, chat plumbing
  sample_data.R           the built-in demo plan
  plan_store.R            session store (a ScenarioSet)
  plan_io.R               csv / xlsx readers, mapping validation
  grid_edit.R             wide<->long pivot, editable reactable
  plan_charts.R           the four comparison charts
  tool_wrappers.R         functions the assistant calls
  ellmer_tools.R          typed tool defs + agent factory
  ui.R / server.R
  ui_panels/ server_panels/
prompts/system_prompt.md
www/grid.js
data/sample_media_plan.csv
```

See [PLAN.md](PLAN.md) for the design reasoning.
