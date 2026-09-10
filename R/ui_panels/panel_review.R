# ---------------------------------------------------------------------------
# Review: the read-only half of the app.
#
# One plan at a time, all of them side by side, or how they relate. No subtab
# here edits spend; the only thing that changes is a plan's status.
# ---------------------------------------------------------------------------

nav_panel(
  title = "Review",
  div(
    class = "container-fluid py-3",
    navset_card_tab(
      id = "review_mode",
      source("R/ui_panels/panel_preview.R",   local = TRUE)$value,
      source("R/ui_panels/panel_compare.R",   local = TRUE)$value,
      source("R/ui_panels/panel_structure.R", local = TRUE)$value
    )
  )
)
