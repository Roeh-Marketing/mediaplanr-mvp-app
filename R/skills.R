# ---------------------------------------------------------------------------
# What the assistant can do, declared once.
#
# A skill is a named capability with a blurb and an example phrasing. The same
# list is rendered three ways -- as chips on a page, as a block in the system
# prompt, and as the `list_skills` tool -- so what the UI advertises and what
# the model says it can do cannot drift apart. Same discipline as
# mediaplanr::status_levels() feeding both the dropdown and the validator.
#
# The chips matter because the chat lives in a drawer that starts closed: a
# planner who has never used the app needs to see what is on offer without
# opening anything first. Clicking a chip opens the drawer with that example
# already sent.
# ---------------------------------------------------------------------------

plan_skills <- function() list(
  list(
    id      = "design_plan",
    label   = "Design a plan",
    icon    = "wand-magic-sparkles",
    where   = "build",
    blurb   = paste("Start from nothing. I ask how time works, which dimensions",
                    "identify a line item, and what the budget is, then fill in",
                    "the Design a plan form for you."),
    example = "Design a Q3 plan: 13 weeks from 6 July, channel and partner, 1.2m.",
    tools   = c("get_scaffold", "set_scaffold", "suggest_dimensions",
                "preview_scaffold", "build_from_scaffold")
  ),
  list(
    id      = "explain_columns",
    label   = "Explain the columns",
    icon    = "table-columns",
    where   = "build",
    blurb   = paste("Which dimensions are conventional (channel, partner,",
                    "campaign, target audience), and what a plan records",
                    "besides spend -- unit type, units, rate."),
    example = "What dimensions and columns should a plan like this have?",
    tools   = c("suggest_dimensions")
  ),
  list(
    id      = "seed_budget",
    label   = "Seed a budget",
    icon    = "coins",
    where   = "build",
    blurb   = paste("Split a total across the plan by share or weight, and",
                    "shape each line item over its weeks -- flat, ramp,",
                    "front-loaded, back-loaded or bursts."),
    example = "Split 2m: TV 55, Search 25, Social 20. Ramp Search, front-load TV.",
    tools   = c("set_scaffold", "preview_scaffold")
  ),
  list(
    id      = "edit_spend",
    label   = "Change the spend",
    icon    = "pen-to-square",
    where   = "edit",
    blurb   = paste("Move money around an existing plan and save the result as",
                    "a named scenario. Add or drop line items, shift a buy,",
                    "restage it."),
    example = "Move 50k from TV into Social in May, and call it 'Social push'.",
    tools   = c("describe_plan", "apply_edits", "list_flights")
  ),
  list(
    id      = "explain_plan",
    label   = "Explain a plan",
    icon    = "magnifying-glass-chart",
    where   = "edit",
    blurb   = paste("Describe what is in a plan, compare scenarios, re-cut it",
                    "onto a monthly or daily calendar, or draw a chart."),
    example = "What changed between the baseline and my latest scenario?",
    tools   = c("describe_plan", "compare_plans", "calendar_view",
                "plot_comparison", "roll_up_plan")
  )
)

skill_by_id <- function(id) {
  for (s in plan_skills()) if (identical(s$id, id)) return(s)
  NULL
}

# --- 1. as chips on a page ---------------------------------------------------
#
# Buttons carry `data-skill`; www/grid.js delegates the click to
# `input$skill_pick`, the same shim the editable cells and the registry's
# status dropdowns ride on.
skills_strip <- function(where = NULL) {
  ss <- plan_skills()
  if (!is.null(where)) ss <- Filter(function(s) identical(s$where, where), ss)
  if (!length(ss)) return(NULL)

  div(
    class = "mp-skills d-flex flex-wrap gap-2",
    lapply(ss, function(s) {
      tags$button(
        type = "button", class = "mp-skill", `data-skill` = s$id,
        title = s$blurb,
        icon(s$icon), tags$span(class = "mp-skill-label", s$label)
      )
    })
  )
}

# --- 2. as a block in the system prompt --------------------------------------
skills_prompt_block <- function() {
  lines <- vapply(plan_skills(), function(s) {
    sprintf("- **%s** - %s\n  Example: *%s*", s$label, s$blurb, s$example)
  }, character(1))
  paste(c("These are the things you can do, and what the app advertises you",
          "can do. When someone asks what you can help with, answer from this",
          "list in your own words.", "", lines), collapse = "\n")
}

# --- 3. as the list_skills tool ----------------------------------------------
skills_for_tool <- function() {
  lapply(plan_skills(), function(s) {
    list(skill = s$label, does = s$blurb, example = s$example,
         tools = paste(s$tools, collapse = ", "))
  })
}
