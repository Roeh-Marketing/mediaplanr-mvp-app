library(ellmer)

# ---------------------------------------------------------------------------
# Typed tool definitions. Each closes over the session's plan store, so the
# agent and the UI operate on the same objects.
# ---------------------------------------------------------------------------

make_plan_tools <- function(st) {

  tool_describe <- tool(
    function(scenario = NULL) describe_plan_tool(st, scenario),
    description = paste0(
      "Describe a plan: its grain, line items, weeks, status, and spend by ",
      "line item. Call this FIRST, before any edit, so you use the exact ",
      "line item and week values the plan actually contains."
    ),
    arguments = list(
      scenario = type_string(
        "Scenario label to describe. Omit for the one currently being edited.",
        required = FALSE)
    )
  )

  tool_list <- tool(
    function() list_scenarios_tool(st),
    description = paste0(
      "List every scenario in the set with its status, total spend and delta ",
      "versus the baseline. Use it to find the exact label to pass elsewhere."
    )
  )

  tool_apply_edits <- tool(
    function(edits_json, name, nickname = "", status = "in development",
             objective = "", from_scenario = NULL) {
      apply_edits_tool(st, edits_json, name, nickname, status, objective,
                       from_scenario)
    },
    description = paste0(
      "Create a new scenario by changing planned spend. This is the main ",
      "editing tool.\n\n",
      "`edits_json` is a JSON ARRAY of operations applied in order. Each ",
      "operation is an object with an optional `target` plus EXACTLY ONE of ",
      "set / scale / delta / total:\n",
      "  target  - object of grain column -> value(s). Names any SUBSET of the ",
      "grain, so {\"channel\":\"TV\"} hits every TV row across all weeks. ",
      "Omit target to affect the whole plan. Values may be arrays.\n",
      "  set     - absolute spend for EACH matched row\n",
      "  scale   - multiply each matched row (1.2 = +20%)\n",
      "  delta   - add to each matched row (may be negative)\n",
      "  total   - make the matched rows SUM to this, holding their current mix\n\n",
      "Note set is per-row while total is across rows: with target ",
      "{\"channel\":\"TV\"}, set=50000 puts 50000 on every TV row, total=50000 ",
      "makes all TV rows add up to 50000.\n\n",
      "Examples:\n",
      "  'increase Search 20%'        -> [{\"target\":{\"channel\":\"Search\"},\"scale\":1.2}]\n",
      "  'set the budget to 2M'       -> [{\"total\":2000000}]\n",
      "  'cut the week of Apr 20 in half' -> [{\"target\":{\"week\":\"2026-04-20\"},\"scale\":0.5}]\n",
      "  'move 50k from TV to Social' -> [{\"target\":{\"channel\":\"TV\"},\"delta\":-50000},",
      "{\"target\":{\"channel\":\"Social\"},\"delta\":50000}]\n\n",
      "NEVER compute the arithmetic yourself and pass absolute numbers when the ",
      "user asked for a relative change -- state the operation and let R do it. ",
      "Targeting a value that does not exist is an error naming the valid ",
      "options, so call describe_plan first if unsure."
    ),
    arguments = list(
      edits_json    = type_string("JSON array of edit operations (see description)"),
      name          = type_string("Formal name for the new scenario. Required."),
      nickname      = type_string(
        "Short working handle shown in the scenario list, e.g. 'TV -20%'. Recommended.",
        required = FALSE),
      status        = type_enum("Workflow status for the new scenario",
        values = c("in development", "to review", "approved"), required = FALSE),
      objective     = type_string("Optional note on what this scenario is testing",
                                  required = FALSE),
      from_scenario = type_string(
        "Scenario to derive from. Omit to use the one currently being edited.",
        required = FALSE)
    )
  )

  tool_compare <- tool(
    function(level = "summary") compare_plans_tool(st, level),
    description = paste0(
      "Compare the scenarios. level='summary' gives one row per scenario ",
      "(total spend, delta and % vs baseline); level='cell' gives one row per ",
      "scenario x grain cell over the union of cells, zero-filled, so both ",
      "added and dropped line items show a real delta."
    ),
    arguments = list(
      level = type_enum("Level of detail", values = c("summary", "cell"),
                        required = FALSE)
    )
  )

  tool_set_status <- tool(
    function(scenario, status) set_status_tool(st, scenario, status),
    description = paste0(
      "Set a scenario's workflow status. Changing a label is not a new ",
      "scenario, so this edits in place rather than deriving one."
    ),
    arguments = list(
      scenario = type_string("Scenario label"),
      status   = type_enum("New status",
                           values = c("in development", "to review", "approved"))
    )
  )

  tool_roll_up <- tool(
    function(grain, scenario = NULL) roll_up_tool(st, grain, scenario),
    description = paste0(
      "Aggregate a plan to a coarser grain, e.g. channel totals from a ",
      "channel+partner+week plan. Read-only: it does not create a scenario."
    ),
    arguments = list(
      grain    = type_array("Grain columns to roll up to; must be a subset of the plan grain",
                            items = type_string("column name")),
      scenario = type_string("Scenario label. Omit for the active one.", required = FALSE)
    )
  )

  tool_set_active <- tool(
    function(scenario) set_active_tool(st, scenario),
    description = "Switch which scenario the Scenarios page is editing.",
    arguments = list(scenario = type_string("Scenario label"))
  )

  tool_plot <- tool(
    function(which = "totals", scenario = NULL) plot_comparison_tool(st, which, scenario),
    description = paste0(
      "Draw a comparison chart and show it in the chat. ",
      "totals = total spend per scenario; mix = spend by channel; ",
      "flighting = spend by week; deltas = per-line-item change versus the ",
      "baseline for one scenario (pass `scenario`)."
    ),
    arguments = list(
      which    = type_enum("Chart to draw",
                           values = c("totals", "mix", "flighting", "deltas"),
                           required = FALSE),
      scenario = type_string("Scenario for the deltas chart", required = FALSE)
    )
  )

  list(
    describe_plan      = tool_describe,
    list_scenarios     = tool_list,
    apply_edits        = tool_apply_edits,
    compare_plans      = tool_compare,
    set_scenario_status = tool_set_status,
    roll_up_plan       = tool_roll_up,
    set_active_scenario = tool_set_active,
    plot_comparison    = tool_plot
  )
}

# ---------------------------------------------------------------------------
# Agent factory
# ---------------------------------------------------------------------------

make_plan_agent <- function(
    st,
    api_key     = Sys.getenv("ANTHROPIC_API_KEY"),
    model       = "claude-sonnet-4-6",
    prompt_path = file.path(getwd(), "prompts", "system_prompt.md")
) {
  if (!nzchar(api_key)) return(NULL)   # chat degrades to a hint in the UI

  system_prompt <- paste(readLines(prompt_path, warn = FALSE), collapse = "\n")
  chat <- chat_anthropic(system_prompt = system_prompt, api_key = api_key,
                         model = model)
  purrr::walk(make_plan_tools(st), function(t) chat$register_tool(t))
  chat
}
