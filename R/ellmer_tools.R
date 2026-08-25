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
      "operation is an object with an optional `target` plus EXACTLY ONE ",
      "operation key.\n\n",
      "Selecting rows:\n",
      "  target  - object of grain column -> value(s). Names any SUBSET of the ",
      "grain, so {\"channel\":\"TV\"} hits every TV row across all weeks. ",
      "Omit target to affect the whole plan. Values may be arrays.\n\n",
      "Changing spend. The first three act ACROSS the matched rows and hold ",
      "their existing mix, so a channel's flighting shape survives. The last ",
      "two act on EACH row and so multiply by the number of weeks matched -- ",
      "use them only when the user really means every week individually.\n",
      "  total      - make the matched rows SUM to this\n",
      "  delta      - move the matched rows' SUM by this (may be negative)\n",
      "  scale      - multiply each matched row (1.2 = +20%; same either way)\n",
      "  set        - absolute spend for EACH matched row\n",
      "  delta_each - add to EACH matched row (may be negative)\n\n",
      "A plan is weekly, so a channel is typically 13 or 26 rows. 'Take 50k out ",
      "of TV' is delta=-50000; delta_each=-50000 would take out 1.3 million. ",
      "Getting this wrong RECONCILES -- a transfer done with delta_each leaves ",
      "the plan total correct while both channels are wrong -- so prefer total ",
      "and delta.\n\n",
      "Examples:\n",
      "  'increase Search 20%'        -> [{\"target\":{\"channel\":\"Search\"},\"scale\":1.2}]\n",
      "  'set the budget to 2M'       -> [{\"total\":2000000}]\n",
      "  'cut the week of Apr 20 in half' -> [{\"target\":{\"week\":\"2026-04-20\"},\"scale\":0.5}]\n",
      "  'set the TV budget to 500k'  -> [{\"target\":{\"channel\":\"TV\"},\"total\":500000}]\n",
      "  'move 50k from TV to Social' -> [{\"target\":{\"channel\":\"TV\"},\"delta\":-50000},",
      "{\"target\":{\"channel\":\"Social\"},\"delta\":50000}]\n",
      "  'put 10k on every Hulu week' -> [{\"target\":{\"partner\":\"Hulu\"},\"set\":10000}]\n\n",
      "CHANGING WHICH ROWS THERE ARE. The five above change spend on rows that ",
      "already exist. Four more change the row set itself:\n",
      "  add     - introduce a line item. A named object of the line item ",
      "columns plus planned_spend, and either the week column or ",
      "flight_start/flight_end. Takes NO target: it names a row that does not ",
      "exist yet.\n",
      "  drop    - true. Removes the matched rows entirely. Not the same as ",
      "setting them to zero: a dropped line item is not bought at all.\n",
      "  shift   - a whole number of days, negative to move earlier. Moves the ",
      "matched BUYS, re-spreading each total across its new dates.\n",
      "  restage - {\"from\":\"YYYY-MM-DD\",\"to\":\"YYYY-MM-DD\"}. Moves the ",
      "matched buys to those dates. Needs flights.\n\n",
      "SELECTING BY DATE. Alongside `target`, any operation may carry ",
      "`during`: {\"from\":\"YYYY-MM-DD\",\"to\":\"YYYY-MM-DD\"}. It narrows to ",
      "rows whose in-market period OVERLAPS that window, so 'cut back April' ",
      "reaches a buy that started in March and is still running. It SELECTS ",
      "rows, never slices them: a buy half inside the window is matched whole.\n\n",
      "More examples:\n",
      "  'push the OOH buy a week later' -> [{\"target\":{\"channel\":\"OOH\"},\"shift\":7}]\n",
      "  'drop Search entirely'          -> [{\"target\":{\"channel\":\"Search\"},\"drop\":true}]\n",
      "  'add Audio at 25k in the week of Apr 6' -> ",
      "[{\"add\":{\"channel\":\"Audio\",\"partner\":\"Spotify\",\"week\":\"2026-04-06\",\"planned_spend\":25000}}]\n",
      "  'halve everything running in late April' -> ",
      "[{\"during\":{\"from\":\"2026-04-20\",\"to\":\"2026-04-30\"},\"scale\":0.5}]\n\n",
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
      "added and dropped line items show a real delta; level='flight' gives one ",
      "row per scenario x BUY, joined on flight identity, with a `change` column ",
      "naming what happened -- moved / resized / moved & resized / added / ",
      "dropped / repaced / unchanged. Use 'flight' when the user asks what ",
      "happened to a buy: 'cell' can only show money leaving one week and ",
      "arriving in another, which leaves the reader to infer that a single buy ",
      "moved. Only plans authored as flights have any."
    ),
    arguments = list(
      level = type_enum("Level of detail",
                        values = c("summary", "cell", "flight"),
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

  tool_list_flights <- tool(
    function(scenario = NULL) list_flights_tool(st, scenario),
    description = paste0(
      "List the BUYS a plan was authored from: line item, in-market dates, ",
      "total spend, how many weeks each covers, and its pacing (even, or custom ",
      "once someone has hand-shaped it). Only plans authored as flights have ",
      "any -- a plan written week by week reports none rather than guessing, ",
      "because four equal weeks could be one long buy or four separate ones. ",
      "Call this before shift or restage, which act on buys."
    ),
    arguments = list(
      scenario = type_string("Scenario label. Omit for the active one.",
                             required = FALSE)
    )
  )

  tool_calendar <- tool(
    function(basis = "month", scenario = NULL) calendar_view_tool(st, basis, scenario),
    description = paste0(
      "Re-cut a plan onto a different calendar and return the table: by day, by ",
      "week, or by month. Read-only -- it does not create a scenario. Spend ",
      "follows the days each row is actually in market, so a buy that starts ",
      "mid-week or straddles a month end is split correctly rather than landing ",
      "wholly in one period. Use it for 'what does this look like monthly'."
    ),
    arguments = list(
      basis    = type_enum("Calendar to cut onto",
                           values = c("month", "week", "day"), required = FALSE),
      scenario = type_string("Scenario label. Omit for the active one.",
                             required = FALSE)
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
    list_flights       = tool_list_flights,
    calendar_view      = tool_calendar,
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
  chat <- chat_anthropic(system_prompt = system_prompt,
                         credentials = function() list(`x-api-key` = api_key),
                         model = model)
  purrr::walk(make_plan_tools(st), function(t) chat$register_tool(t))
  chat
}
