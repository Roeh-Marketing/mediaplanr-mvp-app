library(shiny)

# ---------------------------------------------------------------------------
# Top-level server.
#
# Owns the session's plan store and delegates each tab to a module. The store is
# a plain environment rather than a reactiveVal because the ellmer tools mutate
# it from outside the reactive graph; `bump` is the invalidation signal that
# tells Shiny something in it changed. Anything that writes to the store must
# bump afterwards.
# ---------------------------------------------------------------------------

app_server <- function(input, output, session) {

  st   <- new_plan_store()          # session-scoped plan store
  bump <- reactiveVal(0L)           # increment to re-read the store
  uploaded <- reactiveVal(NULL)     # list(name=, sheets=) from the last file

  # One agent per session, closing over this session's store so anything the
  # assistant does lands in the same place the UI reads from.
  agent <- reactiveVal(NULL)
  observe({
    if (!is.null(agent())) return()
    a <- tryCatch(make_plan_agent(st), error = function(e) {
      warning("Could not start the assistant: ", conditionMessage(e))
      NULL
    })
    agent(a)
  })

  server_welcome(input, output, session, st, bump, uploaded)
  server_plan(input, output, session, st, bump, uploaded)
  server_scenarios(input, output, session, st, bump, agent)
  server_compare(input, output, session, st, bump)
  server_export(input, output, session, st, bump)
}
