library(shiny)

# ---------------------------------------------------------------------------
# Top-level server.
#
# Owns the session's plan store and delegates each tab to a module. The store is
# a plain environment rather than a reactiveVal because the ellmer tools mutate
# it from outside the reactive graph; `bump` is the invalidation signal that
# tells Shiny something in it changed. Anything that writes to the store must
# bump afterwards.
#
# It also owns the CHAT. The assistant lives in a drawer on the navbar rather
# than on any one page, so there is one conversation for the whole session --
# a plan described while designing it is still in context when you go to edit
# it. That makes this the only place with all three of `st`, `bump` and
# `agent` in scope, which is exactly what the chat handler needs.
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
  server_build(input, output, session, st, bump, uploaded)
  server_scenarios(input, output, session, st, bump)
  server_preview(input, output, session, st, bump)
  server_compare(input, output, session, st, bump)
  server_structure(input, output, session, st, bump)
  server_export(input, output, session, st, bump)

  # --- the assistant ------------------------------------------------------

  save_plot <- make_chat_plot_saver(session)

  # What the drawer says above the transcript. It changes with the page,
  # because "what can I ask for here" is a different question on Build than on
  # Review -- but the conversation underneath is the same one.
  output$drawer_hint <- renderUI({
    if (is.null(agent())) {
      return(span(class = "text-danger",
                  "No ANTHROPIC_API_KEY set - everything else still works."))
    }
    switch(input$main_nav %||% "",
      Build  = "Describe the plan you want and I will fill in the Design form.",
      Edit   = "Ask for a change and I will save it as a new scenario.",
      Review = "Ask what changed between scenarios.",
      "Ask about the plan, or tell me what to build.")
  })

  # One way in, whether the text came from the composer or from a skill chip.
  send_to_agent <- function(text) {
    a <- agent()
    if (is.null(a)) {
      chat_append("plan_chat", paste(
        "The assistant needs an `ANTHROPIC_API_KEY` environment variable.",
        "Set it and restart the app; meanwhile the Build form, the grid and",
        "the quick operations all work as normal."))
      return()
    }
    n0 <- length(a$get_turns())
    stream <- a$stream_async(text)
    promises::then(
      chat_append("plan_chat", stream),
      onFulfilled = function(value) {
        # The agent may have built a plan, created scenarios or rewritten the
        # scaffold; refresh everything that reads the store.
        bump(bump() + 1)
        for (uri in extract_chat_images(a, from_turn = n0 + 1L)) {
          url <- save_plot(uri)
          if (!is.null(url)) {
            chat_append("plan_chat", sprintf(
              '<img src="%s" alt="chart" style="max-width:100%%;height:auto;border-radius:8px;">',
              url))
          }
        }
      })
  }

  observeEvent(input$plan_chat_user_input, send_to_agent(input$plan_chat_user_input))

  # A skill chip is a shortcut, not a separate feature: it opens the drawer and
  # sends the skill's own example, so what the chips advertise and what the
  # assistant then does cannot come apart.
  observeEvent(input$skill_pick, {
    s <- skill_by_id(input$skill_pick$id)
    req(s)
    session$sendCustomMessage("mp_open_drawer", list())
    chat_append("plan_chat", s$example, role = "user")
    send_to_agent(s$example)
  })
}
