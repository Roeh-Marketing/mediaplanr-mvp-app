# ---------------------------------------------------------------------------
# Welcome page: two shortcuts into the flow.
# ---------------------------------------------------------------------------

server_welcome <- function(input, output, session, st, bump, uploaded) {

  # Both buttons land on Build, on the subtab that matches what was asked for.
  # The sample itself is loaded by server_build's own observer, so there is one
  # definition of what "the sample" is rather than two that can drift.
  observeEvent(input$welcome_load_sample, {
    load_sample(session, uploaded, "weekly")
    nav_select("main_nav", "Build")
    nav_select("build_mode", "From a sample")
    showNotification("Sample loaded — check the preview, then Build plan.",
                     type = "message", duration = 6)
  })

  observeEvent(input$welcome_goto_plan, {
    nav_select("main_nav", "Build")
    nav_select("build_mode", "From a file")
  })

  output$welcome_hint <- renderUI({
    bump()
    if (!store_has_plan(st)) return(NULL)
    span(class = "small text-muted ms-2",
         icon("circle-check", class = "text-success"), " ",
         sprintf("%d scenario%s loaded", length(store_scenario_names(st)),
                 if (length(store_scenario_names(st)) == 1) "" else "s"))
  })

  output$nav_status <- renderUI({
    bump()
    if (!store_has_plan(st)) {
      return(span(class = "navbar-text small opacity-75", "no plan loaded"))
    }
    p <- store_base(st)
    span(class = "navbar-text small",
         tags$b(p@name),
         span(class = "opacity-75",
              sprintf(" · %d scenario%s", length(store_scenario_names(st)),
                      if (length(store_scenario_names(st)) == 1) "" else "s")))
  })
}
