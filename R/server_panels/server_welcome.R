# ---------------------------------------------------------------------------
# Welcome page: two shortcuts into the flow.
# ---------------------------------------------------------------------------

server_welcome <- function(input, output, session, st, bump, uploaded) {

  observeEvent(input$welcome_load_sample, {
    uploaded(list(name = "Q2 2026 Media Plan",
                  sheets = stats::setNames(list(sample_plan_df()), "baseline")))
    updateTextInput(session, "meta_name", value = "Q2 2026 Media Plan")
    updateTextInput(session, "meta_nickname", value = "baseline")
    updateTextInput(session, "meta_advertiser", value = "Acme Corp")
    nav_select("main_nav", "Plan")
    showNotification("Sample loaded — the mapping is pre-filled, press Build plan.",
                     type = "message", duration = 6)
  })

  observeEvent(input$welcome_goto_plan, nav_select("main_nav", "Plan"))

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
