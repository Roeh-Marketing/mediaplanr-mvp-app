# ---------------------------------------------------------------------------
# Review -> Plan: one plan, read only, and the status control.
#
# Status is a LABEL, not an edit, so it goes through store_set_status() -- the
# same function the assistant's set_scenario_status tool calls -- which rebuilds
# the plan with its id and parent_id intact rather than forking the lineage.
# ---------------------------------------------------------------------------

server_preview <- function(input, output, session, st, bump) {

  observeEvent(bump(), {
    if (!store_has_plan(st)) return()
    nms <- store_scenario_names(st)
    sel <- if (isTRUE(input$pv_scenario %in% nms)) input$pv_scenario else st$set@base_name
    updateSelectInput(session, "pv_scenario", choices = nms, selected = sel)
  })

  pv_plan <- reactive({
    bump(); req(store_has_plan(st))
    nm <- input$pv_scenario
    if (is.null(nm) || !nm %in% store_scenario_names(st)) nm <- st$set@base_name
    store_get(st, nm)
  })

  # The status dropdown follows whichever plan is on screen, so it always shows
  # where that plan actually is rather than where the last one was.
  observeEvent(pv_plan(), {
    p <- pv_plan()
    updateSelectInput(session, "pv_status",
                      choices  = mediaplanr::status_levels(),
                      selected = if (nzchar(p@status)) p@status else "in development")
  })

  output$pv_status_now <- renderUI({
    if (!store_has_plan(st)) return(NULL)
    p <- pv_plan()
    span(class = "small text-muted", "Currently ", status_badge(p@status))
  })

  observeEvent(input$pv_status_apply, {
    if (!store_has_plan(st)) {
      showNotification("Build a plan first.", type = "warning"); return()
    }
    nm <- input$pv_scenario; new <- input$pv_status
    req(nzchar(nm %||% ""), nzchar(new %||% ""))
    res <- tryCatch(store_set_status(st, nm, new), error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 8); return()
    }
    bump(bump() + 1)
    showNotification(sprintf("'%s' is now %s.", nm, new), type = "message", duration = 4)
  })

  # --- outputs ------------------------------------------------------------
  output$pv_kpis <- renderUI({
    bump()
    if (!store_has_plan(st)) {
      return(card(card_body(empty_state(
        "Build a plan on the Build tab first.", "file-arrow-up"))))
    }
    plan_kpi_strip(pv_plan())
  })

  output$pv_meta <- renderUI({
    if (!store_has_plan(st)) return(empty_state("Nothing to show yet.", "tag"))
    p <- pv_plan()
    row <- function(k, v) {
      if (is.null(v) || !length(v) || !nzchar(as.character(v))) v <- "—"
      tags$div(class = "d-flex justify-content-between border-bottom py-1",
               tags$span(class = "text-muted small", k),
               tags$span(class = "small", v))
    }
    lineage <- if (length(p@parent_id) && nzchar(p@parent_id)) {
      parent <- Filter(function(q) identical(q@id, p@parent_id), st$set@scenarios)
      paste0("derived from ",
             if (length(parent)) names(parent)[1] else mediaplanr::short_id(p@parent_id))
    } else "baseline"

    tagList(
      div(class = "mb-2", status_badge(p@status)),
      row("Name", p@name),
      row("Nickname", p@nickname),
      row("Advertiser", p@advertiser),
      row("Planner", p@planner),
      row("Objective", p@objective),
      row("Lineage", lineage),
      row("Plan id", mediaplanr::short_id(p@id))
    )
  })

  output$pv_flight <- renderPlot({
    bump(); req(store_has_plan(st))
    nm <- input$pv_scenario
    if (is.null(nm) || !nm %in% store_scenario_names(st)) nm <- st$set@base_name
    p <- chart_flighting(st$set, nm)
    if (is.null(p)) {
      return(ggplot2::ggplot() + ggplot2::annotate(
        "text", 1, 1, label = "This plan has no time dimension", colour = "grey55") +
        ggplot2::theme_void())
    }
    p
  }, bg = "transparent")

  output$pv_grid <- reactable::renderReactable({
    req(store_has_plan(st))
    plan_grid(pv_plan(), editable = FALSE)
  })

  output$pv_items <- reactable::renderReactable({
    req(store_has_plan(st))
    tbl <- mediaplanr::line_item_summary(pv_plan())
    reactable::reactable(
      tbl, compact = TRUE, bordered = TRUE, highlight = TRUE,
      defaultPageSize = 10, defaultSorted = list(planned_spend = "desc"),
      columns = list(
        line_item     = reactable::colDef(show = FALSE),
        planned_spend = reactable::colDef(name = "Planned spend", align = "right",
                                          cell = function(v) fmt_money(v)),
        n_rows        = reactable::colDef(name = "Rows", align = "right", maxWidth = 80)
      ),
      style = list(fontSize = "0.82rem"))
  })
}
