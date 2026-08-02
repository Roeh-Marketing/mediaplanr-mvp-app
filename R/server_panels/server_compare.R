# ---------------------------------------------------------------------------
# Compare page: tables and charts across the selected scenarios.
# ---------------------------------------------------------------------------

server_compare <- function(input, output, session, st, bump) {

  # Scenarios already offered to the user, so a newly created one can be told
  # apart from one they deliberately unticked.
  seen <- reactiveVal(character(0))

  # Keep the selectors in step with the store. The user's choice is preserved,
  # but a scenario that has just appeared is selected automatically -- someone
  # who just saved one expects to see it, and leaving it unticked makes the
  # Compare page look like nothing happened.
  observeEvent(bump(), {
    if (!store_has_plan(st)) return()
    nms <- store_scenario_names(st)
    fresh <- setdiff(nms, seen())
    keep <- union(intersect(input$cmp_scenarios %||% character(0), nms), fresh)
    if (!length(keep)) keep <- nms
    seen(nms)
    updateCheckboxGroupInput(session, "cmp_scenarios", choices = nms, selected = keep)

    derived <- setdiff(nms, st$set@base_name)
    updateSelectInput(session, "cmp_delta_scn",
                      choices = if (length(derived)) derived else nms,
                      selected = if (length(derived))
                        (if (input$cmp_delta_scn %in% derived) input$cmp_delta_scn
                         else utils::tail(derived, 1)) else nms[1])
  })

  observeEvent(input$cmp_all, {
    updateCheckboxGroupInput(session, "cmp_scenarios",
                             selected = store_scenario_names(st))
  })
  observeEvent(input$cmp_none, {
    updateCheckboxGroupInput(session, "cmp_scenarios", selected = character(0))
  })

  selected <- reactive({
    bump(); req(store_has_plan(st))
    s <- input$cmp_scenarios
    if (!length(s)) return(character(0))
    intersect(store_scenario_names(st), s)
  })

  has_any <- reactive(store_has_plan(st) && length(selected()) > 0)

  # --- KPI strip ---------------------------------------------------------
  output$cmp_kpis <- renderUI({
    bump()
    if (!store_has_plan(st)) {
      return(card(card_body(empty_state("Build a plan first.", "chart-simple"))))
    }
    s <- mediaplanr::compare_scenarios(st$set, "summary")
    sel <- selected()
    s <- s[s$scenario %in% sel, , drop = FALSE]
    if (!nrow(s)) {
      return(card(card_body(empty_state("Select at least one scenario.", "list-check"))))
    }
    rng <- range(s$total_planned_spend)
    kpi <- function(label, value, sub = NULL) {
      card(class = "border-0 shadow-sm",
           card_body(class = "py-3",
                     div(class = "kpi-label", label),
                     div(class = "kpi-value", value),
                     if (!is.null(sub)) div(class = "small text-muted", sub)))
    }
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      kpi("Scenarios", nrow(s)),
      kpi("Baseline", fmt_money_short(
        s$total_planned_spend[s$scenario == st$set@base_name][1] %||% NA)),
      kpi("Spread", fmt_money_short(diff(rng)),
          paste0(fmt_money_short(rng[1]), " – ", fmt_money_short(rng[2]))),
      kpi("Approved", sum(vapply(st$set@scenarios[s$scenario],
                                 function(p) p@status == "approved", logical(1))))
    )
  })

  # --- summary table -----------------------------------------------------
  output$cmp_summary <- reactable::renderReactable({
    req(has_any())
    s <- mediaplanr::compare_scenarios(st$set, "summary")
    s <- s[s$scenario %in% selected(), , drop = FALSE]
    s$status <- vapply(st$set@scenarios[s$scenario], function(p) p@status, character(1))
    s$nickname <- vapply(st$set@scenarios[s$scenario], function(p) p@nickname, character(1))
    s$is_base <- s$scenario == st$set@base_name

    tbl <- data.frame(
      Scenario = s$scenario,
      Name     = vapply(st$set@scenarios[s$scenario], function(p) p@name, character(1)),
      Status   = s$status,
      Spend    = s$total_planned_spend,
      `vs base` = s$spend_vs_base,
      `%`      = s$spend_pct_vs_base,
      check.names = FALSE, stringsAsFactors = FALSE)

    reactable::reactable(
      tbl, compact = TRUE, bordered = TRUE, highlight = TRUE, pagination = FALSE,
      defaultSorted = list(Spend = "desc"),
      columns = list(
        Scenario = reactable::colDef(minWidth = 130, style = list(fontWeight = 600)),
        Name     = reactable::colDef(minWidth = 180,
                                     style = list(color = "#6c757d", fontSize = "0.78rem")),
        Status   = reactable::colDef(minWidth = 120, cell = function(v) {
          cls <- switch(v, approved = "background:#DFF0D8;color:#3B6D11",
                        `to review` = "background:#FCF3D6;color:#8a6d1f",
                        "background:#EEE;color:#555")
          htmltools::tags$span(
            style = paste0(cls, ";padding:2px 8px;border-radius:10px;font-size:0.72rem"), v)
        }),
        Spend    = reactable::colDef(align = "right", cell = function(v) fmt_money(v)),
        `vs base` = reactable::colDef(align = "right", cell = function(v) {
          if (v == 0) return("—")
          htmltools::tags$span(style = paste0("color:", if (v > 0) "#3B6D11" else "#A32D2D"),
                               fmt_delta(v))
        }),
        `%`      = reactable::colDef(align = "right", cell = function(v) fmt_pct(v))
      ),
      style = list(fontSize = "0.84rem"))
  })

  # --- charts ------------------------------------------------------------
  blank <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", 1, 1, label = msg, colour = "grey55", size = 4) +
      ggplot2::theme_void()
  }

  output$cmp_plot_totals <- renderPlot({
    if (!has_any()) return(blank("Select scenarios to compare"))
    chart_totals(st$set, selected())
  }, bg = "transparent")

  output$cmp_plot_mix <- renderPlot({
    if (!has_any()) return(blank("Select scenarios to compare"))
    chart_mix(st$set, selected())
  }, bg = "transparent")

  output$cmp_plot_flight <- renderPlot({
    if (!has_any()) return(blank("Select scenarios to compare"))
    p <- chart_flighting(st$set, selected())
    if (is.null(p)) blank("This plan has no week column") else p
  }, bg = "transparent")

  output$cmp_plot_deltas <- renderPlot({
    if (!has_any()) return(blank("Select scenarios to compare"))
    scn <- input$cmp_delta_scn
    if (is.null(scn) || !nzchar(scn)) return(blank("No derived scenario yet"))
    p <- chart_deltas(st$set, scn)
    if (is.null(p)) blank("No differences against the baseline") else p
  }, bg = "transparent")

  # --- cell table --------------------------------------------------------
  output$cmp_cells <- reactable::renderReactable({
    req(has_any())
    cmp <- mediaplanr::compare_scenarios(st$set, "cell")
    cmp <- cmp[cmp$scenario %in% selected(), , drop = FALSE]

    val_col <- switch(input$cmp_cell_view %||% "spend",
                      spend = "planned_spend",
                      delta = "spend_vs_base",
                      share = "share_of_total")

    g <- st$set@grain
    keep <- c("scenario", g, val_col)
    tbl <- cmp[, keep, drop = FALSE]
    names(tbl)[names(tbl) == val_col] <- "value"

    fmt <- switch(input$cmp_cell_view %||% "spend",
                  spend = function(v) fmt_money(v),
                  delta = function(v) if (isTRUE(v == 0)) "—" else fmt_delta(v),
                  share = function(v) if (is.na(v)) "—" else paste0(round(v * 100, 1), "%"))

    reactable::reactable(
      tbl, compact = TRUE, bordered = TRUE, highlight = TRUE,
      defaultPageSize = 12, filterable = TRUE,
      columns = c(
        list(scenario = reactable::colDef(name = "Scenario", minWidth = 130,
                                          style = list(fontWeight = 500))),
        stats::setNames(lapply(g, function(x) reactable::colDef(minWidth = 100)), g),
        list(value = reactable::colDef(
          name = switch(input$cmp_cell_view %||% "spend",
                        spend = "Planned spend", delta = "vs base", share = "Share"),
          align = "right", cell = fmt))
      ),
      style = list(fontSize = "0.8rem"))
  })
}
