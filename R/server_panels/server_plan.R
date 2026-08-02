# ---------------------------------------------------------------------------
# Plan page: source -> column mapping -> build the base MediaPlan.
# ---------------------------------------------------------------------------

server_plan <- function(input, output, session, st, bump, uploaded) {

  # --- source -----------------------------------------------------------
  observeEvent(input$plan_upload, {
    req(input$plan_upload)
    res <- tryCatch(
      read_plan_file(input$plan_upload$datapath, input$plan_upload$name),
      error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    uploaded(res)
    updateTextInput(session, "meta_name", value = res$name)
    nm <- names(res$sheets)
    if (length(nm) && nzchar(nm[1])) {
      updateTextInput(session, "meta_nickname", value = nm[1])
    }
    showNotification(
      sprintf("Loaded %s (%d sheet%s).", input$plan_upload$name,
              length(res$sheets), if (length(res$sheets) == 1) "" else "s"),
      type = "message")
  })

  observeEvent(input$plan_load_sample, {
    uploaded(list(name = "Q2 2026 Media Plan",
                  sheets = stats::setNames(list(sample_plan_df()), "baseline")))
    updateTextInput(session, "meta_name", value = "Q2 2026 Media Plan")
    updateTextInput(session, "meta_nickname", value = "baseline")
    updateTextInput(session, "meta_advertiser", value = "Acme Corp")
    showNotification("Sample plan loaded — check the mapping, then Build plan.",
                     type = "message")
  })

  # The first sheet drives the column mapping; every sheet must share it.
  first_sheet <- reactive({
    u <- uploaded(); req(u)
    u$sheets[[1]]
  })

  # --- mapping controls, seeded from a guess ----------------------------
  observeEvent(uploaded(), {
    df <- first_sheet()
    g  <- guess_cols(df)
    nms <- names(df)
    dateish <- nms[vapply(df, is_dateish, logical(1))]

    grain0 <- c(setdiff(g$grain, g$week), if (!is.null(g$week)) g$week)
    updateSelectizeInput(session, "map_grain", choices = nms, selected = grain0)
    updateSelectInput(session, "map_week",
                      choices = c("(none)" = "", dateish),
                      selected = g$week %||% "")
    updateSelectInput(session, "map_spend",
                      choices = nms,
                      selected = g$spend %||% nms[length(nms)])
  })

  output$plan_source_status <- renderUI({
    u <- uploaded()
    if (is.null(u)) return(div(class = "alert alert-light border small py-2 mb-3",
                               "No file loaded yet."))
    sheets <- names(u$sheets)
    div(class = "alert alert-light border small py-2 mb-3",
        tags$b(u$name), tags$br(),
        sprintf("%d row%s", nrow(u$sheets[[1]]),
                if (nrow(u$sheets[[1]]) == 1) "" else "s"),
        if (length(sheets) > 1)
          span(" · sheets: ", paste(sheets, collapse = ", ")))
  })

  # --- live mapping validation ------------------------------------------
  mapping_problems <- reactive({
    u <- uploaded(); req(u)
    wk <- if (nzchar(input$map_week %||% "")) input$map_week else NULL
    check_mapping(first_sheet(), input$map_grain, wk, input$map_spend %||% "")
  })

  output$plan_mapping_status <- renderUI({
    if (is.null(uploaded())) return(NULL)
    p <- mapping_problems()
    if (!length(p)) {
      return(div(class = "alert alert-success small py-2",
                 icon("check"), " Mapping looks good."))
    }
    div(class = "alert alert-warning small py-2",
        tags$ul(class = "mb-0 ps-3", lapply(p, tags$li)))
  })

  # --- build ------------------------------------------------------------
  observeEvent(input$plan_build, {
    u <- uploaded()
    if (is.null(u)) {
      showNotification("Load a file or the sample first.", type = "warning"); return()
    }
    if (length(mapping_problems())) {
      showNotification("Fix the mapping problems first.", type = "warning"); return()
    }
    if (!nzchar(input$meta_name %||% "")) {
      showNotification("A plan name is required.", type = "warning"); return()
    }

    wk <- if (nzchar(input$map_week %||% "")) input$map_week else NULL
    sheets <- u$sheets

    built <- tryCatch({
      first <- TRUE
      for (i in seq_along(sheets)) {
        nick <- names(sheets)[i]
        if (!nzchar(nick)) nick <- input$meta_nickname %||% ""
        p <- build_plan(
          sheets[[i]], grain = input$map_grain, week = wk,
          spend_col = input$map_spend,
          name = input$meta_name, nickname = nick,
          advertiser = input$meta_advertiser %||% "",
          planner = input$meta_planner %||% "",
          status = if (first) input$meta_status else "in development")
        if (first) { store_init(st, p); first <- FALSE } else store_add(st, p)
      }
      TRUE
    }, error = function(e) e)

    if (inherits(built, "error")) {
      showNotification(conditionMessage(built), type = "error", duration = 10)
      return()
    }

    bump(bump() + 1)
    showNotification(
      sprintf("Built '%s' with %d scenario%s.", input$meta_name,
              length(store_scenario_names(st)),
              if (length(store_scenario_names(st)) == 1) "" else "s"),
      type = "message")
    nav_select("main_nav", "Scenarios")
  })

  # --- outputs ----------------------------------------------------------
  output$plan_kpis <- renderUI({
    bump()
    if (!store_has_plan(st)) {
      return(card(card_body(empty_state(
        "Load a file or the sample, map the columns, then Build plan.", "file-arrow-up"))))
    }
    p  <- store_base(st)
    d  <- p@data
    li <- length(unique(mediaplanr::line_item(d, mediaplanr::line_item_grain(p))))
    nw <- if (length(p@week_col)) length(unique(d[[p@week_col]])) else 0

    kpi <- function(label, value) {
      card(class = "border-0 shadow-sm",
           card_body(class = "py-3",
                     div(class = "kpi-label", label),
                     div(class = "kpi-value", value)))
    }
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      kpi("Total planned", fmt_money(sum(d$planned_spend))),
      kpi("Line items", li),
      kpi("Weeks", if (nw) nw else "—"),
      kpi("Rows", nrow(d))
    )
  })

  output$plan_preview <- reactable::renderReactable({
    bump(); req(store_has_plan(st))
    d <- store_base(st)@data
    reactable::reactable(
      d, defaultPageSize = 8, compact = TRUE, bordered = TRUE, highlight = TRUE,
      columns = list(planned_spend = reactable::colDef(
        name = "Planned spend", align = "right",
        cell = function(v) fmt_money(v))),
      style = list(fontSize = "0.82rem"))
  })

  output$plan_flight_plot <- renderPlot({
    bump(); req(store_has_plan(st))
    p <- chart_flighting(st$set, st$set@base_name)
    if (is.null(p)) {
      return(ggplot2::ggplot() + ggplot2::annotate(
        "text", 1, 1, label = "No week column mapped", colour = "grey50") +
        ggplot2::theme_void())
    }
    p
  }, bg = "transparent")

  output$plan_file_note <- renderUI({
    u <- uploaded()
    if (is.null(u)) return(p(class = "small text-muted mb-1", "Nothing uploaded."))
    p(class = "small text-muted mb-1",
      "First 6 rows of ", tags$b(names(u$sheets)[1] %||% "the file"),
      " exactly as read.")
  })

  output$plan_raw_preview <- reactable::renderReactable({
    u <- uploaded(); req(u)
    reactable::reactable(utils::head(u$sheets[[1]], 6), compact = TRUE,
                         bordered = TRUE, pagination = FALSE,
                         style = list(fontSize = "0.78rem"))
  })
}
