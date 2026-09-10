# ---------------------------------------------------------------------------
# Review -> Structure.
#
# Read-only, and thin by design: every chart is a mediaplanr projection handed
# to a mediaplanr.viz builder. The app decides WHICH plan to draw and nothing
# about how it is drawn -- which is the whole reason the companion package
# exists.
#
# A plan with no subplans still has a structure; it is just a tree of one. The
# tree and sunburst say so rather than erroring, because "nothing hangs beneath
# this" is a real and common answer.
# ---------------------------------------------------------------------------

server_structure <- function(input, output, session, st, bump) {

  observeEvent(bump(), {
    if (!store_has_plan(st)) return()
    nms <- store_scenario_names(st)
    sel <- if (isTRUE(input$str_scenario %in% nms)) input$str_scenario else st$set@base_name
    updateSelectInput(session, "str_scenario", choices = nms, selected = sel)
  })

  str_plan <- reactive({
    bump(); req(store_has_plan(st))
    nm <- input$str_scenario
    if (is.null(nm) || !nm %in% store_scenario_names(st)) nm <- st$set@base_name
    store_get(st, nm)
  })

  output$str_note <- renderUI({
    if (!store_has_plan(st)) return("Build a plan first.")
    p <- str_plan()
    n <- length(p@subplans)
    if (n == 0L) {
      return(tagList(
        "This plan holds no subplans, so its tree is a single node. Attach one ",
        "and the cells it backs become read-only here and on the Edit grid."))
    }
    sprintf("%d subplan%s attached.", n, if (n == 1L) "" else "s")
  })

  # Every cell of the plan, so one can be picked to refine. line_item() is the
  # same function attach_subplan() keys on, so a cell named here is a cell the
  # package will recognise.
  cells <- reactive({
    p <- str_plan()
    li <- setdiff(mediaplanr::line_item_grain(p), mediaplanr::flight_cols())
    if (!length(li)) return(character(0))
    sort(unique(mediaplanr::line_item(p@data, li)))
  })

  observeEvent(list(str_plan(), bump()), {
    cs <- cells()
    sel <- if (isTRUE(input$str_cell %in% cs)) input$str_cell else cs[1]
    updateSelectInput(session, "str_cell", choices = cs, selected = sel)
  })

  output$str_cell_action <- renderUI({
    req(store_has_plan(st))
    cell <- input$str_cell
    req(cell, nzchar(cell))
    if (cell %in% names(str_plan()@subplans)) {
      actionButton("str_detach", "Detach subplan", icon = icon("link-slash"),
                   class = "btn-outline-danger")
    } else {
      actionButton("str_refine", "Plan this cell in detail",
                   icon = icon("diagram-project"), class = "btn-primary")
    }
  })

  # Seeding goes through the same door the agent uses, so the form is filled
  # from the parent either way and a subplan that would not attach cannot be
  # authored by hand.
  observeEvent(input$str_refine, {
    res <- tryCatch(seed_subplan_tool(st, input$str_cell, input$str_scenario),
                    error = function(e) e)
    if (inherits(res, "error") || isFALSE(res$ok)) {
      showNotification(if (inherits(res, "error")) conditionMessage(res) else res$error,
                       type = "error", duration = 10)
      return()
    }
    bump(bump() + 1)
    nav_select("main_nav", "Build")
    nav_select("build_mode", "Design a plan")
    showNotification(
      sprintf("Designing a subplan for %s. Add a finer dimension, then Build to attach it.",
              input$str_cell), type = "message", duration = 8)
  })

  observeEvent(input$str_detach, {
    res <- tryCatch(store_detach_subplan(st, input$str_scenario %||% st$set@base_name,
                                         input$str_cell),
                    error = function(e) e)
    bump(bump() + 1)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 10); return()
    }
    showNotification(sprintf("Detached %s. The cell keeps its numbers and is editable again.",
                             input$str_cell), type = "message", duration = 6)
  })

  # Ownership and reconciliation only exist once something is attached. Drawn
  # from ownership_map() and the subplan rollup, both mediaplanr projections.
  output$str_owned <- renderUI({
    bump()
    req(store_has_plan(st))
    if (!length(str_plan()@subplans)) return(NULL)
    layout_columns(
      col_widths = c(7, 5),
      card(full_screen = TRUE, height = "420px",
           card_header(icon("table-cells"), " Who owns which cell"),
           card_body(class = "pt-2",
                     echarts4r::echarts4rOutput("str_ownership", height = "100%"))),
      card(full_screen = TRUE, height = "420px",
           card_header(icon("equals"), " How the cell adds up"),
           card_body(class = "pt-2",
                     echarts4r::echarts4rOutput("str_rollup", height = "100%")))
    )
  })

  output$str_ownership <- echarts4r::renderEcharts4r({
    p <- str_plan(); req(length(p@subplans))
    mediaplanr.viz::as_widget(mediaplanr.viz::ec_ownership(p))
  })

  output$str_rollup <- echarts4r::renderEcharts4r({
    p <- str_plan(); req(length(p@subplans))
    key <- if (isTRUE(input$str_cell %in% names(p@subplans))) input$str_cell else NULL
    mediaplanr.viz::as_widget(mediaplanr.viz::ec_rollup(p, key = key))
  })

  output$str_lineage <- echarts4r::renderEcharts4r({
    bump(); req(store_has_plan(st))
    mediaplanr.viz::as_widget(mediaplanr.viz::ec_lineage(st$set))
  })

  output$str_tree <- echarts4r::renderEcharts4r({
    mediaplanr.viz::as_widget(mediaplanr.viz::ec_tree(str_plan()))
  })

  output$str_sunburst <- echarts4r::renderEcharts4r({
    mediaplanr.viz::as_widget(mediaplanr.viz::ec_sunburst(str_plan()))
  })

  output$str_table <- reactable::renderReactable({
    mediaplanr.viz::tbl_tree(str_plan())
  })
}
