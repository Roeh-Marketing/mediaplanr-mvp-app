# ---------------------------------------------------------------------------
# Export page.
#
# The workbook download is the one that matters: it is written in the same shape
# read_plan_file() reads, so export -> edit in Excel -> re-upload round-trips
# back into the same scenarios. Import and export are two ends of one format.
# ---------------------------------------------------------------------------

server_export <- function(input, output, session, st, bump) {

  observeEvent(bump(), {
    if (!store_has_plan(st)) return()
    nms <- store_scenario_names(st)
    updateSelectInput(session, "exp_scenario", choices = nms,
                      selected = st$active %||% nms[1])
  })

  # jsonlite is not attached (it masks shiny::validate), but qualify anyway so
  # these handlers keep working regardless of what else gets attached later.
  need_plan <- function() {
    shiny::validate(shiny::need(store_has_plan(st), "No plan loaded."))
  }

  safe  <- function(x) gsub("[^A-Za-z0-9._-]+", "-", x)
  stamp <- function() format(Sys.Date(), "%Y%m%d")

  # Which scenarios go into the workbook.
  wb_scenarios <- reactive({
    bump()
    if (!store_has_plan(st)) return(character(0))
    all <- store_scenario_names(st)
    if (isTRUE(input$exp_wb_selected_only)) {
      sel <- intersect(all, input$cmp_scenarios %||% character(0))
      if (length(sel)) sel else all
    } else all
  })

  output$exp_wb_note <- renderUI({
    n <- length(wb_scenarios())
    if (!n) return(div(class = "small text-muted mb-2", "No plan loaded."))
    div(class = "small text-muted mb-2",
        sprintf("%d sheet%s: ", n, if (n == 1) "" else "s"),
        tags$code(paste(wb_scenarios(), collapse = ", ")))
  })

  plan_file_name <- function() {
    if (!store_has_plan(st)) return("media-plan")
    safe(store_base(st)@name)
  }

  # --- workbook: one sheet per scenario ---------------------------------
  output$exp_workbook <- downloadHandler(
    filename = function() paste0(plan_file_name(), "-", stamp(), ".xlsx"),
    content = function(file) {
      need_plan()
      write_scenarios_xlsx(st$set, file, scenarios = wb_scenarios())
    }
  )

  # --- comparison -------------------------------------------------------
  # The flight level only means anything for a plan authored as buys. On a
  # weekly plan compare_scenarios(level = "flight") returns zero rows, so
  # offering it would hand the user an empty file with no explanation. It is
  # also the level that can WARN (on a set whose scenarios share no flight ids),
  # and a downloadHandler sends warnings to the console, not the user.
  observe({
    bump()
    has_flights <- store_has_plan(st) &&
      nrow(mediaplanr::flights(store_base(st))) > 0
    ch <- c("Summary — one row per scenario" = "summary",
            "Cells — line item x week x scenario" = "cell")
    if (has_flights) ch <- c(ch, "Flights — one buy x scenario" = "flight")
    updateSelectInput(session, "exp_level", choices = ch,
                      selected = isolate(input$exp_level) %||% "summary")
  })

  output$exp_compare <- downloadHandler(
    filename = function() {
      paste0("mediaplan-compare-", input$exp_level %||% "summary", "-", stamp(),
             ".", input$exp_cmp_fmt %||% "xlsx")
    },
    content = function(file) {
      need_plan()
      tbl <- mediaplanr::compare_scenarios(st$set, input$exp_level %||% "summary")
      if (identical(input$exp_cmp_fmt %||% "xlsx", "csv")) {
        utils::write.csv(tbl, file, row.names = FALSE)
      } else {
        write_df_xlsx(tbl, file, sheet = paste0("compare-", input$exp_level))
      }
    }
  )

  # --- one scenario -----------------------------------------------------
  output$exp_plan <- downloadHandler(
    filename = function() {
      paste0("mediaplan-", safe(input$exp_scenario %||% "scenario"), "-", stamp(),
             ".", input$exp_plan_fmt %||% "xlsx")
    },
    content = function(file) {
      need_plan()
      d <- store_get(st, input$exp_scenario)@data
      if (identical(input$exp_plan_fmt %||% "xlsx", "csv")) {
        utils::write.csv(d, file, row.names = FALSE)
      } else {
        write_df_xlsx(d, file, sheet = input$exp_scenario %||% "plan")
      }
    }
  )

  # --- everything stacked ----------------------------------------------
  output$exp_all <- downloadHandler(
    filename = function() paste0("mediaplan-all-scenarios-", stamp(), ".csv"),
    content = function(file) {
      need_plan()
      # Stacked long, with the scenario label and its metadata, so a row can
      # always be traced back to the plan it belongs to.
      parts <- lapply(names(st$set@scenarios), function(nm) {
        p <- st$set@scenarios[[nm]]
        cbind(data.frame(scenario = nm, plan_name = p@name,
                         nickname = p@nickname, status = p@status,
                         stringsAsFactors = FALSE),
              p@data)
      })
      utils::write.csv(do.call(rbind, parts), file, row.names = FALSE)
    }
  )

  output$exp_status <- renderUI({
    bump()
    if (!store_has_plan(st)) {
      return(div(class = "alert alert-light border small mt-3",
                 "Build a plan before exporting."))
    }
    div(class = "small text-muted mt-3",
        sprintf("%d scenario%s available.", length(store_scenario_names(st)),
                if (length(store_scenario_names(st)) == 1) "" else "s"))
  })
}
