# ---------------------------------------------------------------------------
# Build page: source -> (mapping) -> the base MediaPlan, plus the registry.
#
# Both routes write to the same `uploaded()` slot, but they carry different
# amounts of knowledge with them. A sample knows its own columns, so it ships a
# `defaults` list and its subtab shows no mode / week / spend pickers at all; a
# file knows nothing, so its subtab asks. Build therefore reads a SPEC assembled
# per route rather than reading `input$map_*` directly -- those inputs do not
# exist on the sample route, and reading them there would silently take the
# wrong branch.
# ---------------------------------------------------------------------------

# The two built-in samples, as `uploaded()` values. A sample knows its own
# columns, so it ships the mapping the build needs; the sample subtab therefore
# shows no mode, week or spend picker at all.
#
# Defined at file level rather than inside the module so the Welcome page's
# shortcut loads exactly the same thing -- there used to be two definitions of
# "the sample", one per page, free to drift apart.
sample_source <- function(which = c("weekly", "flights")) {
  which <- match.arg(which)
  if (which == "flights") {
    return(list(
      name     = "Q2 2026 Flight Plan",
      sheets   = stats::setNames(list(sample_flights_df()), "baseline"),
      source   = "sample",
      defaults = list(mode = "flights", spend = "planned_spend",
                      start = "flight_start", end = "flight_end",
                      week_start = "Monday")))
  }
  list(
    name     = "Q2 2026 Media Plan",
    sheets   = stats::setNames(list(sample_plan_df()), "baseline"),
    source   = "sample",
    defaults = list(mode = "weekly", week = "week", spend = "planned_spend"))
}

load_sample <- function(session, uploaded, which) {
  u <- sample_source(which)
  uploaded(u)
  updateTextInput(session, "smp_name", value = u$name)
  updateTextInput(session, "smp_nickname", value = "baseline")
  updateTextInput(session, "smp_advertiser", value = "Acme Corp")
  invisible(u)
}

server_build <- function(input, output, session, st, bump, uploaded) {

  blank_to_null <- function(x) if (is.null(x) || !nzchar(x)) NULL else x

  # --- the scratch route: a recipe, not a file ----------------------------
  #
  # The recipe is assembled from the inputs on every change and, once it has no
  # problems, generates the data frame straight into `uploaded()`. There is no
  # Generate button: the outline table below the textarea is the preview, and
  # the only commit point stays "Build plan" -- the same as the other two
  # routes.
  observe({
    updateSelectizeInput(session, "scaf_dims",
                         choices = DIMENSION_SUGGESTIONS,
                         selected = c("channel", "partner"), server = FALSE)
    updateSelectInput(session, "scaf_pacing", choices = pacing_shapes(),
                      selected = "flat")
    updateSelectInput(session, "scaf_unit_type",
                      choices = c("(none)" = "", mediaplanr::unit_type_levels()))
    updateDateInput(session, "scaf_start", value = Sys.Date())
    updateTextAreaInput(session, "scaf_tree", value = paste(
      "TV 55%", "  NBC 60%", "  ESPN 40% ~front",
      "Search 25%", "  Google ~ramp",
      "Social 20%", "  Meta x3 ~ramp", "  TikTok x1 ~back", sep = "\n"))
  }, priority = 100)

  # An empty numericInput arrives as NA, not NULL, and NA is not "absent" to
  # anything downstream -- it is a number that poisons arithmetic.
  na_to_null <- function(x) if (!length(x) || all(is.na(x))) NULL else x

  # Typing is the only high-frequency input here, so it is the only one
  # debounced: re-parsing the outline on every keystroke would also rebuild
  # the generated data frame on every keystroke.
  tree_txt <- debounce(reactive(input$scaf_tree %||% ""), 400)

  # The parse is separate from the recipe so a broken outline reports its own
  # problems rather than emptying the tree and reporting "no line items".
  parsed <- reactive({
    parse_tree_text(tree_txt(), input$scaf_dims %||% character(0))
  })

  recipe <- reactive({
    r <- new_recipe()
    r$dimensions <- input$scaf_dims %||% character(0)
    r$tree       <- parsed()$tree
    r$time <- list(basis      = input$scaf_basis %||% "weekly",
                   week_start = input$scaf_week_start %||% "Monday",
                   start      = na_to_null(input$scaf_start),
                   n_weeks    = input$scaf_weeks)
    r$budget <- list(total    = na_to_null(input$scaf_total),
                     round_to = as.numeric(input$scaf_round %||% "0.01"))
    r$defaults$pacing <- input$scaf_pacing %||% "flat"
    r$measures <- list(unit_type = blank_to_null(input$scaf_unit_type),
                       rate      = na_to_null(input$scaf_rate))
    r$meta <- list(name = input$scaf_name %||% "",
                   nickname = input$scaf_nickname %||% "",
                   advertiser = input$scaf_advertiser %||% "",
                   planner = input$scaf_planner %||% "", objective = "")
    r
  })

  scaf_problems <- reactive(c(parsed()$problems, scaffold_problems(recipe())))

  # Guarded on the active subtab. `uploaded()` is shared by all three routes,
  # so without this the recipe would overwrite a file the user had just
  # uploaded on another tab, simply because a scaffold input changed.
  observe({
    req(identical(input$build_mode, "Design a plan"))
    if (length(scaf_problems())) return()
    uploaded(scaffold_to_uploaded(recipe()))
  })

  # The form's state is mirrored into the store on every change, so the agent's
  # get_scaffold always reads what is actually on screen. Silent by design --
  # store_sync_scaffold does not touch the counter, so this echo cannot be
  # mistaken for the agent having changed something.
  observe(store_sync_scaffold(st, recipe()))

  # ...and the other direction. The agent's writes bump the counter, and this
  # pulls them into the inputs, which rebuilds recipe() from them. Comparing
  # the counter rather than the recipe is what keeps the two from chasing each
  # other round: the form's own echo never increments it.
  applied <- reactiveVal(0L)
  observeEvent(bump(), {
    n <- st$scaffold_n %||% 0L
    if (n == 0L || identical(n, applied())) return()
    applied(n)
    r <- st$scaffold
    req(!is.null(r))

    dims <- as.character(r$dimensions %||% character(0))
    updateSelectizeInput(session, "scaf_dims",
                         choices = unique(c(dims, DIMENSION_SUGGESTIONS)),
                         selected = dims, server = FALSE)
    updateTextAreaInput(session, "scaf_tree",
                        value = format_tree_text(r$tree %||% list(), dims))
    updateRadioButtons(session, "scaf_basis", selected = r$time$basis %||% "weekly")
    if (!is.null(r$time$start)) {
      updateDateInput(session, "scaf_start", value = as.Date(r$time$start))
    }
    if (!is.null(r$time$n_weeks))    updateNumericInput(session, "scaf_weeks", value = r$time$n_weeks)
    if (!is.null(r$time$week_start)) updateSelectInput(session, "scaf_week_start", selected = r$time$week_start)
    if (!is.null(r$budget$total))    updateNumericInput(session, "scaf_total", value = r$budget$total)
    if (!is.null(r$defaults$pacing)) updateSelectInput(session, "scaf_pacing", selected = r$defaults$pacing)
    if (!is.null(r$measures$unit_type)) updateSelectInput(session, "scaf_unit_type", selected = r$measures$unit_type)
    if (!is.null(r$measures$rate))   updateNumericInput(session, "scaf_rate", value = r$measures$rate)
    for (f in c("name", "nickname", "advertiser", "planner")) {
      v <- r$meta[[f]]
      if (!is.null(v)) updateTextInput(session, paste0("scaf_", f), value = v)
    }

    # Show the user what changed rather than leaving it on a page they cannot
    # see. Harmless when they are already here.
    nav_select("main_nav", "Build")
    nav_select("build_mode", "Design a plan")
  })

  output$scaf_outline <- reactable::renderReactable({
    o <- tryCatch(tree_outline(recipe()), error = function(e) NULL)
    req(o)
    outline_table(o)
  })

  output$scaf_count <- renderUI({
    o <- tryCatch(tree_outline(recipe()), error = function(e) NULL)
    if (is.null(o) || !nrow(o)) return(span(class = "text-muted small", "nothing yet"))
    basis <- input$scaf_basis %||% "weekly"
    rows  <- if (identical(basis, "weekly")) sum(o$weeks) else nrow(o)
    span(class = "small text-muted",
         sprintf("%d line item%s \u00b7 %d row%s", nrow(o),
                 if (nrow(o) == 1) "" else "s", rows, if (rows == 1) "" else "s"))
  })

  output$scaf_problems <- renderUI({
    p <- scaf_problems()
    if (!length(p)) return(NULL)
    div(class = "alert alert-warning small py-2",
        tags$ul(class = "mb-0 ps-3", lapply(p, tags$li)))
  })

  output$scaf_status <- renderUI({
    if (length(scaf_problems())) {
      return(span(class = "text-muted", "Fill in what is missing above."))
    }
    o  <- tree_outline(recipe())
    nm <- input$scaf_name %||% ""
    tagList(if (nzchar(nm)) tagList(tags$b(nm), " \u00b7 "),
            sprintf("%d line item%s \u00b7 %s", nrow(o),
                    if (nrow(o) == 1) "" else "s", fmt_money(sum(o$amount))))
  })

  # Build does two different things depending on whether a cell is targeted,
  # so it says which. A button that silently replaces your session instead of
  # attaching would be the worst kind of surprise.
  output$scaf_target <- renderUI({
    bump()
    tgt <- st$subplan_target
    if (is.null(tgt)) return(NULL)
    div(class = "alert alert-info small py-2 d-flex align-items-center gap-2",
        icon("diagram-project"),
        div(class = "flex-grow-1",
            "Designing a subplan for ", tags$b(tgt$key), " of ",
            tags$b(tgt$parent), ". Building will attach it to that cell, not ",
            "start a new plan."),
        actionButton("scaf_cancel_target", "Cancel", class = "btn-sm btn-outline-secondary"))
  })

  observeEvent(input$scaf_cancel_target, {
    st$subplan_target <- NULL
    bump(bump() + 1)
    showNotification("No longer designing a subplan.", type = "message")
  })

  output$scaf_build_btn <- renderUI(
    build_btn("scaf_build", !length(scaf_problems()) && nzchar(input$scaf_name %||% "")))

  # --- sources -----------------------------------------------------------
  observeEvent(input$smp_load_weekly, {
    load_sample(session, uploaded, "weekly")
    showNotification("Weekly sample loaded — check the preview, then Build plan.",
                     type = "message", duration = 5)
  })

  observeEvent(input$smp_load_flights, {
    load_sample(session, uploaded, "flights")
    showNotification("Flight sample loaded — one row per buy, mapped to in-market dates.",
                     type = "message", duration = 5)
  })

  observeEvent(input$plan_upload, {
    req(input$plan_upload)
    res <- tryCatch(
      read_plan_file(input$plan_upload$datapath, input$plan_upload$name),
      error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 8)
      return()
    }
    res$source <- "file"
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

  # Each subtab only ever reports on its own route, so switching tabs does not
  # show the other one's file.
  src_for <- function(route) {
    u <- uploaded()
    if (is.null(u) || !identical(u$source %||% "file", route)) NULL else u
  }
  smp_src  <- reactive(src_for("sample"))
  file_src <- reactive(src_for("file"))

  first_sheet <- reactive({
    u <- uploaded(); req(u)
    u$sheets[[1]]
  })

  # --- mapping controls, seeded from a guess -----------------------------
  # Both id families are seeded from the same guess: the hidden route's controls
  # are updated too, which costs nothing and means switching tabs never lands on
  # a stale mapping.
  observeEvent(uploaded(), {
    df  <- first_sheet()
    g   <- guess_cols(df)
    nms <- names(df)
    dateish <- nms[vapply(df, is_dateish, logical(1))]
    grain0  <- c(setdiff(g$grain, g$week), if (!is.null(g$week)) g$week)

    updateSelectizeInput(session, "smp_grain", choices = nms, selected = grain0)
    updateSelectizeInput(session, "map_grain", choices = nms, selected = grain0)

    for (id in c("smp_unit_type", "map_unit_type")) {
      updateSelectInput(session, id, choices = c("(none)" = "", nms),
                        selected = g$unit_type %||% "")
    }
    for (id in c("smp_units", "map_units")) {
      updateSelectInput(session, id, choices = c("(none)" = "", nms),
                        selected = g$units %||% "")
    }
    for (id in c("smp_rate", "map_rate")) {
      updateSelectInput(session, id, choices = c("(none)" = "", nms),
                        selected = g$rate %||% "")
    }

    updateSelectInput(session, "map_week", choices = c("(none)" = "", dateish),
                      selected = g$week %||% "")
    updateSelectInput(session, "map_spend", choices = c("(none)" = "", nms),
                      selected = g$spend %||% nms[length(nms)])

    # Flight mode: the week column is CREATED by the constructor, so the grain
    # is the line item only and the dates are mapped separately.
    fs <- if ("flight_start" %in% nms) "flight_start" else (dateish[1] %||% "")
    fe <- if ("flight_end" %in% nms) "flight_end" else (utils::tail(dateish, 1) %||% "")
    updateSelectInput(session, "map_flight_start", choices = c("(none)" = "", nms),
                      selected = fs)
    updateSelectInput(session, "map_flight_end", choices = c("(none)" = "", nms),
                      selected = fe)

    # A file arriving as flights should land on the flights radio; nothing else
    # in the file route can know that.
    if (identical(uploaded()$source, "file") &&
        all(c("flight_start", "flight_end") %in% nms)) {
      updateRadioButtons(session, "map_mode", selected = "flights")
    }
  })

  # --- the spec each route builds from -----------------------------------
  route_spec <- function(route) {
    u <- uploaded()
    d <- u$defaults %||% list()
    if (identical(route, "scratch")) {
      # The generator wrote every column, so the whole mapping comes from
      # `defaults` and this subtab renders no column pickers at all. Reading
      # input$map_* here would silently return NULL and take the wrong branch,
      # which is exactly what the header comment warns about.
      return(list(mode  = d$mode %||% "weekly",
                  grain = d$grain,
                  week  = d$week, spend = d$spend %||% "planned_spend",
                  start = d$start, end = d$end,
                  week_start = d$week_start %||% "Monday",
                  unit_type = d$unit_type, units = d$units, rate = d$rate,
                  name = input$scaf_name, nickname = input$scaf_nickname,
                  advertiser = input$scaf_advertiser, planner = input$scaf_planner))
    }
    if (identical(route, "sample")) {
      list(mode  = d$mode %||% "weekly",
           grain = input$smp_grain,
           week  = d$week, spend = d$spend %||% "",
           start = d$start, end = d$end,
           week_start = d$week_start %||% "Monday",
           unit_type = blank_to_null(input$smp_unit_type),
           units     = blank_to_null(input$smp_units),
           rate      = blank_to_null(input$smp_rate),
           name = input$smp_name, nickname = input$smp_nickname,
           advertiser = input$smp_advertiser, planner = input$smp_planner)
    } else {
      list(mode  = input$map_mode %||% "weekly",
           grain = input$map_grain,
           week  = blank_to_null(input$map_week),
           spend = input$map_spend %||% "",
           start = blank_to_null(input$map_flight_start),
           end   = blank_to_null(input$map_flight_end),
           week_start = input$map_week_start %||% "Monday",
           unit_type = blank_to_null(input$map_unit_type),
           units     = blank_to_null(input$map_units),
           rate      = blank_to_null(input$map_rate),
           name = input$meta_name, nickname = input$meta_nickname,
           advertiser = input$meta_advertiser, planner = input$meta_planner)
    }
  }

  spec_problems <- function(spec, df) {
    if (identical(spec$mode, "flights")) {
      return(check_flight_mapping(df, spec$grain, spec$start, spec$end,
                                  spec$spend %||% "",
                                  units_col = spec$units, rate_col = spec$rate))
    }
    check_mapping(df, spec$grain, spec$week, spec$spend %||% "",
                  units_col = spec$units, rate_col = spec$rate)
  }

  smp_problems  <- reactive({ req(smp_src());  spec_problems(route_spec("sample"), first_sheet()) })
  file_problems <- reactive({ req(file_src()); spec_problems(route_spec("file"),   first_sheet()) })

  # --- source status, previews, buttons ----------------------------------
  source_line <- function(u) {
    if (is.null(u)) return(NULL)
    sheets <- names(u$sheets)
    tagList(
      tags$b(u$name),
      sprintf(" · %d row%s", nrow(u$sheets[[1]]),
              if (nrow(u$sheets[[1]]) == 1) "" else "s"),
      if (length(sheets) > 1) span(" · sheets: ", paste(sheets, collapse = ", ")),
      if (identical(u$defaults$mode, "flights")) span(" · one row per buy"))
  }

  output$smp_source_status <- renderUI({
    u <- smp_src()
    if (is.null(u)) return(span(class = "text-muted", "Pick a sample to start."))
    source_line(u)
  })

  output$file_source_status <- renderUI({
    u <- file_src()
    if (is.null(u)) return(span(class = "text-muted", "No file loaded yet."))
    source_line(u)
  })

  output$smp_file_note <- renderUI({
    u <- smp_src()
    if (is.null(u)) return(p(class = "small text-muted mb-1",
                             "Nothing loaded — press a sample button above."))
    p(class = "small text-muted mb-1",
      "First 10 rows of the sample, exactly as the app reads them.")
  })

  output$file_note <- renderUI({
    u <- file_src()
    if (is.null(u)) return(p(class = "small text-muted mb-1", "Nothing uploaded."))
    # A csv has one unnamed sheet, so `%||%` is not enough here: the name is ""
    # rather than NULL, and the note read "First 10 rows of  exactly as read."
    sheet <- names(u$sheets)[1] %||% ""
    p(class = "small text-muted mb-1",
      "First 10 rows of ",
      tags$b(if (nzchar(sheet)) sheet else "the file"),
      " exactly as read.")
  })

  output$smp_raw_preview <- reactable::renderReactable({
    u <- smp_src(); req(u); raw_preview_table(u$sheets[[1]])
  })

  output$file_raw_preview <- reactable::renderReactable({
    u <- file_src(); req(u); raw_preview_table(u$sheets[[1]])
  })

  # Inert until there is something to build, so "load first" is visible rather
  # than enforced by a notification after the fact.
  build_btn <- function(id, ready) {
    if (ready) {
      actionButton(id, "Build plan", icon = icon("check"), class = "btn-primary")
    } else {
      tags$button(class = "btn btn-primary disabled", disabled = NA,
                  icon("check"), " Build plan")
    }
  }
  output$smp_build_btn  <- renderUI(build_btn("smp_build",  !is.null(smp_src())))
  output$file_build_btn <- renderUI(build_btn("file_build", !is.null(file_src())))

  # The sample route only speaks up when something is wrong: "mapping looks
  # good" is noise on a mapping the user never made.
  output$smp_mapping_status <- renderUI({
    if (is.null(smp_src())) return(NULL)
    p <- smp_problems()
    if (!length(p)) return(NULL)
    div(class = "alert alert-warning small py-2",
        tags$ul(class = "mb-0 ps-3", lapply(p, tags$li)))
  })

  output$file_mapping_status <- renderUI({
    if (is.null(file_src())) return(NULL)
    p <- file_problems()
    if (!length(p)) {
      return(div(class = "alert alert-success small py-2",
                 icon("check"), " Mapping looks good."))
    }
    div(class = "alert alert-warning small py-2",
        tags$ul(class = "mb-0 ps-3", lapply(p, tags$li)))
  })

  # --- build --------------------------------------------------------------
  do_build <- function(route) {
    u <- uploaded()
    if (is.null(u)) {
      showNotification("Load a sample or a file first.", type = "warning"); return()
    }
    spec <- route_spec(route)
    if (length(spec_problems(spec, first_sheet()))) {
      showNotification("Fix the mapping problems first.", type = "warning"); return()
    }
    if (!nzchar(spec$name %||% "")) {
      showNotification("A plan name is required.", type = "warning"); return()
    }

    # A scratch recipe seeded for a cell attaches to that cell rather than
    # replacing the session. The agent's build_from_scaffold tool takes the
    # same branch, so the button and the assistant land in the same place.
    tgt <- st$subplan_target
    if (identical(route, "scratch") && !is.null(tgt)) {
      res <- tryCatch({
        store_attach_subplan(st, tgt$parent, build_from_recipe(recipe()))
        TRUE
      }, error = function(e) e)
      if (inherits(res, "error")) {
        showNotification(conditionMessage(res), type = "error", duration = 12)
        return()
      }
      st$subplan_target <- NULL
      bump(bump() + 1)
      showNotification(
        sprintf("Attached to '%s' under %s - that cell is now read-only on the parent.",
                tgt$parent, tgt$key), type = "message", duration = 8)
      nav_select("main_nav", "Review")
      nav_select("review_mode", "Structure")
      return()
    }

    sheets <- u$sheets
    built <- tryCatch({
      first <- TRUE
      for (i in seq_along(sheets)) {
        nick <- names(sheets)[i]
        if (!nzchar(nick %||% "")) nick <- spec$nickname %||% ""
        p <- if (identical(spec$mode, "flights")) {
          build_plan_from_flights(
            sheets[[i]], grain = spec$grain,
            start_col = spec$start, end_col = spec$end,
            spend_col = spec$spend, week_start = spec$week_start,
            units_col = spec$units, rate_col = spec$rate,
            unit_type_col = spec$unit_type,
            name = spec$name, nickname = nick,
            advertiser = spec$advertiser %||% "", planner = spec$planner %||% "",
            status = "in development")
        } else {
          build_plan(
            sheets[[i]], grain = spec$grain, week = spec$week,
            spend_col = spec$spend,
            units_col = spec$units, rate_col = spec$rate,
            unit_type_col = spec$unit_type,
            name = spec$name, nickname = nick,
            advertiser = spec$advertiser %||% "", planner = spec$planner %||% "",
            status = "in development")
        }
        if (first) { store_init(st, p); first <- FALSE } else store_add(st, p)
      }
      TRUE
    }, error = function(e) e)

    if (inherits(built, "error")) {
      showNotification(conditionMessage(built), type = "error", duration = 10)
      return()
    }

    bump(bump() + 1)

    # The preview has done its job; the registry below is what matters next.
    # The scratch route has no raw-file preview to close.
    if (!identical(route, "scratch")) {
      accordion_panel_close(
        if (identical(route, "sample")) "smp_prev_acc" else "file_prev_acc", "preview")
    }

    n <- length(store_scenario_names(st))
    showNotification(
      sprintf("Built '%s' with %d scenario%s — it is in the registry below.",
              spec$name, n, if (n == 1) "" else "s"),
      type = "message", duration = 6)
  }

  observeEvent(input$scaf_build, do_build("scratch"))
  observeEvent(input$smp_build,  do_build("sample"))
  observeEvent(input$file_build, do_build("file"))

  # --- registry -----------------------------------------------------------
  output$registry_body <- renderUI({
    bump()
    if (!store_has_plan(st)) {
      return(empty_state(
        "Load a sample or a file above, then press Build plan.", "layer-group"))
    }
    registry_table(st$set)
  })

  output$registry_actions <- renderUI({
    bump()
    if (!store_has_plan(st)) return(NULL)
    tagList(
      actionButton("reg_goto_edit", "Edit this plan", icon = icon("table-cells"),
                   class = "btn-sm btn-primary me-1"),
      actionButton("reg_goto_review", "Review it", icon = icon("eye"),
                   class = "btn-sm btn-outline-secondary")
    )
  })

  observeEvent(input$reg_goto_edit, nav_select("main_nav", "Edit"))
  observeEvent(input$reg_goto_review, {
    nav_select("main_nav", "Review")
    nav_select("review_mode", "Plan")
  })

  # The registry's inline status select, delegated through www/grid.js exactly
  # like the editable spend cells.
  observeEvent(input$registry_status, {
    e <- input$registry_status
    req(e$scenario, e$status)
    res <- tryCatch(store_set_status(st, e$scenario, e$status),
                    error = function(err) err)
    bump(bump() + 1)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 8); return()
    }
    showNotification(sprintf("'%s' is now %s.", e$scenario, e$status),
                     type = "message", duration = 3)
  })
}
