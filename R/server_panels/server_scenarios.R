# ---------------------------------------------------------------------------
# Scenarios page: the editing workbench.
#
# Three ways to change spend, one commit point. Grid edits and staged
# operations both accumulate into `pending`; nothing becomes a scenario until
# Save is pressed. The chat is the exception -- the agent's apply_edits tool
# creates a scenario directly, because a chat instruction is already a complete
# thought ("make it 20% bigger"), not a half-finished edit.
# ---------------------------------------------------------------------------

server_scenarios <- function(input, output, session, st, bump, agent) {

  save_plot <- make_chat_plot_saver(session)

  # Staged cell edits: cell_key -> value. Cleared on save, discard, or switch.
  pending <- reactiveVal(list())

  active_plan <- reactive({
    bump(); req(store_has_plan(st))
    store_get(st, input$scn_active %||% st$active)
  })

  # --- scenario selector -------------------------------------------------
  observeEvent(bump(), {
    if (!store_has_plan(st)) return()
    nms <- store_scenario_names(st)
    updateSelectInput(session, "scn_active", choices = nms,
                      selected = st$active %||% nms[1])
  })

  observeEvent(input$scn_active, {
    req(nzchar(input$scn_active %||% ""))
    if (!input$scn_active %in% store_scenario_names(st)) return()
    store_set_active(st, input$scn_active)
    pending(list())          # staged edits belong to the plan they were typed on
  })

  output$scn_active_meta <- renderUI({
    p <- active_plan()
    div(class = "small text-muted mb-1",
        status_badge(p@status), " ",
        tags$span(fmt_money(sum(p@data$planned_spend))),
        if (length(p@parent_id) && nzchar(p@parent_id))
          tags$span(class = "d-block mt-1", icon("code-branch"),
                    " derived scenario") else
          tags$span(class = "d-block mt-1", icon("star"), " baseline"))
  })

  # --- quick operation ---------------------------------------------------
  output$scn_target_inputs <- renderUI({
    p <- active_plan()
    # grain_values() is what the package offers for exactly this: each grain
    # column's distinct values, in the shape an edit `target` accepts. Hand-
    # rolling it here was a second implementation that could drift.
    lg <- setdiff(mediaplanr::line_item_grain(p), mediaplanr::flight_cols())
    controls <- lapply(lg, function(g) {
      selectizeInput(paste0("tgt_", g), tools::toTitleCase(g),
                     choices = as.character(mediaplanr::grain_values(p, g)),
                     multiple = TRUE,
                     options = list(placeholder = "all",
                                    plugins = list("remove_button")))
    })
    if (length(p@week_col)) {
      controls <- c(controls, list(selectizeInput(
        "tgt_week", "Week",
        choices = as.character(mediaplanr::grain_values(p, p@week_col)),
        multiple = TRUE,
        options = list(placeholder = "all", plugins = list("remove_button")))))
    }
    tagList(controls)
  })

  # `add` names a line item that does not exist yet, so it needs free text
  # rather than the target selectors, which only offer existing values.
  output$scn_add_inputs <- renderUI({
    p <- active_plan()
    lg <- setdiff(mediaplanr::line_item_grain(p), mediaplanr::flight_cols())
    ctrls <- lapply(lg, function(g) {
      textInput(paste0("add_", g), tools::toTitleCase(g), value = "")
    })
    if (length(p@week_col)) {
      wks <- as.character(mediaplanr::grain_values(p, p@week_col))
      ctrls <- c(ctrls, list(selectInput("add_week", "Week", choices = wks)))
    }
    tagList(ctrls)
  })

  output$op_hint <- renderText({
    switch(input$op_kind %||% "total",
      scale      = "1.2 raises matched rows 20%; 0.5 halves them.",
      delta      = "Moves the matched rows' TOTAL by this, keeping their mix. Negative to cut.",
      set        = "Every matched row becomes exactly this -- so the total is this times the row count.",
      delta_each = "Added to EVERY matched row -- so the total moves by this times the row count.",
      total      = "Matched rows are rescaled to sum to this, keeping their mix.",
      drop       = "Removes the matched rows entirely. Not the same as setting them to zero.",
      add        = "Introduces a line item the plan does not have. Spend goes in Value.",
      shift      = "Moves the matched buys by this many days. Negative moves earlier.",
      restage    = "Moves the matched buys to the dates above. Needs a plan with flights.")
  })

  observeEvent(input$op_kind, {
    updateNumericInput(session, "op_value",
      value = switch(input$op_kind, scale = 1.1, delta = 50000,
                     delta_each = 5000, set = 25000, total = 500000,
                     shift = 7, add = 25000, 1),
      step = switch(input$op_kind, scale = 0.05, shift = 1, 5000))
  })

  # A staged operation is appended to the pending list and replayed. It is no
  # longer diffed back into cell values -- that could not represent add, drop,
  # shift or restage, because the diff walked the BASE plan's rows and those ops
  # change which rows there are.
  observeEvent(input$op_stage, {
    p  <- active_plan()
    lg <- setdiff(mediaplanr::line_item_grain(p), mediaplanr::flight_cols())
    kind <- input$op_kind %||% "total"

    target <- list()
    for (g in lg) {
      v <- input[[paste0("tgt_", g)]]
      if (length(v)) target[[g]] <- v
    }
    if (length(p@week_col)) {
      v <- input$tgt_week
      if (length(v)) target[[p@week_col]] <- as.Date(v)
    }

    op <- list(target = if (length(target)) target else NULL)

    # `during` is a selector, not an operation: it rides alongside any of them.
    if (isTRUE(input$op_use_during)) {
      if (is.null(input$op_during) || length(input$op_during) < 2) {
        showNotification("Pick both dates for the in-market window.", type = "warning"); return()
      }
      op$during <- list(from = as.character(input$op_during[1]),
                        to   = as.character(input$op_during[2]))
    }

    # Each operation takes a different shape of value; a single numeric input
    # only ever fitted the spend ones.
    if (kind == "drop") {
      op$drop <- TRUE
    } else if (kind == "restage") {
      if (is.null(input$op_restage) || length(input$op_restage) < 2) {
        showNotification("Pick the new start and end dates.", type = "warning"); return()
      }
      op$restage <- list(from = as.character(input$op_restage[1]),
                         to   = as.character(input$op_restage[2]))
    } else if (kind == "add") {
      spec <- list()
      for (g in lg) {
        v <- input[[paste0("add_", g)]]
        if (!nzchar(v %||% "")) {
          showNotification(paste0("Give a value for ", g, "."), type = "warning"); return()
        }
        spec[[g]] <- v
      }
      if (length(p@week_col)) spec[[p@week_col]] <- as.character(input$add_week)
      if (!is.finite(input$op_value %||% NA)) {
        showNotification("Enter a spend for the new line item.", type = "warning"); return()
      }
      spec$planned_spend <- input$op_value
      op <- list(add = spec)                       # add takes no target
    } else {
      val <- input$op_value
      if (!is.finite(val)) {
        showNotification("Enter a numeric value.", type = "warning"); return()
      }
      op[[kind]] <- val
    }

    # Validate by replaying before accepting it, so a bad op is refused here
    # with the package's own message rather than breaking the grid.
    trial <- tryCatch(
      mediaplanr::build_scenario(active_plan(), edits = c(pending(), list(op)),
                                 name = "staging"),
      error = function(e) e)
    if (inherits(trial, "error")) {
      showNotification(conditionMessage(trial), type = "error", duration = 10); return()
    }

    pending(stage_op(pending(), op))
    showNotification("Change staged - review the grid, then Save scenario.",
                     type = "message", duration = 4)
  })

  # The active plan with the pending ops replayed, in order. Measured at about
  # 0.1 ms an op on a 104-row plan, so this is cheap enough to do on render.
  staged_plan <- reactive({
    p  <- active_plan()
    pe <- pending()
    if (!length(pe)) return(p)
    tryCatch(mediaplanr::build_scenario(p, edits = pe, name = "staging"),
             error = function(e) p)
  })

  # --- grid --------------------------------------------------------------
  # Typing in a cell is an operation like any other: a `set` when the row
  # exists, an `add` when it does not. The second case is why a flight plan's
  # empty weeks are now editable rather than silently swallowing the edit.
  observeEvent(input$grid_edit, {
    e <- input$grid_edit
    req(e$key)
    op <- cell_edit_op(active_plan(), e$key, e$value)
    if (is.null(op)) {
      showNotification("That cell is not part of the plan.", type = "warning"); return()
    }
    trial <- tryCatch(
      mediaplanr::build_scenario(active_plan(), edits = stage_op(pending(), op),
                                 name = "staging"),
      error = function(e) e)
    if (inherits(trial, "error")) {
      showNotification(conditionMessage(trial), type = "error", duration = 8); return()
    }
    pending(stage_op(pending(), op))
  })

  output$scn_grid <- reactable::renderReactable({
    plan_grid(staged_plan(), baseline = active_plan(), editable = TRUE)
  })

  output$grid_badge <- renderUI({
    n <- length(pending())
    if (!n) return(tags$span(class = "badge bg-light text-muted", "no pending changes"))
    tags$span(class = "badge bg-warning text-dark",
              sprintf("%d pending change%s", n, if (n == 1) "" else "s"))
  })

  output$scn_pending_note <- renderUI({
    p <- active_plan(); pe <- pending()
    if (!length(pe)) {
      return(div(class = "small text-muted mb-2",
                 "Edit the grid or stage an operation first."))
    }
    new_total <- tryCatch(sum(staged_plan()@data$planned_spend),
                          error = function(e) NA_real_)
    old_total <- sum(p@data$planned_spend)
    div(class = "small mb-2",
        sprintf("%d change%s - ", length(pe), if (length(pe) == 1) "" else "s"),
        tags$b(fmt_money(new_total)),
        tags$span(class = if (isTRUE(new_total >= old_total)) "text-success" else "text-danger",
                  " (", fmt_delta(new_total - old_total), ")"))
  })

  # The staged list, in words, with an x per row. Until now the only affordance
  # was Discard All: one mistyped cell meant losing every other staged change.
  output$scn_staged_list <- renderUI({
    pe <- pending()
    if (!length(pe)) return(NULL)
    p <- active_plan()
    rows <- lapply(seq_along(pe), function(i) {
      div(class = "d-flex align-items-start gap-2 py-1 border-bottom small",
          tags$span(class = "text-muted", style = "min-width:1.2rem;", paste0(i, ".")),
          tags$span(class = "flex-grow-1", describe_op(pe[[i]], p)),
          actionLink(paste0("undo_op_", i), label = NULL, icon = icon("xmark"),
                     class = "text-muted", title = "Undo this change"))
    })
    div(class = "mb-2", rows)
  })

  # One observer per slot, created once. Shiny inputs are not removed when the
  # UI that made them goes away, so these are registered up to a ceiling rather
  # than rebuilt per render -- rebuilding would stack duplicate handlers.
  lapply(seq_len(50), function(i) {
    observeEvent(input[[paste0("undo_op_", i)]], {
      pe <- pending()
      if (i > length(pe)) return()
      pending(pe[-i])
      showNotification("Change removed.", type = "message", duration = 2)
    }, ignoreInit = TRUE)
  })

  observeEvent(input$scn_discard, {
    pending(list())
    showNotification("Staged edits discarded.", type = "message", duration = 3)
  })

  observeEvent(input$scn_save, {
    pe <- pending()
    if (!length(pe)) {
      showNotification("Nothing staged to save.", type = "warning"); return()
    }
    if (!nzchar(input$new_name %||% "")) {
      showNotification("A scenario name is required.", type = "warning"); return()
    }
    p <- active_plan()
    res <- tryCatch(
      mediaplanr::build_scenario(
        p, edits = pe, name = input$new_name,
        nickname = input$new_nickname %||% "",
        status = input$new_status %||% "in development"),
      error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 10); return()
    }
    label <- store_add(st, res)
    pending(list())
    updateTextInput(session, "new_name", value = "")
    updateTextInput(session, "new_nickname", value = "")
    bump(bump() + 1)
    showNotification(paste0("Saved scenario '", label, "'."), type = "message")
  })

  # --- chat --------------------------------------------------------------
  observeEvent(input$plan_chat_user_input, {
    a <- agent()
    if (is.null(a)) {
      chat_append("plan_chat", paste(
        "The assistant needs an `ANTHROPIC_API_KEY` environment variable.",
        "Set it and restart the app; meanwhile the grid and quick operations",
        "work as normal.")); return()
    }
    if (!store_has_plan(st)) {
      chat_append("plan_chat", "Load a plan on the **Plan** page first."); return()
    }
    n0 <- length(a$get_turns())
    stream <- a$stream_async(input$plan_chat_user_input)
    promises::then(
      chat_append("plan_chat", stream),
      onFulfilled = function(value) {
        # The agent may have created scenarios; refresh everything that reads
        # the store.
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
  })
}
