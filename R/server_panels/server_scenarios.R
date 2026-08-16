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
    lg <- mediaplanr::line_item_grain(p)
    d  <- p@data
    controls <- lapply(lg, function(g) {
      selectizeInput(paste0("tgt_", g), tools::toTitleCase(g),
                     choices = sort(unique(as.character(d[[g]]))),
                     multiple = TRUE,
                     options = list(placeholder = "all",
                                    plugins = list("remove_button")))
    })
    if (length(p@week_col)) {
      controls <- c(controls, list(selectizeInput(
        "tgt_week", "Week",
        choices = as.character(sort(unique(d[[p@week_col]]))),
        multiple = TRUE,
        options = list(placeholder = "all", plugins = list("remove_button")))))
    }
    tagList(controls)
  })

  output$op_hint <- renderText({
    switch(input$op_kind %||% "total",
      scale      = "1.2 raises matched rows 20%; 0.5 halves them.",
      delta      = "Moves the matched rows' TOTAL by this, keeping their mix. Negative to cut.",
      set        = "Every matched row becomes exactly this -- so the total is this times the row count.",
      delta_each = "Added to EVERY matched row -- so the total moves by this times the row count.",
      total      = "Matched rows are rescaled to sum to this, keeping their mix.")
  })

  observeEvent(input$op_kind, {
    updateNumericInput(session, "op_value",
      value = switch(input$op_kind, scale = 1.1, delta = 50000,
                     delta_each = 5000, set = 25000, total = 500000),
      step = switch(input$op_kind, scale = 0.05, 5000))
  })

  # An operation is applied to the *staged* plan, then flattened back into
  # pending cell edits -- so the grid shows its effect immediately and the user
  # can keep editing on top of it before committing.
  observeEvent(input$op_stage, {
    p <- active_plan()
    lg <- mediaplanr::line_item_grain(p)

    target <- list()
    for (g in lg) {
      v <- input[[paste0("tgt_", g)]]
      if (length(v)) target[[g]] <- v
    }
    if (length(p@week_col)) {
      v <- input$tgt_week
      if (length(v)) target[[p@week_col]] <- as.Date(v)
    }

    val <- input$op_value
    if (!is.finite(val)) {
      showNotification("Enter a numeric value.", type = "warning"); return()
    }
    op <- list(target = if (length(target)) target else NULL)
    op[[input$op_kind]] <- val

    staged <- staged_plan()
    res <- tryCatch(
      mediaplanr::build_scenario(staged, edits = list(op), name = "staging"),
      error = function(e) e)
    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 8); return()
    }

    pending(diff_to_pending(active_plan(), res))
    showNotification("Change staged — review the grid, then Save scenario.",
                     type = "message", duration = 4)
  })

  # The active plan with staged edits applied. Used by the op handler so
  # operations compose, and by save.
  staged_plan <- function() {
    p <- active_plan()
    pe <- pending()
    if (!length(pe)) return(p)
    v <- edits_to_vector(p, pe)
    if (!length(v)) return(p)
    mediaplanr::build_scenario(p, edits = v, name = "staging")
  }

  # Every cell where two plans differ, as pending edits.
  diff_to_pending <- function(base, other) {
    lg <- mediaplanr::line_item_grain(base)
    wk <- base@week_col
    b <- base@data; o <- other@data
    kb <- mediaplanr::line_item(b, base@grain)
    ko <- mediaplanr::line_item(o, other@grain)
    idx <- match(kb, ko)
    changed <- which(!is.na(idx) & b$planned_spend != o$planned_spend[idx])
    out <- list()
    if (!length(changed)) return(out)
    li <- mediaplanr::line_item(b, lg)
    for (i in changed) {
      k <- if (length(wk)) cell_key(li[i], as.character(b[[wk]][i])) else
        cell_key(li[i], NA)
      out[[k]] <- o$planned_spend[idx[i]]
    }
    out
  }

  # --- grid --------------------------------------------------------------
  observeEvent(input$grid_edit, {
    e <- input$grid_edit
    req(e$key)
    pe <- pending()
    pe[[e$key]] <- as.numeric(e$value)
    pending(pe)
  })

  output$scn_grid <- reactable::renderReactable({
    plan_grid(active_plan(), pending = pending(), editable = TRUE)
  })

  output$grid_badge <- renderUI({
    n <- length(pending())
    if (!n) return(tags$span(class = "badge bg-light text-muted", "no pending edits"))
    tags$span(class = "badge bg-warning text-dark",
              sprintf("%d pending edit%s", n, if (n == 1) "" else "s"))
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
        sprintf("%d edit%s · ", length(pe), if (length(pe) == 1) "" else "s"),
        tags$b(fmt_money(new_total)),
        tags$span(class = if (isTRUE(new_total >= old_total)) "text-success" else "text-danger",
                  " (", fmt_delta(new_total - old_total), ")"))
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
    v <- edits_to_vector(p, pe)
    res <- tryCatch(
      mediaplanr::build_scenario(
        p, edits = v, name = input$new_name,
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
