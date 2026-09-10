# ---------------------------------------------------------------------------
# Build: three routes to a MediaPlan, each showing only what its route needs.
#
# Design a plan / From a sample / From a file, in that order, because designing
# is the route that needs no input to start from. The order on screen is the
# order in the head -- load, see what you loaded, build -- so the action bar is
# at the top and the preview of the raw file sits directly under it rather than
# beneath the outputs it explains.
#
# The routes cannot share input ids: a navset renders every panel into the DOM,
# so `smp_grain` and `map_grain` are two controls, seeded from the same place.
# Two of the three omit the mode, week and spend pickers entirely -- a sample
# knows its own columns and the scaffold wrote them, so there is nothing to
# choose. Both ship their mapping as `uploaded()$defaults` instead.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Route three: design a plan from nothing.
#
# The other two routes start from data and ask about its columns. This one has
# no data, so it asks about the PLAN -- what identifies a line item, over what
# weeks, funded by how much -- and generates the rest. Because the generator
# knows every column it wrote, this subtab shows no column pickers at all: it
# ships its own mapping, exactly as a sample does.
#
# The grain is authored as an indented outline in ONE textarea. That is what
# keeps arbitrary depth possible without a forest of dynamically generated
# input ids, and it is what lets the assistant rewrite the tree in one call.
# The table underneath is where it becomes legible.
# ---------------------------------------------------------------------------

build_from_scratch <- nav_panel(
  title = "Design a plan",

  div(
    class = "mp-actionbar d-flex flex-wrap align-items-center gap-2 mb-3",
    div(class = "flex-grow-1 small text-muted px-2",
        uiOutput("scaf_status", inline = TRUE)),
    uiOutput("scaf_build_btn", inline = TRUE)
  ),

  # What the assistant can do, before you have opened it. The drawer starts
  # closed, so without this a planner has no way of knowing there is anything
  # to ask; clicking a chip opens the drawer with that request already sent.
  div(
    class = "d-flex flex-wrap align-items-center gap-2 mb-3",
    span(class = "small text-muted", "Or let the assistant do it:"),
    skills_strip("build")
  ),

  uiOutput("scaf_target"),
  uiOutput("scaf_problems"),

  layout_columns(
    col_widths = c(7, 5),

    card(
      card_header("The grain"),
      card_body(
        selectizeInput("scaf_dims", "Dimensions, coarsest first",
                       choices = NULL, multiple = TRUE, width = "100%",
                       options = list(create = TRUE,
                                      plugins = list("remove_button", "drag_drop"),
                                      placeholder = "channel, partner, ...")),
        div(class = "form-text mb-2 mt-n2",
            "Suggestions only \u2014 type any name you like. The order here is the ",
            "order of the outline below."),

        textAreaInput("scaf_tree", "The plan, as an outline", rows = 10,
                      width = "100%", resize = "vertical"),
        div(class = "form-text mt-n2",
            tags$b("name"), " then any of: ",
            tags$code("55%"), " share \u00b7 ",
            tags$code("x3"), " weight \u00b7 ",
            tags$code("=500k"), " a fixed amount \u00b7 ",
            tags$code("~ramp"), " pacing \u00b7 ",
            tags$code("2026-08-03..2026-09-28"), " its own dates.",
            tags$br(),
            "Indent to nest. A parent\u2019s split rule follows its children\u2019s markers."
        )
      )
    ),

    div(
      card(
        card_header("Time"),
        card_body(
          radioButtons("scaf_basis", label = NULL,
                       choices = c("One row per line item per week" = "weekly",
                                   "One row per flight (in-market dates)" = "flights",
                                   "No time dimension" = "none"),
                       selected = "weekly"),
          conditionalPanel(
            "input.scaf_basis != 'none'",
            layout_columns(
              col_widths = c(6, 6),
              dateInput("scaf_start", "First week", value = NULL,
                        weekstart = 1, width = "100%"),
              numericInput("scaf_weeks", "Weeks", value = 13, min = 1, step = 1)
            ),
            selectInput("scaf_week_start", "Weeks begin on",
                        choices = c("Monday", "Sunday", "Tuesday", "Wednesday",
                                    "Thursday", "Friday", "Saturday")),
            div(class = "form-text mt-n2",
                "Daily and monthly are views, not grains \u2014 a plan is held as ",
                "weeks and re-cut on the Review page.")
          )
        )
      ),

      card(
        class = "mt-3",
        card_header("The money"),
        card_body(
          layout_columns(
            col_widths = c(7, 5),
            numericInput("scaf_total", "Total budget", value = NULL, min = 0),
            selectInput("scaf_round", "Round to",
                        choices = c("cent" = "0.01", "1" = "1",
                                    "100" = "100", "1,000" = "1000"),
                        selected = "0.01")
          ),
          selectInput("scaf_pacing", "Default pacing", choices = NULL),
          div(class = "form-text mt-n2",
              "Every level splits an integer, so the line items add up to the ",
              "total exactly \u2014 and so does every channel subtotal.")
        )
      )
    )
  ),

  card(
    class = "mt-3",
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
          span(icon("sitemap"), " Line items this makes"),
          uiOutput("scaf_count", inline = TRUE))
    ),
    card_body(class = "pt-2", reactableOutput("scaf_outline"))
  ),

  accordion(
    id = "scaf_opts_acc", open = FALSE, class = "mt-3",

    accordion_panel(
      "What it buys (optional)", value = "units", icon = icon("chart-simple"),
      layout_columns(
        col_widths = c(6, 6),
        selectInput("scaf_unit_type", "Unit type", choices = NULL),
        numericInput("scaf_rate", "Rate", value = NULL, min = 0)
      ),
      div(class = "form-text mt-n2",
          "Spend, units and rate are one identity, so stating the rate is ",
          "enough \u2014 the units follow. Impressions are priced as a CPM.")
    ),

    accordion_panel(
      "Plan details", value = "meta", icon = icon("tag"),
      layout_columns(
        col_widths = c(6, 6),
        textInput("scaf_name", "Name", placeholder = "Q3 2026 Media Plan"),
        textInput("scaf_nickname", "Nickname", placeholder = "baseline")
      ),
      layout_columns(
        col_widths = c(6, 6),
        textInput("scaf_advertiser", "Advertiser", placeholder = "Acme Corp"),
        textInput("scaf_planner", "Planner", placeholder = "your name")
      ),
      div(class = "form-text mt-n2",
          "A new plan starts ", tags$i("in development"),
          " \u2014 promote it on the Review page once you can see it.")
    )
  )
)

build_from_sample <- nav_panel(
  title = "From a sample",

  div(
    class = "mp-actionbar d-flex flex-wrap align-items-center gap-2 mb-3",
    actionButton("smp_load_weekly", "Weekly sample", icon = icon("database"),
                 class = "btn-sm btn-outline-secondary"),
    actionButton("smp_load_flights", "Flight sample", icon = icon("plane-departure"),
                 class = "btn-sm btn-outline-secondary"),
    div(class = "flex-grow-1 small text-muted px-2",
        uiOutput("smp_source_status", inline = TRUE)),
    uiOutput("smp_build_btn", inline = TRUE)
  ),

  uiOutput("smp_mapping_status"),

  # Open on arrival, closed by the server once Build succeeds: it has done its
  # job, and the registry underneath is what matters next.
  accordion(
    id = "smp_prev_acc", class = "mb-3", open = "preview",
    accordion_panel(
      "Sample preview", value = "preview", icon = icon("table"),
      uiOutput("smp_file_note"),
      reactableOutput("smp_raw_preview")
    )
  ),

  accordion(
    id = "smp_opts_acc", open = FALSE, class = "mb-2",

    accordion_panel(
      "Grain — what identifies a row", value = "grain", icon = icon("layer-group"),
      selectizeInput("smp_grain", label = NULL, choices = NULL, multiple = TRUE,
                     width = "100%",
                     options = list(plugins = list("remove_button", "drag_drop"),
                                    placeholder = "channel, partner, week...")),
      div(class = "form-text mt-n2",
          "Order matters: coarsest first (channel, then partner). ",
          "The sample is already mapped — change this only to regroup it.")
    ),

    accordion_panel(
      "What it buys (optional)", value = "units", icon = icon("chart-simple"),
      layout_columns(
        col_widths = c(4, 4, 4),
        selectInput("smp_unit_type", "Unit type column", choices = NULL),
        selectInput("smp_units", "Planned units column", choices = NULL),
        selectInput("smp_rate", "Rate column", choices = NULL)
      ),
      div(class = "form-text mt-n2",
          "Spend, units and rate are bound by one identity, so mapping any two ",
          "computes the third. Impressions are priced as a CPM, everything else ",
          "per unit.")
    ),

    accordion_panel(
      "Plan details", value = "meta", icon = icon("tag"),
      layout_columns(
        col_widths = c(6, 6),
        textInput("smp_name", "Name", placeholder = "Q2 2026 Media Plan"),
        textInput("smp_nickname", "Nickname", placeholder = "baseline")
      ),
      layout_columns(
        col_widths = c(6, 6),
        textInput("smp_advertiser", "Advertiser", placeholder = "Acme Corp"),
        textInput("smp_planner", "Planner", placeholder = "your name")
      ),
      div(class = "form-text mt-n2",
          "A new plan starts ", tags$i("in development"),
          " — promote it on the Review page once you can see it.")
    )
  )
)

build_from_file <- nav_panel(
  title = "From a file",

  div(
    class = "mp-actionbar d-flex flex-wrap align-items-end gap-2 mb-3",
    div(class = "mp-upload",
        fileInput("plan_upload", label = NULL,
                  accept = c(".csv", ".tsv", ".xlsx", ".xls"),
                  placeholder = "CSV or Excel...",
                  buttonLabel = icon("upload"), width = "100%")),
    div(class = "flex-grow-1 small text-muted px-2 pb-2",
        uiOutput("file_source_status", inline = TRUE)),
    div(class = "pb-2", uiOutput("file_build_btn", inline = TRUE))
  ),

  uiOutput("file_mapping_status"),

  accordion(
    id = "file_prev_acc", class = "mb-3", open = "preview",
    accordion_panel(
      "Uploaded file preview", value = "preview", icon = icon("table"),
      uiOutput("file_note"),
      reactableOutput("file_raw_preview")
    )
  ),

  layout_columns(
    col_widths = c(7, 5),

    card(
      card_header("Map the columns"),
      card_body(
        radioButtons("map_mode", "The file has",
                     choices = c("One row per line item per week" = "weekly",
                                 "One row per flight (in-market dates)" = "flights"),
                     selected = "weekly"),
        selectizeInput("map_grain", "Grain (what identifies a row)",
                       choices = NULL, multiple = TRUE, width = "100%",
                       options = list(plugins = list("remove_button", "drag_drop"),
                                      placeholder = "channel, partner, week...")),
        div(class = "form-text mb-2 mt-n2",
            "Order matters: coarsest first (channel, then partner)."),
        conditionalPanel(
          "input.map_mode == 'flights'",
          layout_columns(
            col_widths = c(6, 6),
            selectInput("map_flight_start", "Flight start column", choices = NULL),
            selectInput("map_flight_end", "Flight end column", choices = NULL)
          ),
          selectInput("map_week_start", "Weeks begin on",
                      choices = c("Monday", "Sunday", "Tuesday", "Wednesday",
                                  "Thursday", "Friday", "Saturday")),
          div(class = "form-text mb-2 mt-n2",
              "Each buy is spread across its days and gathered into these weeks.")
        ),
        conditionalPanel(
          "input.map_mode != 'flights'",
          selectInput("map_week", "Week column", choices = NULL)
        ),
        selectInput("map_spend", "Planned spend column", choices = NULL),

        # Most plans record no units, so this stays folded away until asked for.
        accordion(
          open = FALSE,
          accordion_panel(
            "What it buys (optional)", icon = icon("chart-simple"),
            selectInput("map_unit_type", "Unit type column", choices = NULL),
            selectInput("map_units", "Planned units column", choices = NULL),
            selectInput("map_rate", "Rate column", choices = NULL),
            div(class = "form-text mt-n2",
                "Map any two of spend, units and rate; the third is computed.")
          )
        )
      )
    ),

    card(
      card_header("Plan details"),
      card_body(
        textInput("meta_name", "Name", placeholder = "Q2 2026 Media Plan"),
        textInput("meta_nickname", "Nickname", placeholder = "baseline"),
        textInput("meta_advertiser", "Advertiser", placeholder = "Acme Corp"),
        textInput("meta_planner", "Planner", placeholder = "your name"),
        div(class = "form-text",
            "An Excel workbook loads one scenario per sheet, and each sheet name ",
            "becomes that scenario's nickname."),
        div(class = "form-text mt-2",
            "A new plan starts ", tags$i("in development"),
            " — promote it on the Review page.")
      )
    )
  )
)

nav_panel(
  title = "Build",
  div(
    class = "container-fluid py-3",

    navset_card_tab(
      id = "build_mode",
      build_from_scratch,
      build_from_sample,
      build_from_file
    ),

    # Below the subtabs, so both routes fill the same registry and there is only
    # ever one of it in the DOM.
    card(
      class = "mt-3",
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span(icon("layer-group"), " Plans in this session"),
            uiOutput("registry_actions", inline = TRUE))
      ),
      card_body(class = "pt-2", uiOutput("registry_body"))
    )
  )
)
