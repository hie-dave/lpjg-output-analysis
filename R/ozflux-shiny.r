#'
#' Run a shiny app which generates interactive plots of site-level ozflux
#' simulations against predictions.
#'
#' @param sources List of [DGVMTools::Source] objects to plot
#' @param launch.browser If `TRUE`, the app will be launched in the system's default web browser. Otherwise, it will run in the R session's built-in viewer (if available) or the console.
#' @param allow_unrecognised If `TRUE`, sources with unrecognised formats will be ignored with a warning. If `FALSE`, the presence of any unrecognised source formats will cause an error. This is useful to avoid silent misconfiguration where a user accidentally provides sources that don't actually contain any OZFLUX data.
#'
#' @details Select multiple outputs and choose columns separately for each output.
#' Timeseries are aligned by date and, where present, stand, patch, and indiv.
#' Different aggregation levels remain separate series. Annual values are dated
#' December 31. Colour, line type, and point type can vary by column, quantity,
#' source, stand, patch, or cohort (when present). The combined plot uses a shared value axis. Observed
#' comparisons and subannual plots use the first selected output.
#'
#' @return Returns a [shiny::shinyApp] object
#' @import shiny
#' @export
#'
ozflux_shiny <- function(sources,
                         launch.browser = FALSE,
                         allow_unrecognised = FALSE) {
  sources <- sanitise_sources(sources)
  log_info("Starting ozflux_shiny with ", length(sources), " source(s)")

  gridcells <- read_ozflux_sites()
  gridcells <- get_available_sites_ozflux(sources,
                                          allow_unrecognised = allow_unrecognised)

  get_site_output_files <- function(site_name) {
    lapply(sources, function(source) {
      if (source@format@id != "OZFLUX") return(character(0))
      root <- get_ozflux_path(source)
      directory <- file.path(root, site_name,
        get_output_dir(file.path(root, "outputs.ins")))
      ozflux_fresh_output_files(directory)
    })
  }

  get_site_vars <- function(site_name, files = get_site_output_files(site_name)) {
    vars <- unlist(lapply(basename(unlist(files)), quant_from_file_name))
    vars <- vars[!is.na(vars) & vars != "" & !(vars %in% c("*", "guess_out", "guess_err"))]
    unique(sort(vars))
  }

  get_var_group_name <- function(var_id) {
    log_debug("Resolving group name for variable '", var_id, "'")
    metadata <- guess_metadata_from_filename(paste0(var_id, ".out"))
    if (!is.null(metadata) && nzchar(metadata@name)) {
      log_debug("Using metadata group name '", metadata@name, "' for variable '", var_id, "'")
      return(metadata@name)
    }
    log_debug("No metadata group name for variable '", var_id, "'; using readable_name fallback")
    readable_name(var_id)
  }

  get_var_choice_label <- function(var_id) {
    log_debug("Resolving choice label for variable '", var_id, "'")
    display_name <- get_display_name(paste0(var_id, ".out"))
    if (!is.null(display_name) && nzchar(display_name)) {
      log_debug("Using display name '", display_name, "' for variable '", var_id, "'")
      return(display_name)
    }
    log_debug("No display name for variable '", var_id, "'; using readable fallback label")
    paste0(readable_name(var_id), " (", var_id, ")")
  }

  make_var_choices <- function(var_ids) {
    log_debug("Building variable choices from ", length(var_ids), " variable id(s)")
    if (length(var_ids) < 1) {
      log_debug("No variables provided; returning empty variable choices")
      return(list())
    }

    groups <- unlist(lapply(var_ids, get_var_group_name), use.names = FALSE)
    labels <- unlist(lapply(var_ids, get_var_choice_label), use.names = FALSE)

    table <- data.frame(
      id = var_ids,
      group = groups,
      label = labels,
      stringsAsFactors = FALSE
    )
    table <- table[order(table$group, table$label, table$id), , drop = FALSE]

    grouped <- split(table, table$group)
    choices <- lapply(grouped, function(group_tbl) {
      group_labels <- make.unique(group_tbl$label)
      setNames(group_tbl$id, group_labels)
    })

    ordered <- choices[order(names(choices))]
    log_debug("Constructed ", length(ordered), " variable group(s): ",
              paste(names(ordered), collapse = ", "))
    ordered
  }

  get_site_layers <- function(site_name, var_name) {
    log_debug("Getting site layers for site '", site_name, "' and variable '", var_name, "'")
    var <- sanitise_variable(var_name)
    out_layers <- c()
    for (source in sources) {
      if (source@format@id != "OZFLUX") {
        log_debug("Skipping non-OZFLUX source '", source@name, "' while querying layers")
        next
      }
      layers <- tryCatch(
        available_layers_ozflux(source, var, sites = site_name),
        error = function(e) {
          log_warning("Failed to read layers for site '", site_name,
                      "', variable '", var_name, "', source '", source@name,
                      "': ", e$message)
          c()
        }
      )
      out_layers <- c(out_layers, layers)
    }
    resolved <- unique(sort(out_layers))
    log_debug("Resolved ", length(resolved), " layer(s) for site '", site_name,
              "', variable '", var_name, "'")
    resolved
  }

  get_observation_layer_choices <- function(var_name) {
    log_debug("Looking up observation layer choices for variable '", var_name, "'")
    readers <- find_readers_for_var(trim_dave(var_name))
    if (length(readers) == 0) {
      log_debug("No readers found for variable '", var_name, "'")
      return(c())
    }
    choices <- names(readers)
    log_debug("Resolved ", length(choices), " observation layer choice(s) for variable '", var_name, "'")
    choices
  }

  first_site <- gridcells$Name[1]
  initial_vars <- get_site_vars(first_site)
  initial_choices <- make_var_choices(initial_vars)
  initial_var <- if (length(initial_vars) > 0) initial_vars[1] else NULL
  initial_layers <- if (!is.null(initial_var)) get_site_layers(first_site, initial_var) else c()
  initial_obs_layers <- if (!is.null(initial_var)) get_observation_layer_choices(initial_var) else c()
  log_info("Initial site '", first_site, "' has ", length(initial_vars),
           " variable(s), ", length(initial_layers), " layer(s), and ",
           length(initial_obs_layers), " observation layer choice(s)")

  cached_vars <- c()
  cached_data <- c()

  # Function to get the variable with the specified name.
  get_var <- function(name) {
    log_debug("Sanitising variable '", name, "'")
    sanitise_variable(name)
  }

  # Function to read data for the specified variable/site pair, either from
  # the cache (if it contains this key), or from disk (saving to cache).
  get_data <- function(var_name, site_name, layers) {
    log_debug("Requesting data for site '", site_name, "', variable '", var_name,
              "', layer count ", length(layers))
    layer_key <- if (is.null(layers) || length(layers) < 1) "<default>" else paste(sort(layers), collapse = ",")
    cache_key <- paste(var_name, site_name, layer_key, sep = "::")
    if (cache_key %in% cached_vars) {
      log_debug("Reading ", cache_key, " from cache")
      data <- cached_data[which(cached_vars == cache_key)][[1]]
    } else {
      log_debug("Reading ", cache_key, " from disk")
      var <- get_var(var_name)
      data <- read_data(sources, var, sites = site_name, layers = layers)
      cached_vars <<- c(cached_vars, cache_key)
      cached_data <<- c(cached_data, data)
      log_debug("Cached data for key ", cache_key, "; cache now has ",
                length(cached_vars), " item(s)")
    }
    return(data)
  }

  # Define the GUI for the app.
  ui <- fluidPage(
    # App title.
    titlePanel("Ozflux Benchmarks"),
    shinyjs::useShinyjs(),

    # Sidebar layout with input and output definitions.
    sidebarLayout(

      # Sidebar panel for inputs.
      sidebarPanel(
        # Custom CSS to show a "loading" banner when data is being processed.
        tags$head(
          tags$style(HTML(".multicol {-webkit-column-count: 3; /* Chrome, Safari, Opera */-moz-column-count: 3; /* Firefox */column-count: 3;}")),
          tags$style(type="text/css", "#loadmessage {position: fixed;top: 0px;left: 0px;width: 100%;padding: 5px 0px 5px 0px;text-align: center;font-weight: bold;font-size: 100%;color: #000000;background-color: #CCFF66;z-index: 105;}"),
          tags$style(type = "text/css", "
            :root {
              --plot-height: 70vh;
            }

            #timeseries,
            #pvo,
            #subannual {
              height: var(--plot-height) !important;
            }

            #site ~ .selectize-control .selectize-dropdown-content,
            #var ~ .selectize-control .selectize-dropdown-content {
              max-height: 420px;
            }
          ")
        ),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")),

        # The input controls.
        selectInput(inputId = "site",
                    label = "Site",
                    gridcells$Name,
                    selected = gridcells$Name[1]),
        selectInput(inputId = "var",
                    label = "Outputs",
                    choices = initial_choices,
                    selected = initial_var, multiple = TRUE),
        uiOutput("quantity_layers"),
        selectInput("colour_by", "Colour", c("Vary by layer" = "Layer",
          "Vary by quantity" = "Quantity", "Vary by source" = "Source",
          "Constant" = "constant")),
        selectInput("line_by", "Line type", c("Constant" = "constant",
          "None" = "none", "Vary by layer" = "Layer",
          "Vary by quantity" = "Quantity", "Vary by source" = "Source")),
        selectInput("point_by", "Point type", c("None" = "none",
          "Constant" = "constant", "Vary by layer" = "Layer",
          "Vary by quantity" = "Quantity", "Vary by source" = "Source")),
        helpText("Observed comparisons and subannual plots use the first selected output. Combined timeseries use a shared value axis; units are shown in tooltips."),
        selectInput(inputId = "obs_layer",
              label = "Observed Layer",
              choices = c("Auto-detect" = "", initial_obs_layers),
              selected = ""),
        checkboxInput(inputId = "do_timeseries",
                      label = "Create Timeseries Plot",
                      value = TRUE),
        checkboxInput(inputId = "do_pvo",
                      label = "Create Predicted vs. Observed Plot",
                      value = FALSE),
        checkboxInput(inputId = "do_subannual",
                      label = "Create Subannual Plot",
                      value = FALSE),
        sliderInput(inputId = "plot_height",
                    label = "Plot Height",
                    min = 40,
                    max = 100,
                    value = 70,
                    step = 1)
      ),

      # Main panel for displaying outputs.
      mainPanel(
        plotly::plotlyOutput(outputId = "timeseries"),
        plotly::plotlyOutput(outputId = "pvo"),
        plotly::plotlyOutput(outputId = "subannual")
      )
    ),

    fluidRow(
      column(
        width = 12,
        tags$details(
          tags$summary("Diagnostics"),
          fluidRow(
            column(
              width = 4,
              selectInput(
                inputId = "log_level",
                label = "Log Level",
                choices = c(
                  "Error" = as.character(get_global("LOG_LEVEL_ERROR")),
                  "Warning" = as.character(get_global("LOG_LEVEL_WARNING")),
                  "Information" = as.character(get_global("LOG_LEVEL_INFORMATION")),
                  "Diagnostic" = as.character(get_global("LOG_LEVEL_DIAGNOSTIC")),
                  "Debug" = as.character(get_global("LOG_LEVEL_DEBUG"))
                ),
                selected = as.character(get_log_level())
              )
            ),
            column(
              width = 8,
              tags$div(style = "margin-top: 25px;"),
              actionButton("clear_logs", "Clear Logs")
            )
          ),
          tags$div(
            id = "log_output_container",
            style = "margin-top: 8px; max-height: 360px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; background-color: #fafafa; width: 100%;",
            verbatimTextOutput("log_output", placeholder = TRUE)
          )
        )
      )
    )
  )

  # Define server logic required to draw a plot ----
  server <- function(input, output, session) {
    log_debug("Initialising shiny server")
    log_lines <- reactiveVal(character(0))
    max_log_lines <- 1000
    writer_name <- paste0("ozflux_shiny_", as.integer(Sys.time()), "_", as.integer(stats::runif(1, 0, 1000000)))

    append_log_line <- function(line) {
      isolate({
        current <- log_lines()
        clean <- sub("\n$", "", line)
        updated <- c(current, clean)
        if (length(updated) > max_log_lines) {
          updated <- tail(updated, max_log_lines)
        }
        log_lines(updated)
      })
    }

    register_log_writer(writer_name, function(line, level) {
      append_log_line(line)
    })

    onSessionEnded(function() {
      unregister_log_writer(writer_name)
    })

    output$log_output <- renderText({
      paste(log_lines(), collapse = "\n")
    })

    observeEvent(log_lines(), {
      shinyjs::runjs(
        "setTimeout(function() {
           var el = document.getElementById('log_output_container');
           if (el) {
             el.scrollTop = el.scrollHeight;
           }
         }, 0);"
      )
    }, ignoreInit = TRUE)

    observeEvent(input$clear_logs, {
      log_lines(character(0))
      log_info("Cleared in-app log display")
    }, ignoreInit = TRUE)

    observeEvent(input$log_level, {
      req(input$log_level)
      level <- suppressWarnings(as.integer(input$log_level))
      if (is.na(level)) {
        log_warning("Invalid log level selected in UI: '", input$log_level, "'")
        return()
      }
      set_log_level(level)
      log_info("Changed log level via UI to ", level_to_string(level),
               " (", level, ")")
    }, ignoreInit = TRUE)

    observeEvent(input$plot_height, {
      req(input$plot_height)
      height_vh <- suppressWarnings(as.integer(input$plot_height))
      if (is.na(height_vh)) {
        log_warning("Invalid plot height selected in UI: '", input$plot_height,
                    "'")
        return()
      }
      height_vh <- max(40L, min(100L, height_vh))
      shinyjs::runjs(
        paste0("document.documentElement.style.setProperty('--plot-height', '",
               height_vh, "vh');")
      )
      log_debug("Set plot height to ", height_vh, "vh")
    }, ignoreInit = FALSE)

    # Poll once for a shared snapshot: choices and plotted files must agree.
    fresh_files <- reactivePoll(2000, session,
      checkFunc = function() {
        req(input$site)
        list(site = input$site, files = get_site_output_files(input$site))
      },
      valueFunc = function() get_site_output_files(input$site))

    available_vars <- reactive(get_site_vars(input$site, fresh_files()))

    observeEvent(available_vars(), {
      vars <- available_vars()
      selected <- intersect(input$var, vars)
      if (!length(selected)) selected <- head(vars, 1)
      updateSelectInput(session, "var", choices = make_var_choices(vars),
                        selected = selected)
    }, ignoreNULL = FALSE)

    layer_id <- function(quantity) paste0("layers_", paste(as.integer(charToRaw(quantity)), collapse = "_"))

    output$quantity_layers <- renderUI({
      req(input$site)
      tagList(lapply(intersect(input$var, available_vars()), function(quantity) {
        layers <- setdiff(get_site_layers(input$site, quantity),
                          c("Lon", "Lat", "Year", "Day", "stand", "patch", "indiv"))
        previous <- isolate(input[[layer_id(quantity)]])
        selectizeInput(layer_id(quantity), paste(quantity, "columns"),
          choices = layers,
          selected = if (is.null(previous)) layers else intersect(previous, layers),
          multiple = TRUE)
      }))
    })

    observeEvent(input$var, {
      req(input$var)
      updateSelectInput(session, "obs_layer",
        choices = c("Auto-detect" = "", get_observation_layer_choices(input$var[1])),
        selected = "")
    })

    timeseries_data <- reactive({
      req(input$site, input$var)
      records <- list()
      files_by_source <- fresh_files()
      for (quantity in intersect(input$var, available_vars())) {
        layers <- input[[layer_id(quantity)]]
        if (!length(layers)) next
        for (source_index in seq_along(sources)) {
          source <- sources[[source_index]]
          files <- files_by_source[[source_index]]
          files <- files[basename(files) %in% paste0(quantity, c(".out", ".out.gz"))]
          if (!length(files)) next
          # Read each output independently: entity columns must never become
          # prediction layers or be discarded by a Field merge.
          frame <- read.table(files[1], header = TRUE, check.names = FALSE)
          records[[length(records) + 1L]] <- list(data = frame,
            quantity = quantity, source = source@id, layers = layers)
        }
      }
      validate(need(length(records) > 0, "Select at least one output column."))
      ozflux_timeseries_data(records)
    })

    observeEvent(timeseries_data(), {
      choices <- c("Constant" = "constant", "Vary by layer" = "Layer",
        "Vary by quantity" = "Quantity", "Vary by source" = "Source")
      choices <- c(choices, ozflux_entity_choices(timeseries_data()))
      for (id in c("colour_by", "line_by", "point_by")) {
        available <- if (id == "colour_by") choices else c("None" = "none", choices)
        selected <- isolate(input[[id]])
        if (!(selected %in% available)) selected <- "constant"
        updateSelectInput(session, id, choices = available, selected = selected)
      }
    })

    plots <- reactive({
      req(input$site)
      req(input$var)
      req(input[[layer_id(input$var[1])]])
      validate(need(length(input[[layer_id(input$var[1])]]) > 0,
                    "Please select at least one layer to plot."))

      # Parse inputs.
      site <- input$site
      var_name <- input$var[1]
      selected_layers <- input[[layer_id(var_name)]]
      log_debug("Preparing plots for site '", site, "', variable '", var_name,
            "', selected layers: ", paste(selected_layers, collapse = ", "))

      validate(need(var_name %in% available_vars(),
                    "Selected variable is not available for this site."))

      ncol <- 1#input$ncol
      index <- which(gridcells$Name == site)
      lat <- gridcells$Lat[index]
      lon <- gridcells$Lon[index]

      # Read data (from cache or from disk).
      data <- get_data(var_name, site, selected_layers)

      # Filter to the selected grid point.
      gridcell <- get_gridcell(data, lat, lon, site)
      log_debug("Resolved gridcell for site '", site, "' at lat/lon (",
            lat, ", ", lon, ")")

      obs_candidates <- get_observation_layer_choices(var_name)
      obs_layer <- input$obs_layer
      if (identical(obs_layer, "")) {
        obs_layer <- NULL
      }
      if (!is.null(obs_layer) && !(obs_layer %in% names(gridcell@data))) {
        obs_layer <- NULL
      }
      if (is.null(obs_layer)) {
        avail_obs <- obs_candidates[obs_candidates %in% names(gridcell@data)]
        if (length(avail_obs) > 0) {
          obs_layer <- avail_obs[1]
        }
      }
      log_debug("Resolved observation layer: ",
                if (is.null(obs_layer)) "<none>" else paste0("'", obs_layer, "'"))

      do_pvo <- isTRUE(input$do_pvo) && !is.null(obs_layer)
      log_debug("Plot toggles: timeseries=", isTRUE(input$do_timeseries),
                ", pvo=", isTRUE(input$do_pvo), " (effective=", do_pvo,
                "), subannual=", isTRUE(input$do_subannual))

      y_label <- get_y_label(var_name)
      var_display <- get_display_name(paste0(var_name, ".out"))
      if (is.null(var_display) || !nzchar(var_display)) {
        var_display <- readable_name(var_name)
      }

      graphs <- create_plots(gridcell, y_label, ncol = ncol,
        do_timeseries = FALSE, do_pvo = do_pvo,
        do_subannual = input$do_subannual, obs_lyr = obs_layer)

      title <- paste(site, var_display)
      if (!is.null(graphs$pvo)) {
        graphs$pvo <- plotly::layout(graphs$pvo, title = title)
      }
      if (input$do_subannual) {
        graphs$subannual <- plotly::layout(graphs$subannual, title = title)
      }

      log_debug("Finished creating plots for site '", site,
                "', variable '", var_name, "'")

      graphs
    })

    output$timeseries <- plotly::renderPlotly({
      log_debug("Rendering timeseries output")
      if (!input$do_timeseries) {
        log_debug("Timeseries plot disabled by toggle")
        return(NULL)
      }
      validate(need(input$line_by != "none" || input$point_by != "none",
                    "Enable lines or points to draw the timeseries."))
      ozflux_timeseries_plot(timeseries_data(), input$colour_by,
        input$line_by, input$point_by, input$site)
    })

    output$pvo <- plotly::renderPlotly({
      log_debug("Rendering predicted-vs-observed output")
      if (!input$do_pvo) {
        log_debug("Predicted-vs-observed plot disabled by toggle")
        return(NULL)
      }
      p <- plots()$pvo
      validate(need(!is.null(p),
                    "No observed layer is available for this variable/site; predicted vs. observed is disabled."))
      p
    })

    output$subannual <- plotly::renderPlotly({
      log_debug("Rendering subannual output")
      if (input$do_subannual) {
        return(plots()$subannual)
      }
      log_debug("Subannual plot disabled by toggle")
      return(NULL)
    })

    observeEvent(c(input$do_timeseries, input$do_pvo, input$do_subannual), {
      log_debug("Updating plot visibility for toggles")
      if (input$do_timeseries) {
        shinyjs::show("timeseries")
      } else {
        shinyjs::hide("timeseries")
      }
      if (input$do_pvo) {
        shinyjs::show("pvo")
      } else {
        shinyjs::hide("pvo")
      }
      if (input$do_subannual) {
        shinyjs::show("subannual")
      } else {
        shinyjs::hide("subannual")
      }
    })

    log_info("ozflux_shiny server initialised")
  }

  app <- shinyApp(ui = ui, server = server)
  runApp(app, launch.browser = launch.browser)
}

# Align quantities within each source and aggregation level. Different levels
# remain independent, so gridcell values are never replicated across patches.
ozflux_timeseries_data <- function(records) {
  groups <- list()
  series <- list()
  for (i in seq_along(records)) {
    record <- records[[i]]
    frame <- as.data.frame(record$data)
    entities <- intersect(c("stand", "patch", "indiv"), names(frame))
    required <- if ("indiv" %in% entities) c("stand", "patch", "indiv") else
      if ("patch" %in% entities) c("stand", "patch") else entities
    if (!all(required %in% entities)) stop("Missing entity columns in ", record$quantity)
    frame$Date <- calc_date(frame)
    if (anyNA(frame$Date)) stop("Invalid dates in ", record$quantity)
    keys <- c("Date", entities)
    if (anyDuplicated(frame[keys])) {
      stop("Duplicate date/entity keys in ", record$quantity, " (", record$source, ")")
    }
    layers <- intersect(record$layers, setdiff(names(frame),
      c("Date", "Lon", "Lat", "Year", "Day", "stand", "patch", "indiv")))
    layers <- layers[vapply(frame[layers], is.numeric, logical(1))]
    if (!length(layers)) next
    group <- paste(record$source, paste(entities, collapse = "/"), sep = "::")
    columns <- paste0("value_", i, "_", seq_along(layers))
    present <- paste0("present_", i)
    part <- frame[c(keys, layers)]
    names(part) <- c(keys, columns)
    part[[present]] <- TRUE
    if (is.null(groups[[group]])) groups[[group]] <- part else
      groups[[group]] <- merge(groups[[group]], part, by = keys, all = TRUE)
    series[[length(series) + 1L]] <- list(group = group, columns = columns,
      present = present, layers = layers, entities = entities,
      quantity = record$quantity, source = record$source)
  }
  result <- list()
  for (entry in series) {
    frame <- groups[[entry$group]]
    frame <- frame[!is.na(frame[[entry$present]]), , drop = FALSE]
    # Style at each level uses the full ancestry, while the trace identity
    # always uses the most detailed level available in this output.
    hierarchy <- lapply(seq_len(3L), function(level) {
      keys <- c("stand", "patch", "indiv")[seq_len(level)]
      if (!all(keys %in% entry$entities)) return(rep(NA_character_, nrow(frame)))
      labels <- lapply(keys, function(key) paste(key, frame[[key]]))
      do.call(paste, c(labels, sep = ", "))
    })
    entity <- if (length(entry$entities)) hierarchy[[length(entry$entities)]] else
      rep("Gridcell", nrow(frame))
    for (j in seq_along(entry$columns)) {
      result[[length(result) + 1L]] <- data.frame(
        Date = frame$Date, Value = frame[[entry$columns[j]]],
        Quantity = entry$quantity, Layer = entry$layers[j],
        Source = entry$source, Entity = entity,
        Stand = hierarchy[[1]], Patch = hierarchy[[2]], Cohort = hierarchy[[3]],
        stringsAsFactors = FALSE)
    }
  }
  if (!length(result)) stop("No numeric output columns selected.")
  result <- do.call(rbind, result)
  result$Series <- as.integer(interaction(result$Quantity, result$Layer,
    result$Source, result$Entity, drop = TRUE))
  result[order(result$Series, result$Date), ]
}

ozflux_entity_choices <- function(data) {
  choices <- c("Vary by stand" = "Stand", "Vary by patch" = "Patch",
               "Vary by cohort" = "Cohort")
  choices[vapply(choices, function(column) any(!is.na(data[[column]])), logical(1))]
}

ozflux_timeseries_plot <- function(data, colour_by, line_by, point_by, site) {
  mapping <- function(by, values) {
    if (by %in% c("none", "constant")) return(rep(values[1], nrow(data)))
    levels <- unique(data[[by]])
    rep(values, length.out = length(levels))[match(data[[by]], levels)]
  }
  colour_count <- if (colour_by == "constant") 1L else length(unique(data[[colour_by]]))
  colours <- mapping(colour_by, grDevices::hcl.colors(max(1L, colour_count), "Dark 3"))
  lines <- mapping(line_by, c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot"))
  points <- mapping(point_by, c("circle", "square", "diamond", "cross", "x",
    "triangle-up", "triangle-down", "star"))
  mode <- if (line_by == "none") "markers" else
    if (point_by == "none") "lines" else "lines+markers"
  plot <- plotly::plot_ly()
  for (id in unique(data$Series)) {
    rows <- which(data$Series == id)
    first <- rows[1]
    trace <- data[rows, ]
    label <- paste(trace$Quantity[1], trace$Layer[1], trace$Source[1],
                   trace$Entity[1], sep = " / ")
    plot <- plotly::add_trace(plot, x = trace$Date, y = trace$Value,
      type = "scatter", mode = mode, name = label,
      text = paste(label, get_y_label(trace$Quantity[1]), sep = "<br>"),
      hovertemplate = "%{text}<br>%{x}<br>%{y}<extra></extra>",
      line = if (line_by != "none") list(color = colours[first], dash = lines[first]) else NULL,
      marker = if (point_by != "none") list(color = colours[first], symbol = points[first]) else NULL,
      connectgaps = FALSE)
  }
  plotly::layout(plot, title = site, xaxis = list(title = "Date"),
                 yaxis = list(title = "Value"))
}

ozflux_fresh_output_files <- function(site_out_dir, stale_threshold_secs = 10) {
  log_debug("Looking for output files in ", site_out_dir)
  files <- c(
    list.files(site_out_dir, pattern = "\\.out$", full.names = TRUE),
    list.files(site_out_dir, pattern = "\\.out\\.gz$", full.names = TRUE)
  )
  log_debug("Found ", length(files), " output files in ", site_out_dir)
  if (length(files) < 1) {
    log_debug("No output files found in ", site_out_dir)
    return(character(0))
  }

  info <- file.info(files)
  valid <- !is.na(info$mtime)
  if (!any(valid)) {
    log_debug("No valid output files found in ", site_out_dir, " (no mtime)")
    return(character(0))
  }

  files <- files[valid]
  info <- info[valid, , drop = FALSE]
  newest <- max(info$mtime)
  age_secs <- as.numeric(difftime(newest, info$mtime, units = "secs"))
  fresh <- age_secs <= stale_threshold_secs

  log_debug("Oldest fresh output file in ", site_out_dir, " is ", round(max(age_secs[fresh]), 1), " seconds old")
  log_debug("Found ", sum(fresh), " fresh output files in ", site_out_dir, " (", sum(!fresh), " stale)")

  files[fresh]
}
