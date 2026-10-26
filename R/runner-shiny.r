#' Explore site-level LPJ-GUESS runner outputs
#'
#' Reads the runner's `index.toml` and simulation manifests from `output_dir`.
#' The instruction file basename is shown as the site. Select several sites,
#' simulations, outputs, and numeric columns to compare their time series.
#' Colour, line type, and point type can vary by site, simulation, source,
#' quantity, column, entity, gridcell, or a factor recorded in a manifest.
#' Output files are read when selected, not when the app starts. Annual values
#' are placed on December 31; `Day` is interpreted as zero-based.
#' Legends show the fields that distinguish traces. The plot title can be
#' overridden, and the y-axis uses registry units when one known quantity has
#' the same units for every selected column.
#'
#' @param output_dir Top-level output directory passed to `lpjguessRunner`.
#' @param run If `TRUE`, launch the app. Set to `FALSE` to return the app.
#' @param launch.browser Passed to [shiny::runApp()] when `run = TRUE`.
#' @details Requires the `lpjguessRunner` package for manifest discovery.
#' @return A [shiny::shinyApp] object, invisibly when launched.
#' @examples
#' \dontrun{runner_shiny("/path/to/runner/out")}
#' @export
runner_shiny <- function(output_dir, run = TRUE, launch.browser = FALSE) {
  catalog <- runner_shiny_catalog(output_dir)
  site_choices <- sort(unique(catalog$site))
  simulation_choices <- sort(unique(catalog$simulation))
  quantity_choices <- sort(unique(catalog$quantity))
  first <- catalog[1L, , drop = FALSE]

  ui <- shiny::fluidPage(
    shiny::titlePanel("LPJ-GUESS Runner Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("output_dir", "Runner output directory",
                         value = path.expand(output_dir)),
        shiny::actionButton("load_directory", "Load directory"),
        shiny::hr(),
        shiny::selectizeInput("site", "Site (.ins basename)", site_choices,
                              selected = first$site, multiple = TRUE),
        shiny::selectizeInput("simulation", "Simulation", simulation_choices,
                              selected = first$simulation, multiple = TRUE),
        shiny::selectizeInput("quantity", "Outputs", quantity_choices,
                              selected = first$quantity, multiple = TRUE),
        shiny::uiOutput("quantity_columns"),
        shiny::hr(),
        shiny::selectInput("colour_by", "Colour", runner_shiny_style_choices(),
                           selected = "Simulation"),
        shiny::selectInput("line_by", "Line type",
                           c("None" = "none", runner_shiny_style_choices()),
                           selected = "constant"),
        shiny::selectInput("point_by", "Point type",
                           c("None" = "none", runner_shiny_style_choices()),
                           selected = "none"),
        shiny::textInput("plot_title", "Plot title (optional)",
                         value = "", placeholder = "Automatic"),
        shiny::selectInput("legend_location", "Legend location",
          c("Right" = "right", "Left" = "left", "Above" = "top",
            "Below" = "bottom"), selected = "right"),
        shiny::selectInput("legend_direction", "Legend direction",
          c("Vertical" = "vertical", "Horizontal" = "horizontal"),
          selected = "vertical"),
        shiny::helpText("Outputs share one value axis. Check units in the hover text when comparing quantities."),
        shiny::helpText("Only site and simulation pairs present in the runner catalog are plotted.")
      ),
      shiny::mainPanel(
        shiny::textOutput("selection_summary"),
        plotly::plotlyOutput("timeseries", height = "70vh")
      )
    )
  )

  server <- function(input, output, session) {
    current_catalog <- shiny::reactiveVal(catalog)
    file_cache <- new.env(parent = emptyenv())

    shiny::observeEvent(input$load_directory, {
      new_catalog <- tryCatch(runner_shiny_catalog(input$output_dir),
                              error = function(e) e)
      if (inherits(new_catalog, "error")) {
        shiny::showNotification(conditionMessage(new_catalog), type = "error",
                                duration = 10)
        return()
      }
      rm(list = ls(file_cache, all.names = TRUE), envir = file_cache)
      current_catalog(new_catalog)
      shiny::updateSelectizeInput(session, "site",
        choices = sort(unique(new_catalog$site)),
        selected = new_catalog$site[1L])
      shiny::updateSelectizeInput(session, "simulation",
        choices = sort(unique(new_catalog$simulation)),
        selected = new_catalog$simulation[1L])
      shiny::updateSelectizeInput(session, "quantity",
        choices = sort(unique(new_catalog$quantity)),
        selected = new_catalog$quantity[1L])
    })

    selected_files <- shiny::reactive({
      catalog <- current_catalog()
      catalog[catalog$site %in% input$site &
                catalog$simulation %in% input$simulation &
                catalog$quantity %in% input$quantity, , drop = FALSE]
    })

    column_id <- function(quantity) {
      paste0("columns_", paste(as.integer(charToRaw(quantity)), collapse = "_"))
    }

    output$quantity_columns <- shiny::renderUI({
      files <- selected_files()
      shiny::tagList(lapply(intersect(input$quantity, unique(files$quantity)),
        function(quantity) {
          paths <- unique(files$path[files$quantity == quantity])
          columns <- sort(unique(unlist(lapply(paths, function(path) {
            tryCatch(runner_shiny_columns(path), error = function(e) character())
          }), use.names = FALSE)))
          previous <- shiny::isolate(input[[column_id(quantity)]])
          chosen <- if (is.null(previous)) {
            if ("Total" %in% columns) "Total" else head(columns, 1L)
          } else intersect(previous, columns)
          shiny::selectizeInput(column_id(quantity),
            paste(quantity, "columns"), choices = columns,
            selected = chosen, multiple = TRUE)
        }))
    })

    timeseries_data <- shiny::reactive({
      files <- selected_files()
      shiny::validate(shiny::need(nrow(files) > 0,
        "Select a site, simulation, and output with available files."))
      records <- list()
      for (i in seq_len(nrow(files))) {
        file <- files[i, , drop = FALSE]
        columns <- input[[column_id(file$quantity)]]
        if (!length(columns)) next
        key <- file$path
        if (!exists(key, envir = file_cache, inherits = FALSE)) {
          assign(key, data.table::fread(key, check.names = FALSE,
                                        data.table = FALSE), envir = file_cache)
        }
        records[[length(records) + 1L]] <- list(
          data = get(key, envir = file_cache, inherits = FALSE),
          columns = columns, quantity = file$quantity, site = file$site,
          simulation = file$simulation, job = file$job,
          factors = file$factors[[1L]])
      }
      shiny::validate(shiny::need(length(records) > 0,
        "Select at least one numeric output column."))
      result <- runner_shiny_timeseries_data(records)
      shiny::validate(shiny::need(nrow(result) > 0,
        "No numeric values are available for the selected columns."))
      result
    })

    shiny::observeEvent(timeseries_data(), {
      choices <- runner_shiny_style_choices(timeseries_data())
      for (id in c("colour_by", "line_by", "point_by")) {
        available <- if (id == "colour_by") choices else c("None" = "none", choices)
        selected <- shiny::isolate(input[[id]])
        if (is.null(selected) || !selected %in% available) {
          selected <- if (id == "colour_by") "Simulation" else
            if (id == "line_by") "constant" else "none"
        }
        shiny::updateSelectInput(session, id, choices = available,
                                 selected = selected)
      }
    })

    output$selection_summary <- shiny::renderText({
      data <- timeseries_data()
      paste(length(unique(data$Series)), "series across",
            length(unique(data$Site)), "site(s) and",
            length(unique(data$Simulation)), "simulation(s)")
    })

    output$timeseries <- plotly::renderPlotly({
      data <- timeseries_data()
      shiny::req(input$colour_by, input$line_by, input$point_by)
      shiny::validate(shiny::need(input$line_by != "none" || input$point_by != "none",
        "Enable lines or points to draw the time series."))
      runner_shiny_timeseries_plot(data, input$colour_by, input$line_by,
                                   input$point_by, input$legend_location,
                                   input$legend_direction, input$plot_title)
    })
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  if (isTRUE(run)) {
    shiny::runApp(app, launch.browser = launch.browser)
    return(invisible(app))
  }
  app
}

runner_shiny_catalog <- function(output_dir) {
  if (!requireNamespace("lpjguessRunner", quietly = TRUE)) {
    stop("runner_shiny requires the lpjguessRunner package", call. = FALSE)
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(trimws(output_dir))) {
    stop("Provide one runner output directory", call. = FALSE)
  }
  output_dir <- path.expand(trimws(output_dir))
  if (!dir.exists(output_dir)) stop("Directory not found: ", output_dir, call. = FALSE)
  runs <- lpjguessRunner::list_simulations(output_dir)
  if (!nrow(runs)) stop("No simulations in runner index: ", output_dir, call. = FALSE)
  outputs <- lapply(seq_len(nrow(runs)), function(i) {
    files <- lpjguessRunner::list_outputs(runs[i, , drop = FALSE])
    if (!nrow(files)) return(NULL)
    files$site <- runs$instruction_file[i]
    files$job <- runs$path[i]
    files$factors <- rep(list(runner_shiny_factor_values(runs$factors[[i]])),
                         nrow(files))
    files
  })
  outputs <- Filter(Negate(is.null), outputs)
  if (!length(outputs)) stop("No .out files found for runner simulations", call. = FALSE)
  catalog <- do.call(rbind, outputs)
  catalog$quantity <- ifelse(is.na(catalog$output_type),
    sub("\\.out$", "", catalog$file),
    sub("^file_", "", catalog$output_type))
  catalog <- catalog[!(catalog$quantity %in% c("guess_out", "guess_err", "*")),
                     , drop = FALSE]
  if (!nrow(catalog)) stop("No plot-ready .out files found", call. = FALSE)
  rownames(catalog) <- NULL
  catalog
}

runner_shiny_factor_values <- function(factors) {
  if (!length(factors)) return(setNames(character(), character()))
  keys <- vapply(factors, function(factor) {
    if (identical(factor$type, "block")) {
      paste(factor$block_type, factor$block_name, factor$name, sep = "/")
    } else as.character(factor$name)
  }, character(1))
  values <- vapply(factors, function(factor) as.character(factor$value),
                   character(1))
  stats::setNames(values[!duplicated(keys, fromLast = TRUE)],
                  keys[!duplicated(keys, fromLast = TRUE)])
}

runner_shiny_columns <- function(path) {
  frame <- data.table::fread(path, nrows = 1000L, check.names = FALSE,
                             data.table = FALSE)
  index_columns <- c("Lon", "Lat", "Year", "Day", "timestep",
                     "stand", "patch", "indiv")
  names(frame)[!tolower(names(frame)) %in% tolower(index_columns) &
                 vapply(frame, is.numeric, logical(1))]
}

runner_shiny_timeseries_data <- function(records) {
  result <- list()
  factor_names <- unique(unlist(lapply(records, function(record) {
    names(record$factors)
  }), use.names = FALSE))
  for (record in records) {
    frame <- as.data.frame(record$data)
    for (canonical in c("Lon", "Lat", "Year", "Day", "timestep",
                        "stand", "patch", "indiv")) {
      index <- which(tolower(names(frame)) == tolower(canonical))
      if (length(index)) names(frame)[index[1L]] <- canonical
    }
    if (!"Year" %in% names(frame)) {
      stop("Output ", record$quantity, " has no Year column", call. = FALSE)
    }
    entities <- intersect(c("stand", "patch", "indiv"), names(frame))
    if ("indiv" %in% entities && !all(c("stand", "patch") %in% entities) ||
        "patch" %in% entities && !"stand" %in% entities) {
      stop("Missing entity columns in ", record$quantity, call. = FALSE)
    }
    # Use one time class across files so combining annual and subdaily output
    # cannot truncate a subdaily timestamp to a Date.
    frame$Date <- as.POSIXct(calc_date(frame), tz = "UTC")
    if (anyNA(frame$Date)) stop("Invalid dates in ", record$quantity, call. = FALSE)
    gridcell <- if (all(c("Lon", "Lat") %in% names(frame))) {
      paste(frame$Lon, frame$Lat, sep = ", ")
    } else rep("", nrow(frame))
    hierarchy <- lapply(seq_len(3L), function(level) {
      keys <- c("stand", "patch", "indiv")[seq_len(level)]
      if (!all(keys %in% entities)) return(rep(NA_character_, nrow(frame)))
      do.call(paste, c(lapply(keys, function(key) paste(key, frame[[key]])),
                        sep = ", "))
    })
    entity <- if (length(entities)) hierarchy[[length(entities)]] else
      rep("Gridcell", nrow(frame))
    keys <- data.frame(Date = frame$Date, Gridcell = gridcell,
                       Entity = entity, stringsAsFactors = FALSE)
    if (anyDuplicated(keys)) {
      stop("Duplicate date/entity keys in ", record$quantity, " (",
           record$site, "/", record$simulation, ")", call. = FALSE)
    }
    columns <- intersect(record$columns, names(frame))
    columns <- columns[vapply(frame[columns], is.numeric, logical(1))]
    for (column in columns) {
      part <- data.frame(Date = frame$Date, Value = frame[[column]],
        Quantity = record$quantity, Layer = column, Site = record$site,
        Simulation = record$simulation,
        Source = paste(record$site, record$simulation, sep = " / "),
        Gridcell = gridcell, Entity = entity,
        Stand = hierarchy[[1L]], Patch = hierarchy[[2L]], Cohort = hierarchy[[3L]],
        Job = record$job, stringsAsFactors = FALSE)
      for (factor in factor_names) {
        value <- record$factors[[factor]]
        part[[paste0("Factor:", factor)]] <- if (is.null(value)) {
          rep(NA_character_, nrow(part))
        } else rep(value, nrow(part))
      }
      result[[length(result) + 1L]] <- part
    }
  }
  if (!length(result)) return(data.frame())
  data <- do.call(rbind, result)
  series_key <- paste(data$Job, data$Quantity, data$Layer,
                      data$Gridcell, data$Entity, sep = "\r")
  data$Series <- match(series_key, unique(series_key))
  data[order(data$Series, data$Date), , drop = FALSE]
}

runner_shiny_style_choices <- function(data = NULL) {
  choices <- c("Constant" = "constant", "Vary by column" = "Layer",
    "Vary by quantity" = "Quantity", "Vary by site" = "Site",
    "Vary by simulation" = "Simulation", "Vary by source" = "Source")
  if (!is.null(data)) {
    factors <- grep("^Factor:", names(data), value = TRUE)
    if (length(factors)) {
      choices <- c(choices, stats::setNames(factors,
        paste("Vary by factor", sub("^Factor:", "", factors))))
    }
    entities <- c("Vary by stand" = "Stand", "Vary by patch" = "Patch",
                  "Vary by cohort" = "Cohort", "Vary by gridcell" = "Gridcell")
    entities <- entities[vapply(entities, function(column) {
      any(!is.na(data[[column]]) & nzchar(as.character(data[[column]])))
    }, logical(1))]
    choices <- c(choices, entities)
  }
  choices
}

runner_shiny_timeseries_plot <- function(data, colour_by, line_by, point_by,
                                         legend_location = "right",
                                         legend_direction = "vertical",
                                         title = NULL) {
  mapping <- function(by, values) {
    if (by %in% c("none", "constant")) return(rep(values[1L], nrow(data)))
    levels <- unique(data[[by]])
    rep(values, length.out = length(levels))[match(data[[by]], levels)]
  }
  ncolours <- if (colour_by == "constant") 1L else length(unique(data[[colour_by]]))
  colours <- mapping(colour_by, grDevices::hcl.colors(max(1L, ncolours), "Dark 3"))
  lines <- mapping(line_by, c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot"))
  points <- mapping(point_by, c("circle", "square", "diamond", "cross", "x",
                               "triangle-up", "triangle-down", "star"))
  mode <- if (line_by == "none") "markers" else
    if (point_by == "none") "lines" else "lines+markers"
  plot <- plotly::plot_ly()
  labels <- timeseries_trace_labels(data,
    c("Site", "Simulation", "Quantity", "Layer", "Entity", "Gridcell"),
    gridcell_by_job = TRUE)
  for (id in unique(data$Series)) {
    trace <- data[data$Series == id, , drop = FALSE]
    first <- which(data$Series == id)[1L]
    label <- labels$Name[match(id, labels$Series)]
    full_label <- labels$Full[match(id, labels$Series)]
    plot <- plotly::add_trace(plot, x = trace$Date, y = trace$Value,
      type = "scatter", mode = mode, name = label,
      showlegend = nrow(labels) > 1L,
      text = paste(full_label, get_y_label(trace$Quantity[1L]), sep = "<br>"),
      hovertemplate = "%{text}<br>%{x}<br>%{y}<extra></extra>",
      line = if (line_by != "none") list(color = colours[first], dash = lines[first]) else NULL,
      marker = if (point_by != "none") list(color = colours[first], symbol = points[first]) else NULL,
      connectgaps = FALSE)
  }
  plotly::layout(plot, title = timeseries_plot_title(data, title = title),
    xaxis = list(title = "Date"),
    yaxis = list(title = timeseries_y_title(data)),
    legend = timeseries_legend_layout(legend_location, legend_direction))
}
