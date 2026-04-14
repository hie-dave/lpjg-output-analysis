#'
#' Run a shiny app which generates interactive plots of site-level ozflux
#' simulations against predictions.
#'
#' @param sources List of [DGVMTools::Source] objects to plot
#'
#' @return Returns a [shiny::shinyApp] object
#' @import shiny
#' @export
#'
ozflux_shiny <- function(sources) {
  sources <- sanitise_sources(sources)

  gridcells <- read_ozflux_sites()

  get_site_vars <- function(site_name) {
    out_vars <- c()
    ignored <- c("*", "guess_out", "guess_err")

    for (source in sources) {
      if (source@format@id != "OZFLUX") {
        next
      }

      ozflux_dir <- get_ozflux_path(source)
      out_dir_name <- get_output_dir(file.path(ozflux_dir, "outputs.ins"))
      site_out_dir <- file.path(ozflux_dir, site_name, out_dir_name)
      if (!dir.exists(site_out_dir)) {
        next
      }

      files <- c(
        list.files(site_out_dir, pattern = "\\.out$", full.names = FALSE),
        list.files(site_out_dir, pattern = "\\.out\\.gz$", full.names = FALSE)
      )
      vars <- unlist(lapply(files, quant_from_file_name))
      vars <- vars[!is.na(vars) & vars != "" & !(vars %in% ignored)]
      out_vars <- c(out_vars, vars)
    }

    unique(sort(out_vars))
  }

  make_var_choices <- function(var_ids) {
    labels <- paste0(unlist(lapply(var_ids, readable_name)), " (", var_ids, ")")
    setNames(var_ids, labels)
  }

  get_site_layers <- function(site_name, var_name) {
    var <- sanitise_variable(var_name)
    out_layers <- c()
    for (source in sources) {
      if (source@format@id != "OZFLUX") {
        next
      }
      layers <- tryCatch(
        available_layers_ozflux(source, var, sites = site_name),
        error = function(e) c()
      )
      out_layers <- c(out_layers, layers)
    }
    unique(sort(out_layers))
  }

  get_observation_layer_choices <- function(var_name) {
    readers <- find_readers_for_var(trim_dave(var_name))
    if (length(readers) == 0) {
      return(c())
    }
    names(readers)
  }

  first_site <- gridcells$Name[1]
  initial_vars <- get_site_vars(first_site)
  initial_choices <- make_var_choices(initial_vars)
  initial_var <- if (length(initial_vars) > 0) initial_vars[1] else NULL
  initial_layers <- if (!is.null(initial_var)) get_site_layers(first_site, initial_var) else c()
  initial_obs_layers <- if (!is.null(initial_var)) get_observation_layer_choices(initial_var) else c()

  cached_vars <- c()
  cached_data <- c()

  # Function to get the variable with the specified name.
  get_var <- function(name) sanitise_variable(name)

  # Function to read data for the specified variable/site pair, either from
  # the cache (if it contains this key), or from disk (saving to cache).
  get_data <- function(var_name, site_name, layers) {
    layer_key <- if (is.null(layers) || length(layers) < 1) "<default>" else paste(sort(layers), collapse = ",")
    cache_key <- paste(var_name, site_name, layer_key, sep = "::")
    if (cache_key %in% cached_vars) {
      cat(paste0("Reading ", cache_key, " from cache...\n"))
      data <- cached_data[which(cached_vars == cache_key)][[1]]
    } else {
      cat(paste0("Reading ", cache_key, " from disk...\n"))
      var <- get_var(var_name)
      data <- read_data(sources, var, sites = site_name, layers = layers)
      cached_vars <<- c(cached_vars, cache_key)
      cached_data <<- c(cached_data, data)
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
          tags$style(type="text/css", "#loadmessage {position: fixed;top: 0px;left: 0px;width: 100%;padding: 5px 0px 5px 0px;text-align: center;font-weight: bold;font-size: 100%;color: #000000;background-color: #CCFF66;z-index: 105;}")
        ),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")),

        # The input controls.
        selectInput(inputId = "site",
                    label = "Site",
                    gridcells$Name,
                    selected = gridcells$Name[1]),
        selectInput(inputId = "var",
                    label = "Variable",
                    choices = initial_choices,
                    selected = initial_var),
        selectizeInput(inputId = "layers",
                 label = "Layers",
                 choices = initial_layers,
                 selected = initial_layers,
                 multiple = TRUE),
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
      ),

      # Main panel for displaying outputs.
      mainPanel(
        plotly::plotlyOutput(outputId = "timeseries"),
        plotly::plotlyOutput(outputId = "pvo"),
        plotly::plotlyOutput(outputId = "subannual")
      )
    )
  )

  # Define server logic required to draw a plot ----
  server <- function(input, output, session) {
    observeEvent(input$site, {
      vars <- get_site_vars(input$site)
      choices <- make_var_choices(vars)
      selected <- if (input$var %in% vars) input$var else if (length(vars) > 0) vars[1] else NULL
      updateSelectInput(session, "var", choices = choices, selected = selected)
    }, ignoreInit = TRUE)

    observeEvent(c(input$site, input$var), {
      req(input$site)
      req(input$var)

      available_vars <- get_site_vars(input$site)
      if (!(input$var %in% available_vars)) {
        return()
      }

      layers <- get_site_layers(input$site, input$var)
      selected_layers <- if (length(input$layers) > 0) {
        intersect(input$layers, layers)
      } else {
        c()
      }
      if (length(selected_layers) < 1) {
        selected_layers <- layers
      }
      updateSelectizeInput(session, "layers", choices = layers,
                           selected = selected_layers, server = TRUE)

      obs_layers <- get_observation_layer_choices(input$var)
      obs_choices <- c("Auto-detect" = "", obs_layers)
      selected_obs <- if (!is.null(input$obs_layer) && input$obs_layer %in% obs_layers) {
        input$obs_layer
      } else {
        ""
      }
      updateSelectInput(session, "obs_layer", choices = obs_choices,
                        selected = selected_obs)
    }, ignoreInit = FALSE)

    plots <- reactive({
      req(input$site)
      req(input$var)
      req(input$layers)
      validate(need(length(input$layers) > 0,
                    "Please select at least one layer to plot."))

      # Parse inputs.
      site <- input$site
      var_name <- input$var
      selected_layers <- input$layers

      available_vars <- get_site_vars(site)
      validate(need(var_name %in% available_vars,
                    "Selected variable is not available for this site."))

      ncol <- 1#input$ncol
      index <- which(gridcells$Name == site)
      lat <- gridcells$Lat[index]
      lon <- gridcells$Lon[index]

      # Read data (from cache or from disk).
      data <- get_data(var_name, site, selected_layers)

      # Filter to the selected grid point.
      gridcell <- get_gridcell(data, lat, lon, site)

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

      do_pvo <- isTRUE(input$do_pvo) && !is.null(obs_layer)

      graphs <- create_plots(gridcell, var_name, ncol = ncol,
        do_timeseries = input$do_timeseries, do_pvo = do_pvo,
        do_subannual = input$do_subannual, obs_lyr = obs_layer)

      title <- paste(site, var_name)
      if (input$do_timeseries) {
        graphs$timeseries <- plotly::layout(graphs$timeseries, title = title)
      }
      if (!is.null(graphs$pvo)) {
        graphs$pvo <- plotly::layout(graphs$pvo, title = title)
      }
      if (input$do_subannual) {
        graphs$subannual <- plotly::layout(graphs$subannual, title = title)
      }

      graphs
    })

    output$timeseries <- plotly::renderPlotly({
      if (!input$do_timeseries) {
        return(NULL)
      }
      plots()$timeseries
    })

    output$pvo <- plotly::renderPlotly({
      if (!input$do_pvo) {
        return(NULL)
      }
      p <- plots()$pvo
      validate(need(!is.null(p),
                    "No observed layer is available for this variable/site; predicted vs. observed is disabled."))
      p
    })

    output$subannual <- plotly::renderPlotly({
      if (input$do_subannual) {
        return(plots()$subannual)
      }
      return(NULL)
    })

    observeEvent(c(input$do_timeseries, input$do_pvo, input$do_subannual), {
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
  }

  app <- shinyApp(ui = ui, server = server)
  runApp(app, launch.browser = FALSE)
}
