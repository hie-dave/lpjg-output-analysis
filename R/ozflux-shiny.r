#'
#' Run a shiny app which generates interactive plots of site-level ozflux
#' simulations against predictions.
#'
#' @export
#'
ozflux_shiny <- function(sources) {
  sources <- sanitise_sources(sources)

  gridcells <- read_ozflux_sites()

  vars <- get_observed_vars()

  var_names <- c()
  for (var in vars) {
    var_names <- c(var_names, var@name)
  }

  cached_vars <- c()
  cached_data <- c()

  # Function to get the variable with the specified name.
  get_var <- function(name) {
    for (var in vars) {
      if (var@name == name) {
        return(var)
      }
    }
    stop("Unknown variable: ", name)
  }

  # Function to read data for the specified variable, either from the cache (if
  # it contains this variable), or from disk (saving to cache).
  get_data <- function(var_name) {
    if (var_name %in% cached_vars) {
      cat(paste0("Reading ", var_name, " from cache...\n"))
      data <- cached_data[which(cached_vars == var_name)][[1]]
    } else {
      cat(paste0("Reading ", var_name, " from disk...\n"))
      var <- get_var(var_name)
      data <- read_data(sources, var)
      cached_vars <<- c(cached_vars, var_name)
      cached_data <<- c(cached_data, data)
    }
    return(data)
  }

  # Define the GUI for the app.
  ui <- fluidPage(
    # App title.
    titlePanel("Ozflux Benchmarks"),
    useShinyjs(),

    # Sidebar layout with input and output definitions.
    sidebarLayout(

      # Sidebar panel for inputs.
      sidebarPanel(
        # Custom CSS to show a "loading" banner when data is being processed.
        tags$head(
          tags$style(HTML(".multicol {-webkit-column-count: 3; /* Chrome, Safari, Opera */-moz-column-count: 3; /* Firefox */column-count: 3;}")),
          tags$style(type="text/css", "#loadmessage {position: fixed;top: 0px;left: 0px;width: 100%;padding: 5px 0px 5px 0px;text-align: center;font-weight: bold;font-size: 100%;color: #000000;background-color: #CCFF66;z-index: 105;}"),
          tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }")
        ),
        conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Loading...",id="loadmessage")),

        # The input controls.
        selectInput(inputId = "site",
                    label = "Site",
                    gridcells$Name,
                    selected = gridcells$Name[1]),
        selectInput(inputId = "var",
                    label = "Variable",
                    var_names,
                    selected = var_names[1]),
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
        plotlyOutput(outputId = "timeseries"),
        plotlyOutput(outputId = "pvo"),
        plotlyOutput(outputId = "subannual")
      )
    )
  )
  graphs <- NULL
  # Define server logic required to draw a plot ----
  server <- function(input, output) {
    # Plotly graph of the requested grid cell.

    # This expression that generates a plotly object is wrapped in a call
    # to renderPlotly to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plotly object
    output$timeseries <- renderPlotly({
      # Parse inputs.
      site <- input$site
      var_name <- input$var
      ncol <- 1#input$ncol
      index <- which(gridcells$Name == site)
      lat <- gridcells$Lat[index]
      lon <- gridcells$Lon[index]

      # Read data (from cache or from disk).
      data <- get_data(var_name)

      # Filter to the selected grid point.
      gridcell <- get_gridcell(data, lat, lon, site)

      graphs <<- create_plots(gridcell, var_name, ncol = ncol,
        do_timeseries = input$do_timeseries, do_pvo = input$do_pvo,
        do_subannual = input$do_subannual)

      title <- paste(site, var_name)
      if (input$do_timeseries) {
        graphs$timeseries <- graphs$timeseries %>% layout(title = title)
        return(graphs$timeseries)
      } else if (input$do_pvo) {
        graphs$pvo <- graphs$pvo %>% layout(title = title)
      } else if (input$do_subannual) {
        graphs$subannual <- graphs$subannual %>% layout(title = title)
      }
      return(NULL)
    })
    output$pvo <- renderPlotly({
      site <- input$site
      var_name <- input$var
      ncol <- 1#input$ncol
      if (input$do_pvo) {
        return(graphs$pvo)
      }
      return(NULL)
    })
    output$subannual <- renderPlotly({
      site <- input$site
      var_name <- input$var
      ncol <- 1#input$ncol
      if (input$do_subannual) {
        return(graphs$subannual)
      }
      return(NULL)
    })
    observeEvent(c(input$do_timeseries, input$do_pvo, input$do_subannual), {
      if (input$do_timeseries) {
        show("timeseries")
      } else {
        hide("timeseries")
      }
      if (input$do_pvo) {
        show("pvo")
      } else {
        hide("pvo")
      }
      if (input$do_subannual) {
        show("subannual")
      } else {
        hide("subannual")
      }
    })
  }

  app <- shinyApp(ui = ui, server = server)
  runApp(app, launch.browser = FALSE)
}
