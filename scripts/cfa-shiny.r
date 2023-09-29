library("leaflet")
library("raster")
library("shiny")
library("shinyjs")
library("tools")
library("plotly")

#' Read an LPJ-Guess output file and return file data as a dataframe.
#' @param filename: Path to the output file, may be relative or absolute.
read_lpj_guess_output_file <- function(filename) {
    return(read.table(filename, header = TRUE))
}

output_data_to_raster <- function(data) {
    # Generate a raster for this data.
    rs <- rasterFromXYZ(data)

    # Change the coordinate system of the raster (this is requird by the
    # leaflet library).
    crs(rs) <- "+proj=longlat +datum=WGS84"

    return(rs)
}

get_units <- function(var, layer) {
    var <- tolower(var)
    if (var == "lai") {
        return("m2/m2")
    } else if (var == "aaet") {
        return("mm")
    } else if (var == "agpp") {
        return("kgC/m2/year")
    } else if (var == "annual_burned_area") {
        if (layer == "BurntFr") {
            return("0-1")
        }
    } else if (var == "anpp") {
        return("kgC/m2/year")
    } else if (var == "cflux") {
        return("gC/m2/year")
    } else if (var == "clitter") {
        return("kgC/m2/year")
    }
    else if (var == "cmass") {
        return("kgC/m2")
    }
    else if (var == "cpool") {
        return("kgC/m2")
    }
    else if (var == "dens") {
        return("/m2")
    }
    else if (var == "fpc") {
        return("0-1")
    }
    else if (var == "height") {
        return("m")
    }
    else if (var == "mgpp") {
        return("kgC/m2/month")
    }
    else if (var == "mlai") {
        return("m2/m2")
    }
    else if (var == "mwcont_lower") {
        return("0-1")
    }
    else if (var == "mwcont_upper") {
        return("0-1")
    }
    else if (var == "nflux") {
        return("kgN/m2/year")
    }
    else if (var == "nlitter") {
        return("kgN/m2/year")
    }
    else if (var == "npool") {
        return("kgN/m2")
    }
    else if (var == "nuptake") {

    }
    else if (var == "simfireanalysis") {
     
    }
    else if (var == "tot_runoff") {
        return("mm/year")
    }
    return(NULL)
}

get_title <- function(var, year, layer) {
    title <- var
    units <- get_units(var, layer)
    if (!is.null(units)) {
        title <- paste0(title, " (", units, ")")
    }
    title <- paste(title, year)
    return(title)
}

cfa_shiny <- function(out_path) {
    if (!dir.exists(out_path)) {
        stop("Directory not found: '", out_path, "'")
    }

    vars <- list.files(out_path)
    var_names <- lapply(vars, file_path_sans_ext)

    cache <- list()

    get_file_path <- function(var_name) {
        return(file.path(out_path, paste0(var_name, ".out")))
    }

    # Function to read data for the specified variable, either from the cache
    # (if it contains this variable), or from disk (saving to cache).
    get_data <- function(var_name) {
        if (var_name %in% names(cache)) {
            cat(paste0("Reading ", var_name, " from cache...\n"))
            return(cache[[var_name]])
        }
        cat(paste0("Reading ", var_name, " from disk...\n"))
        file_path <- get_file_path(var_name)
        data <- read_lpj_guess_output_file(file_path)
        cache[[var_name]] <<- data
        return(data)
    }

    # Define the GUI for the app.
    ui <- fluidPage(
        # App title.
        titlePanel("CFA Shiny App Demo"),
        useShinyjs(),

        # Sidebar layout with input and output definitions.
        sidebarLayout(

            # Sidebar panel for inputs.
            sidebarPanel(
                # Custom CSS to show a "loading" banner when data is loading.
                tags$head(
                    tags$style(HTML(".multicol {-webkit-column-count: 3; /* Chrome, Safari, Opera */-moz-column-count: 3; /* Firefox */column-count: 3;}")),
                    tags$style(type="text/css", "#loadmessage {position: fixed;top: 0px;left: 0px;width: 100%;padding: 5px 0px 5px 0px;text-align: center;font-weight: bold;font-size: 100%;color: #000000;background-color: #CCFF66;z-index: 105;}"),
                    tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }")
                ),
                conditionalPanel(
                    condition = "$('html').hasClass('shiny-busy')",
                    tags$div("Loading...", id = "loadmessage")
                ),

                # The input controls.
                selectInput(inputId = "var",
                            label = "Variable",
                            var_names,
                            selected = var_names[1]),
                uiOutput("layer"),
                uiOutput("year"),
            ),

            # Main panel for displaying outputs.
            mainPanel(
                leafletOutput(outputId = "spatial"),
                plotlyOutput(outputId = "temporal")
            )
        )
    )

    # Persistent state
    prev_var <- NULL

    # Define server logic required to draw a plot ----
    server <- function(input, output) {
        # Spatial plot of the requested variable.
        output$spatial <- renderLeaflet({
            var <- input$var
            layer <- input$layer
            year <- input$year

            # Update layer/year inputs if the user has changed the variable.
            if (is.null(prev_var) || var != prev_var) {
                output$temporal <- NULL
                data <- get_data(var)
                if (!("Year" %in% colnames(data))) {
                    stop("Variable '", var, "' does not have a Year column")
                }
                ignored <- c("Lon", "Lat", "Year", "Day", "patch")
                layers <- setdiff(colnames(data), ignored)
                years <- unique(data$Year)

                # The layer to select by default (if present - if not, auto-
                # select the first layer in the file. The user will just have to
                # manually change it in this case).
                default_layer <- "Total"
                layer <- ifelse(
                    default_layer %in% layers,
                    default_layer,
                    layers[1]
                )
                year <- ifelse(
                    is.null(year),
                    years[1],
                    year
                )
                output$layer <- renderUI({
                    selectInput("layer",
                                "Column",
                                layers,
                                selected = layer
                    )
                })
                output$year <- renderUI({
                    selectInput("year",
                                "Year",
                                years,
                                selected = year)
                })
                prev_var <<- var
            }

            # Read data (from cache or from disk).
            data <- get_data(var)

            # Create raster for this year's data.
            # Filter data to the specified year and PFT.
            filtered <- data[data$Year == year, c("Lon", "Lat", layer)]

            # Determine axis limits.
            ymin <- min(filtered[[layer]])
            ymax <- max(filtered[[layer]])

            # Convert data frame to raster.
            rs <- output_data_to_raster(filtered)

            # Create title and colour palette for the plot.
            title <- get_title(var, year, layer)
            colours <- rev(terrain.colors(255))
            pal <- colorNumeric(colours, values(rs), na.color = "transparent")
            labels <- seq(ymin, ymax, length.out = length(colours))

            # Finally, draw the spatial plot.
            leaflet() %>%
                addTiles() %>%
                addRasterImage(
                    rs,
                    colors = pal,
                    opacity = 1) %>%
                addLegend(
                    pal = pal,
                    values = labels,
                    title = title)
        })

        observeEvent(input$spatial_click, {
            # Get the coordinates of the point clicked by the user.
            lat <- input$spatial_click$lat
            lon <- input$spatial_click$lng

            # Get the other user inputs.
            var <- input$var
            layer <- input$layer

            # If no variable or layer is selected, do nothing. I don't think
            # this can happen here, but it's good to be prepared.
            if (is.null(var) || is.null(layer)) {
                output$temporal <- NULL
                return(NULL)
            }

            # Read data (from cache or from disk).
            data <- get_data(var)

            # This function will check if two numbers are equal or close to each
            # other (for some definition of "close").
            equal <- function(x, y) {
                eps <- 1e-2
                return(abs(x - y) < eps)
            }

            # Get the closest grid point to the point clicked on by the user.
            lats <- unique(data$Lat)
            lons <- unique(data$Lon)
            diff_lat <- abs(lats - lat)
            diff_lon <- abs(lons - lon)
            lat <- lats[which.min(diff_lat)]
            lon <- lons[which.min(diff_lon)]

            # Filter data to this grid point.
            data <- data[equal(data$Lat, lat) & equal(data$Lon, lon), ]

            # Remove missing values.
            data <- data[!is.null(data$Year), ]
            data <- data[!is.na(data$Year), ]
            data <- data[!is.null(data[[layer]]), ]
            data <- data[!is.na(data[[layer]]), ]

            # Hide the timeseries plot if no data is found for this grid point.
            if (nrow(data) == 0) {
                output$temporal <- NULL
                return(NULL)
            }

            # Create a title for the timeseries plot.
            ti <- paste0(layer, " ", var, " timeseries (", lon, ", ", lat, ")")

            # Draw the timeseries plot using plotly.
            output$temporal <- renderPlotly({
                plot_ly() %>%
                    add_lines(x = data$Year, y = data[[layer]]) %>%
                    layout(
                        title = ti,
                        xaxis = list(title = "Date"),
                        yaxis = list(title = var)
                    )
            })
        })
    }

    app <- shinyApp(ui = ui, server = server)
    runApp(app, launch.browser = FALSE)
}
