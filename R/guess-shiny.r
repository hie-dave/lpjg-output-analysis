#'
#' Run a Shiny app for exploring gridded LPJ-GUESS outputs.
#'
#' The app loads a directory as a `DGVMTools::GUESS` source, allows selecting
#' quantity/layer/timestep, plots the selected timestep spatially, and shows a
#' clicked-pixel time series.
#'
#' @param default_dir Default input directory to prefill in the app.
#' @param run If `TRUE` (default), launch the app immediately.
#' @param launch.browser Passed to [shiny::runApp()] when `run = TRUE`.
#'
#' @return A [shiny::shinyApp] object (invisibly if `run = TRUE`).
#' @import shiny
#' @export
guess_shiny <- function(
    default_dir = "~/code/lpj-guess/.data/runs/barra2/1deg/grass_tree",
    run = TRUE,
    launch.browser = FALSE) {

    ui <- shiny::fluidPage(
        shiny::titlePanel("LPJ-GUESS Spatial Explorer"),
        shinyjs::useShinyjs(),
        shiny::tags$script(shiny::HTML("
            (function() {
              function reportDarkMode() {
                var dark = false;
                if (window.matchMedia) {
                  dark = window.matchMedia('(prefers-color-scheme: dark)').matches;
                }
                if (window.Shiny && Shiny.setInputValue) {
                  Shiny.setInputValue('is_dark_mode', dark ? 'dark' : 'light', {priority: 'event'});
                }
              }
              document.addEventListener('shiny:connected', reportDarkMode);
              window.addEventListener('load', reportDarkMode);
              if (window.matchMedia) {
                var mq = window.matchMedia('(prefers-color-scheme: dark)');
                if (mq.addEventListener) {
                  mq.addEventListener('change', reportDarkMode);
                } else if (mq.addListener) {
                  mq.addListener(reportDarkMode);
                }
              }
            })();
        ")),
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                shiny::textInput(
                    inputId = "data_dir",
                    label = "Directory",
                    value = default_dir
                ),
                shiny::actionButton("load_source", "Load Source"),
                shiny::hr(),
                shiny::selectInput("quantity", "Quantity", choices = character(0)),
                shiny::selectInput("layer", "Layer", choices = character(0)),
                shiny::actionButton("toggle_anim", "Play"),
                shiny::numericInput("anim_interval", "Animation Speed (ms/frame)", value = 300, min = 50, max = 5000, step = 50),
                shiny::sliderInput("year_anim", "Year", min = 0, max = 1, value = 0, step = 1),
                shiny::tags$div(
                    id = "day_container",
                    shiny::selectInput("day", "Day", choices = character(0))
                ),
                shiny::hr(),
                shiny::verbatimTextOutput("selected_cell")
            ),
            shiny::mainPanel(
                shiny::plotOutput("spatial_plot", height = "520px", click = "spatial_click"),
                plotly::plotlyOutput("timeseries_plot", height = "320px")
            )
        )
    )

    server <- function(input, output, session) {
        rv <- shiny::reactiveValues(
            src = NULL,
            src_dir = NULL,
            quantities = character(0),
            quantity_files = character(0),
            years = integer(0),
            animating = FALSE,
            data_cache = list(),
            selected_cell = NULL
        )

        ignored_cols <- c("Lon", "Lat", "Year", "Day", "patch", "stand")
        ignore_quants <- c("*", "guess_out", "guess_err")

        to_title_case_name <- function(dt, canonical) {
            idx <- which(tolower(names(dt)) == tolower(canonical))
            if (length(idx) > 0) {
                names(dt)[idx[1]] <- canonical
            }
            dt
        }

        list_guess_outputs <- function(dir) {
            files <- list.files(dir, pattern = "\\.out(\\.gz)?$", full.names = TRUE)
            if (length(files) < 1) {
                return(character(0))
            }
            quants <- sub("\\.out(\\.gz)?$", "", basename(files))
            keep <- !(quants %in% ignore_quants)
            files <- files[keep]
            quants <- quants[keep]
            # Prefer .out over .out.gz when both exist for same quantity.
            is_gz <- grepl("\\.out\\.gz$", files)
            order_idx <- order(quants, is_gz)
            files <- files[order_idx]
            quants <- quants[order_idx]
            dedup <- !duplicated(quants)
            out <- files[dedup]
            names(out) <- quants[dedup]
            out
        }

        get_quantity_data <- function(quant) {
            if (!quant %in% names(rv$data_cache)) {
                file_path <- rv$quantity_files[[quant]]
                dt <- utils::read.table(file_path, header = TRUE, check.names = FALSE)
                dt <- as.data.frame(dt)
                dt <- to_title_case_name(dt, "Lon")
                dt <- to_title_case_name(dt, "Lat")
                dt <- to_title_case_name(dt, "Year")
                dt <- to_title_case_name(dt, "Day")
                required <- c("Lon", "Lat", "Year")
                if (!all(required %in% names(dt))) {
                    stop(
                        "Output file '", basename(file_path),
                        "' is missing one or more required columns: ",
                        paste(required, collapse = ", ")
                    )
                }
                rv$data_cache[[quant]] <- dt
            }
            rv$data_cache[[quant]]
        }

        get_layers <- function(dt) {
            setdiff(colnames(dt), ignored_cols)
        }

        get_text_color <- function() {
            dark_raw <- input$is_dark_mode
            dark <- FALSE
            if (is.logical(dark_raw)) {
                dark <- isTRUE(dark_raw)
            } else if (is.numeric(dark_raw)) {
                dark <- !is.na(dark_raw) && dark_raw != 0
            } else if (is.character(dark_raw)) {
                dark <- tolower(trimws(dark_raw)) %in% c("true", "1", "dark")
            }
            if (dark) "white" else "black"
        }

        session$onFlushed(function() {
            shinyjs::runjs("
                (function() {
                  function sendDarkMode() {
                    var dark = false;
                    if (window.matchMedia) {
                      dark = window.matchMedia('(prefers-color-scheme: dark)').matches;
                    }
                    if (window.Shiny && Shiny.setInputValue) {
                      Shiny.setInputValue('is_dark_mode', dark ? 'dark' : 'light', {priority: 'event'});
                    }
                  }
                  sendDarkMode();
                  if (!window.__guessDarkModeListenerInstalled && window.matchMedia) {
                    var mq = window.matchMedia('(prefers-color-scheme: dark)');
                    var onChange = function() { sendDarkMode(); };
                    if (mq.addEventListener) {
                      mq.addEventListener('change', onChange);
                    } else if (mq.addListener) {
                      mq.addListener(onChange);
                    }
                    window.__guessDarkModeListenerInstalled = true;
                  }
                })();
            ")
        }, once = TRUE)

        has_variable_day <- function(dt) {
            if (!"Day" %in% names(dt)) {
                return(FALSE)
            }
            days <- unique(dt$Day)
            days <- days[!is.na(days)]
            length(days) > 1
        }

        load_source <- function(dir) {
            src_id <- basename(normalizePath(dir, winslash = "/", mustWork = FALSE))
            rv$src <- DGVMTools::defineSource(
                id = src_id,
                name = src_id,
                dir = dir,
                format = DGVMTools::GUESS
            )
            rv$src_dir <- dir
            rv$quantity_files <- list_guess_outputs(dir)
            rv$quantities <- names(rv$quantity_files)
            rv$data_cache <- list()
            rv$years <- integer(0)
            rv$selected_cell <- NULL
            rv$animating <- FALSE
            shiny::updateActionButton(session, "toggle_anim", label = "Play")
            if (length(rv$quantities) < 1) {
                stop("No .out or .out.gz files found in directory: ", dir)
            }
            shiny::updateSelectInput(
                session,
                "quantity",
                choices = rv$quantities,
                selected = if (length(rv$quantities) > 0) rv$quantities[1] else character(0)
            )
        }

        shiny::observeEvent(input$load_source, {
            dir <- path.expand(trimws(input$data_dir))
            if (!dir.exists(dir)) {
                shiny::showNotification(
                    paste0("Directory not found: ", dir),
                    type = "error",
                    duration = 7
                )
                return()
            }

            tryCatch(
                {
                    load_source(dir)
                    shiny::showNotification("Source loaded", type = "message", duration = 3)
                },
                error = function(e) {
                    shiny::showNotification(
                        paste0("Failed to load source: ", e$message),
                        type = "error",
                        duration = 10
                    )
                }
            )
        }, ignoreInit = TRUE)

        # Autoload default source on startup.
        shiny::observeEvent(TRUE, {
            if (is.null(rv$src) && nzchar(input$data_dir)) {
                dir <- path.expand(trimws(input$data_dir))
                if (dir.exists(dir)) {
                    try(load_source(dir), silent = TRUE)
                }
            }
        }, once = TRUE)

        quantity_data <- shiny::reactive({
            req(rv$src, input$quantity, input$quantity %in% rv$quantities)
            get_quantity_data(input$quantity)
        })

        selected_year <- shiny::reactive({
            req(length(rv$years) > 0, !is.null(input$year_anim), is.finite(input$year_anim))
            year <- as.integer(round(input$year_anim))
            year <- max(min(rv$years), min(max(rv$years), year))
            year
        })

        shiny::observeEvent(input$quantity, {
            req(rv$src, input$quantity, input$quantity %in% rv$quantities)
            dt <- quantity_data()
            layers <- get_layers(dt)
            years <- sort(unique(dt$Year))
            req(length(layers) > 0, length(years) > 0)
            rv$years <- as.integer(years)

            default_layer <- if ("Total" %in% layers) "Total" else layers[1]
            shiny::updateSelectInput(
                session,
                "layer",
                choices = layers,
                selected = default_layer
            )
            shiny::updateSliderInput(
                session,
                "year_anim",
                min = min(rv$years),
                max = max(rv$years),
                value = rv$years[1],
                step = 1
            )
        }, ignoreInit = TRUE)

        shiny::observeEvent(input$toggle_anim, {
            rv$animating <- !isTRUE(rv$animating)
            shiny::updateActionButton(
                session,
                "toggle_anim",
                label = if (rv$animating) "Pause" else "Play"
            )
        })

        shiny::observe({
            req(isTRUE(rv$animating), length(rv$years) > 0)
            interval <- if (is.null(input$anim_interval) || !is.finite(input$anim_interval)) 300L else as.integer(input$anim_interval)
            interval <- max(50L, min(5000L, interval))
            shiny::invalidateLater(interval, session)

            current <- isolate(input$year_anim)
            if (is.null(current) || !is.finite(current)) {
                next_year <- rv$years[1]
            } else {
                current <- as.integer(round(current))
                idx <- match(current, rv$years)
                if (is.na(idx)) {
                    idx <- which.min(abs(rv$years - current))
                }
                next_idx <- if (idx >= length(rv$years)) 1L else idx + 1L
                next_year <- rv$years[next_idx]
            }
            shiny::updateSliderInput(session, "year_anim", value = next_year)
        })

        shiny::observeEvent(c(input$quantity, input$year_anim), {
            req(rv$src, input$quantity, input$quantity %in% rv$quantities)
            dt <- quantity_data()
            if (!has_variable_day(dt)) {
                shinyjs::hide("day_container")
                shiny::updateSelectInput(session, "day", choices = character(0), selected = character(0))
                return()
            }
            shinyjs::show("day_container")
            year <- selected_year()
            days <- sort(unique(dt[dt$Year == year, "Day"]))
            if (length(days) < 1) {
                shiny::updateSelectInput(session, "day", choices = character(0), selected = character(0))
                return()
            }
            shiny::updateSelectInput(
                session,
                "day",
                choices = days,
                selected = days[1]
            )
        })

        timestep_data <- shiny::reactive({
            dt <- quantity_data()
            req(input$layer, input$layer %in% names(dt))
            year <- selected_year()
            filtered <- dt[dt$Year == year, c("Lon", "Lat", "Year", input$layer), drop = FALSE]
            if (has_variable_day(dt)) {
                valid_days <- as.character(dt[dt$Year == year, "Day"])
                req(input$day, as.character(input$day) %in% valid_days)
                filtered <- dt[dt$Year == year & as.character(dt$Day) == as.character(input$day),
                    c("Lon", "Lat", "Year", "Day", input$layer), drop = FALSE]
            }
            names(filtered)[names(filtered) == input$layer] <- "value"
            filtered
        })

        layer_limits <- shiny::reactive({
            dt <- quantity_data()
            req(input$layer, input$layer %in% names(dt))
            vals <- suppressWarnings(as.numeric(dt[[input$layer]]))
            vals <- vals[is.finite(vals)]
            if (length(vals) < 1) {
                return(NULL)
            }
            c(min(vals), max(vals))
        })

        output$spatial_plot <- shiny::renderPlot({
            dt <- timestep_data()
            req(nrow(dt) > 0)
            year <- selected_year()
            lims <- layer_limits()
            txt_col <- get_text_color()

            title <- paste(input$quantity, "-", input$layer, "| Year:", year)
            if ("Day" %in% names(dt)) {
                title <- paste(title, "Day:", input$day)
            }

            p <- ggplot2::ggplot(dt, ggplot2::aes(x = Lon, y = Lat, fill = value)) +
                ggplot2::geom_tile() +
                ggplot2::coord_equal() +
                ggplot2::labs(
                    title = title,
                    x = "Longitude",
                    y = "Latitude",
                    fill = input$layer
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    text = ggplot2::element_text(colour = txt_col),
                    axis.text = ggplot2::element_text(colour = txt_col),
                    axis.title = ggplot2::element_text(colour = txt_col),
                    plot.title = ggplot2::element_text(colour = txt_col),
                    legend.text = ggplot2::element_text(colour = txt_col),
                    legend.title = ggplot2::element_text(colour = txt_col),
                    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                    legend.box.background = ggplot2::element_rect(fill = "transparent", colour = NA)
                )
            if (is.null(lims)) {
                p <- p + ggplot2::scale_fill_viridis_c(option = "C", na.value = "grey90")
            } else {
                p <- p + ggplot2::scale_fill_viridis_c(
                    option = "C",
                    limits = lims,
                    na.value = "grey90"
                )
            }
            p
        }, bg = "transparent")

        shiny::observeEvent(input$spatial_click, {
            click <- input$spatial_click
            req(click, click$x, click$y)
            dt <- timestep_data()
            req(nrow(dt) > 0)

            d2 <- (dt$Lon - click$x)^2 + (dt$Lat - click$y)^2
            idx <- which.min(d2)
            rv$selected_cell <- list(
                Lon = dt$Lon[idx],
                Lat = dt$Lat[idx]
            )
        })

        output$selected_cell <- shiny::renderText({
            if (is.null(rv$selected_cell)) {
                return("Selected cell: none (click map)")
            }
            sprintf(
                "Selected cell: Lon=%.4f, Lat=%.4f",
                rv$selected_cell$Lon,
                rv$selected_cell$Lat
            )
        })

        pixel_timeseries <- shiny::reactive({
            req(rv$selected_cell)
            dt <- quantity_data()
            req(input$layer, input$layer %in% names(dt))

            ts <- dt[dt$Lon == rv$selected_cell$Lon & dt$Lat == rv$selected_cell$Lat,
                c("Lon", "Lat", "Year", intersect("Day", names(dt)), input$layer),
                drop = FALSE]
            names(ts)[names(ts) == input$layer] <- "value"
            ts <- ts[order(ts$Year, if ("Day" %in% names(ts)) ts$Day else ts$Year), ]
            ts
        })

        output$timeseries_plot <- plotly::renderPlotly({
            ts <- pixel_timeseries()
            req(nrow(ts) > 0)
            txt_col <- get_text_color()

            title <- sprintf(
                "%s - %s | Lon=%.4f Lat=%.4f",
                input$quantity,
                input$layer,
                rv$selected_cell$Lon,
                rv$selected_cell$Lat
            )

            if ("Day" %in% names(ts)) {
                ts$Date <- as.Date(paste(ts$Year, ts$Day), format = "%Y %j")
                if (all(is.na(ts$Date))) {
                    p <- ggplot2::ggplot(ts, ggplot2::aes(x = seq_len(nrow(ts)), y = value)) +
                        ggplot2::geom_line() +
                        ggplot2::geom_point(size = 0.8) +
                        ggplot2::labs(
                            title = title,
                            x = "Timestep",
                            y = input$layer
                        ) +
                        ggplot2::theme_minimal()
                } else {
                    p <- ggplot2::ggplot(ts, ggplot2::aes(x = Date, y = value)) +
                        ggplot2::geom_line() +
                        ggplot2::geom_point(size = 0.8) +
                        ggplot2::labs(
                            title = title,
                            x = "Date",
                            y = input$layer
                        ) +
                        ggplot2::theme_minimal()
                }
            } else {
                p <- ggplot2::ggplot(ts, ggplot2::aes(x = Year, y = value)) +
                    ggplot2::geom_line() +
                    ggplot2::geom_point(size = 0.8) +
                    ggplot2::labs(
                        title = title,
                        x = "Year",
                        y = input$layer
                    ) +
                    ggplot2::theme_minimal()
            }
            p <- p + ggplot2::theme(
                text = ggplot2::element_text(colour = txt_col),
                axis.text = ggplot2::element_text(colour = txt_col),
                axis.title = ggplot2::element_text(colour = txt_col),
                plot.title = ggplot2::element_text(colour = txt_col),
                panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
                plot.background = ggplot2::element_rect(fill = "transparent", colour = NA)
            )

            plotly::ggplotly(p) %>%
                plotly::layout(
                    paper_bgcolor = "rgba(0,0,0,0)",
                    plot_bgcolor = "rgba(0,0,0,0)",
                    font = list(color = txt_col),
                    title = list(font = list(color = txt_col)),
                    xaxis = list(
                        color = txt_col,
                        title = list(font = list(color = txt_col)),
                        tickfont = list(color = txt_col)
                    ),
                    yaxis = list(
                        color = txt_col,
                        title = list(font = list(color = txt_col)),
                        tickfont = list(color = txt_col)
                    )
                )
        })

        shiny::observeEvent(TRUE, {
            shinyjs::hide("day_container")
        }, once = TRUE)
    }

    app <- shiny::shinyApp(ui = ui, server = server)
    if (isTRUE(run)) {
        shiny::runApp(app, launch.browser = launch.browser)
        return(invisible(app))
    }
    app
}
