
#'
#' Do AusEFlux benchmarks for a particular variable.
#'
#' @param var Variable to be benchmarked. Should be one of: GPP, NEE, ER
#' @param settings: Benchmark settings.
#' @param params: Benchmark parameters.
#' @param tables_list: Benchmark tables.
#'
#' @name benchmark_bom_lai
#' @rdname benchmark_bom_lai
#' @import DGVMTools
#' @import DGVMBenchmarks
#' @import data.table
#' @import dplyr
#' @export
#' @return A DGVMTools::Field object containing the BoM LAI data.
#' @author Drew Holzworth \email{d.holzworth@@westernsydney.edu.au}
#'
benchmark_auseflux <- function(var,
                               settings,
                               params,
                               tables_list) {
    var <- auseflux_sanitise_var(var)

    auseflux <- read_auseflux(var, settings$data_path)

    # Mean maximum annual LAI for each dataset.
    maps <- list()

    # Trend in LAI for each dataset.
    trends <- list()

    # Mean LAI for each month of the year, for each dataset.
    seasonals <- list()

    do_aggregation <- function(field) {
        name <- field@source@name

        # Longterm trend (gC m-2 month-2).
        trends[[name]] <<- DGVMBenchmarks::calcLinearTrend(field)

        # Mean GPP each month (gC m-2 month-1).
        seasonals[[name]] <<- DGVMTools::aggregateYears(field, "mean")

        # Total GPP each year (gC m-2 year-1).
        year_sum <- DGVMTools::aggregateSubannual(field, "sum", "Year")

        # Mean maximum annual GPP (gC m-2 year-1)
        maps[[name]] <- DGVMTools::aggregateYears(year_sum, "mean")
    }

    do_aggregation(auseflux)

    guess_layer <- paste0("m", tolower(var))

    benchmark <- new("DaveBenchmark"
                     , id = "auseflux"
                     , name = paste("AusEFlux", var)
                     , description = paste("AusEFlux", var, "estimates")
                     , simulation = "tellaus"
                     , guess_var = guess_layer
                     , guess_layers = guess_layer
                     , unit = "gC/m2/month"
                     , agg.unit = "gC/m2/month"
                     , datasets = list(auseflux)
                     , first.year = min(auseflux@data$Year)
                     , last.year = max(auseflux@data$Year)
                     , metrics = settings$metrics
                     , simulation_format = "GUESS"
                     , dataset_source = "<AusEFlux reference should go here>")

    # Define summary lines.
    summary <- make_summary_line(benchmark, settings$summary_col_names)

    # Determine verbosity to be used when reading data.
    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")

    for (simulation in settings$simulations) {
        # Check if file is present.
        if (!has_output(simulation, guess_layer)) {
            warning("No ", guess_layer, " data found for simulation "
                    , simulation@name)
            next
        }

        predictions <- DGVMTools::getField(source = simulation, guess_layer
                                           , first.year = benchmark@first.year
                                           , last.year = benchmark@last.year
                                           , verbose = verbose)
        layerOp(predictions, "mulc", guess_layer, guess_layer, constant = 1000)
        do_aggregation(predictions)
    }

    comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark, maps, trends
                                                         , seasonals
                                                         , params$new_name
                                                         , params$old_name)

    names(summary) <- names(overall_tables[["totals"]])
    overall_tables[["totals"]] <- rbind(overall_tables[["totals"]], summary)

    sims <- list()
    sims[["GUESS"]] <- settings$simulations

    metrics <- DGVMBenchmarks::makeMetricTable(benchmark, comparisons, sims)
    overall_tables[["metrics"]] <- rbind(overall_tables[["metrics"]], metrics)

    result <- list()
    result$maps <- maps
    result$trends <- trends
    result$seasonals <- seasonals
    result$comparisons <- comparisons
    result$tables <- overall_tables
    result$benchmark <- benchmark
    return(result)
}

#'
#' Read BoM LAI from the specified data file, or from the default location if
#' no data file is provided.
#'
#' @param var The AusEFlux variable.
#' @param data_path Data path.
#'
#' @name read_auseflux
#' @rdname read_auseflux
#' @import DGVMTools
#' @import data.table
#' @import ncdf4
#' @export
#' @return A DGVMTools::Field object containing the requested AusEFlux data.
#' @author Drew Holzworth \email{d.holzworth@@westernsydney.edu.au}
#'
read_auseflux <- function(var, data_path) {
    var <- auseflux_sanitise_var(var)
    path <- auseflux_get_file_path(var, data_path)

    name <- paste("AusEFlux", var)
    src <- defineSource("auseflux", name, format = DGVMTools::NetCDF
                        , dir = dirname(path))
    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")
    lyr <- paste0(var, "_median")
    field <- DGVMTools::getField(src, layers = lyr, quant = lyr
                                 , file.name = basename(path)
                                 , verbose = verbose)
    DGVMTools::renameLayers(field, lyr, paste0("m", tolower(var)))
    return(field)
}

#'
#' Get the path to the AusEFlux data file for the specified variable.
#'
#' @param var The AusEFlux variable.
#' @param data_path Path to directory containing data.
#'
#' @keywords internal
#'
auseflux_get_file_path <- function(var, data_path) {
    var <- auseflux_sanitise_var(var)
    filename <- paste0("AusEFlux_", var, "_2003_2022_5km_quantiles_v1.1.nc")

    dir <- file.path(data_path, "auseflux")
    if (!dir.exists(dir)) {
        dir <- data_path
    }
    return(file.path(dir, filename))
}

#'
#' Attempt to interpret the specified variable as a valid AusEFlux variable, and
#' return the name of the corresponding AusEFlux variable. stop() will be called
#' if the variable is invalid.
#'
#' @param var The AusEFlux variable.
#'
#' @keywords internal
#'
auseflux_sanitise_var <- function(var) {
    var <- toupper(var)
    if (var != "GPP" && var != "NEE" && var != "ER") {
        stop("Invalid AusEFlux var: '", var, "''. Must be one of: GPP, NEE, ER")
    }
    return(var)
}
