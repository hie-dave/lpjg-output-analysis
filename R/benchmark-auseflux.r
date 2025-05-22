#'
#' Do AusEFlux benchmarks for a particular variable.
#'
#' @param var Variable to be benchmarked. Should be one of: GPP, NEE, ER
#' @param settings Benchmark settings
#' @param params Benchmark parameters
#' @param tables Benchmark tables
#'
#' @return Returns a list containing the benchmark results, or NULL if DGVMBenchmarks is not available
#' @import DGVMTools
#' @import data.table
#' @import dplyr
#' @export
#'
benchmark_auseflux <- function(var,
                               settings,
                               params,
                               tables) {
    if (!requireNamespace("DGVMBenchmarks", quietly = TRUE)) {
        warning("DGVMBenchmarks package is required for benchmarking functionality")
        return(NULL)
    }

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
        maps[[name]] <<- DGVMTools::aggregateYears(year_sum, "mean")
    }

    do_aggregation(auseflux)

    guess_layer <- paste0("m", tolower(var))

    # The ID of this benchmark object is used as the quantity column in the
    # metrics table.
    log_diag("Defining new benchmark for AusEFlux ", var, "...")
    benchmark <- new("DaveBenchmark"
                     , id = var
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
    log_debug("Creating summary line for AusEFlux ", var, "...")
    # summary <- make_summary_line(benchmark, settings$summary_col_names)

    # Determine verbosity to be used when reading data.
    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")

    for (simulation in settings$simulations) {
        # To get ecosystem respiration, we must sum the outputs of autotrophic
        # and heterotrophic respiration.
        quant <- ifelse(var == "ER", "mra", guess_layer)

        log_diag("Will read quantity: ", quant)

        # Check if file is present.
        if (!has_output(simulation, quant)) {
            sim <- simulation@name
            stop("No ", quant, " data found for simulation ", sim)
        }

        log_diag("Reading ", quant, " from simulation ", simulation@name, "...")
        predictions <- DGVMTools::getField(simulation, quant
                                           , first.year = benchmark@first.year
                                           , last.year = benchmark@last.year
                                           , verbose = verbose)

        if (var == "ER") {
            # Predictions contain autotrophic respiration, but we also need
            # heterotrophic respiration.
            log_diag("Reading mrh from simulation ", simulation@name, "...")
            mrh <- DGVMTools::getField(simulation, "mrh"
                                       , first.year = benchmark@first.year
                                       , last.year = benchmark@last.year
                                       , verbose = verbose)
            log_diag("Copying mrh layer into mra field...")
            DGVMTools::copyLayers(mrh, predictions, "mrh")
            log_diag("Adding autotrophic and heterotrophic respiration...")
            layerOp(predictions, "+", c("mra", "mrh"), guess_layer)
            log_diag("Filtering out all layers except ", guess_layer, "...")
            predictions <- DGVMTools::selectLayers(predictions, guess_layer)
        }

        log_diag("Converting predicted ", var, " from kgC/m2 to gC/m2...")
        layerOp(predictions, "mulc", guess_layer, guess_layer, constant = 1000)
        predictions@quant@units <- "gC/m^2"

        # Change quantity name to match observations.
        predictions@quant@name <- auseflux@quant@name

        log_diag("Aggregating ", simulation@name, " ", var, " data...")
        do_aggregation(predictions)
    }

    log_diag("Performing full spatial comparison for AusEFlux ", var, "...")
    comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark, maps, trends
                                                         , seasonals
                                                         , params$new_name)
                                                         #, params$old_name)

    for (i in seq_along(comparisons[["Seasonal"]])) {
        comparisons[["Seasonal"]][[i]]@name <- sub("Seasonal comparison ", ""
                                        , comparisons[["Seasonal"]][[i]]@name)
    }

    log_diag("Constructing output tables for AusEFlux ", var, "...")
    # names(summary) <- names(tables[["totals"]])
    # tables[["totals"]] <- rbind(tables[["totals"]], summary)

    # Needed for TellMeEurope?
    sims <- settings$simulations
    # sims <- list()
    # sims[["GUESS"]] <- settings$simulations

    # metrics <- DGVMBenchmarks::makeMetricTable(benchmark, comparisons, sims)
    metrics <- make_metric_table(benchmark, comparisons$Values, sims)
    if (ncol(tables[["metrics"]]) != ncol(metrics)) {
        stop("Number of columns in metrics table does not match number of columns in auseflux metrics table. Master table has columns: [", paste(names(tables[["metrics"]]), collapse = ", "), "], and auseflux metrics table has columns: [", paste(names(metrics), collapse = ", "), "].")
    }
    tables[["metrics"]] <- rbind(tables[["metrics"]], metrics)

    log_diag("Successfully read all AusEFlux ", var, " data...")

    result <- list()
    result$maps <- maps
    result$trends <- trends
    result$seasonals <- seasonals
    result$comparisons <- comparisons
    result$tables <- tables
    result$benchmark <- benchmark
    return(result)
}

#'
#' Ensure that all data exists which is required for this benchmark.
#'
#' @param data_path Path to the data directory
#'
#' @export
#'
validate_auseflux <- function(data_path, var) {
    file <- auseflux_get_file_path(var, data_path)
    if (!file.exists(file)) {
        stop("Unable to locate Auseflux ", var, " data. File not found: ", file)
    }
}

#'
#' Read AusEFlux data from the specified data file, or from the default location if
#' no data file is provided.
#'
#' @param var Variable to read (see: [auseflux_sanitise_var])
#' @param data_path Path to the data directory
#'
#' @return Returns a [DGVMTools::Field] object containing the requested AusEFlux data
#' @import data.table
#' @import ncdf4
#' @export
#'
read_auseflux <- function(var, data_path) {
    var <- auseflux_sanitise_var(var)
    path <- auseflux_get_file_path(var, data_path)

    # Note: the name of this source object is used as the 'Dataset' column
    # in the metrics table.
    log_diag("Defining source auseflux with dir = ", dirname(path), "...")
    src <- defineSource("auseflux", "AusEFlux", format = DGVMTools::NetCDF
                        , dir = dirname(path))
    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")
    lyr <- paste0(var, "_median")
    lyr_new <- paste0("m", tolower(var))
    log_diag("Reading field ", lyr, " from file ", basename(path), "...")

    # Explicitly define a quantity in order to ensure consistency with the
    # quantity that's auto-generated when reading LPJ-Guess data. Normally, this
    # would be overwritten when using a NetCDF source, but the AusEFlux files
    # are not quite CF-conformant (e.g. they lack a variable-level units
    # attribute).
    quant <- DGVMTools::defineQuantity(lyr_new, var, "gC/m^2")

    field <- DGVMTools::getField(src, layers = lyr, quant = quant
                                 , file.name = basename(path)
                                 , verbose = verbose)
    # log_diag("Renaming AusEFlux field ", lyr, " to ", lyr_new, "...")
    # DGVMTools::renameLayers(field, lyr, lyr_new)
    log_diag("Successfully read auseflux ", var, " data")
    return(field)
}

#'
#' Get the path to the AusEFlux data file for the specified variable.
#'
#' @param var Variable to read
#' @param data_path Path to directory containing data
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
#' @param var Variable to read
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
