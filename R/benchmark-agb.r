#'
#' Do Above-Ground Biomass benchmark.
#'
#' @param params Benchmark parameters
#' @param settings Benchmark settings
#' @param tables Benchmark tables
#'
#' @name benchmark_agb
#' @rdname benchmark_agb
#' @export
#' @return Returns a list containing the following named items:
#' - maps: Max AGB in each gridcell.
#' - trends: Temporal trend in AGB for each gridcell
#'
#' - comparisons: Output of fullSpatialComparison()
#' - tables: Benchmark tables
#' - benchmark: The benchmark object containing standard metadata
#'
benchmark_agb <- function(params, settings, tables) {
    if (!requireNamespace("DGVMBenchmarks", quietly = TRUE)) {
        warning("DGVMBenchmarks package is required for GPP benchmarking")
        return(NULL)
    }

    fullcam_data <- read_fullcam(settings$data_path)

    benchmark <- new("DaveBenchmark",
                     id = "AGB",
                     name = "Above-Ground Biomass",
                     description = "Above-Ground Biomass",
                     simulation = "FullCAM",
                     guess_var = "aagb",
                     guess_layers = "mgpp",
                     unit = "kg m^-2",
                     agg.unit = "PgC y^-1",
                     datasets = list(fullcam_data),
                     first.year = 1970,
                     last.year = 2017,
                     metrics = settings$metrics,
                     simulation_format = "GUESS",
                     dataset_source = "<FullCAM reference should go here>")

    # Define summary lines.
    summary <- make_summary_line(benchmark, settings$summary_col_names)

    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")

    maps <- list()
    trends <- list()
    totals <- list()

    do_aggregation <- function(field) {
        name <- field@source@name

        # Longterm trend.
        # trends[[name]] <<- DGVMBenchmarks::calcLinearTrend(field)

        # Total AGB across the spatial domain at each timestep.
        # totals[[name]] <- DGVMTools::aggregateSpatial(field, "sum")

        # Maximum (potential) AGB.
        annual <- DGVMTools::aggregateSubannual(field, "max")
        maps[[name]] <<- DGVMTools::aggregateYears(annual, "max")
    }

    DGVMTools::renameLayers(fullcam_data, "agb", "aagb")
    do_aggregation(fullcam_data)
    # maps[[fullcam_data@source@name]] <- DGVMTools::aggregateYears(DGVMTools::aggregateSubannual(fullcam_data, "max"), "max")

    for (simulation in settings$simulations) {
        # Check if output file is present. If not, skip this simulation.
        if (!has_output(simulation, benchmark@guess_var)) {
            warning("No ", benchmark@guess_var, " data found for simulation ",
                    , simulation@name)
            next
        }

        # Note: Reading the full timeseries here, because I'm not sure how
        # relevant the fullcam start/end years are. Must dicuss with the others.
        predictions <- DGVMTools::getField(source = simulation
                                           , benchmark@guess_var
                                           , verbose = verbose
                                           , layers = "total")
        predictions@quant@units <- "kg/m2"
        predictions@quant@name <- "Above-Ground Biomass"
        DGVMTools::renameLayers(predictions, "total", "aagb")
        do_aggregation(predictions)
    }

    comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark, maps, NULL
                                                         , NULL
                                                         , params$new_name)

    # names(summary) <- names(tables[["totals"]])
    # tables[["totals"]] <- rbind(tables[["totals"]], summary)

    # metrics <- make_metric_table(benchmark, comparisons$Values
    #                              , settings$simulations)
    # tables[["metrics"]] <- rbind(tables[["metrics"]], metrics)

    result <- list()
    result$maps <- maps
    result$trends <- trends
    result$totals <- totals
    result$comparisons <- comparisons
    result$tables <- tables
    result$benchmark <- benchmark
    return(result)
}

#'
#' Read Fullcam potential AGB data from the specified data path.
#'
#' @param data_path Base path to data files.
#' @param highres True to read the high-resolution (0.0025°) dataset. False to
#'                read the low-resolution (1°) dataset.
#'
#' @name read_fullcam
#' @rdname read_fullcam
#' @import DGVMTools
#' @keywords internal
#' @return Returns a [DGVMTools::Field] object containing fullcam AGB data.
read_fullcam <- function(data_directory, highres = FALSE) {
    file_path <- get_fullcam_filepath(data_directory, highres)
    log_diag("Expected fullcam file path: '", file_path, "'")
    src <- defineSource(id = "Fullcam_MLayer"
                        , name = "FullCAM"
                        , dir = dirname(file_path)
                        , format = NetCDF)
    fld <- getField(src, "agb", file.name = basename(file_path))
    fld@quant@units <- "kg/m2"
    return(fld)
}

#'
#' Get the path to the fullcam data file from the specified base data path.
#'
#' @param data_path Base path to data files.
#' @param highres True to read the high-resolution (0.0025°) dataset. False to
#'                read the low-resolution (1°) dataset.
#'
#' @name get_fullcam_filepath
#' @rdname get_fullcam_filepath
#' @keywords internal
#' @return Returns a string containing the expected path to the fullcam AGB data
#' file, relative to the base data directory path.
get_fullcam_filepath <- function(data_directory, highres = FALSE) {
    dir_name <- "site-potential-and-fpi-version-2.0"
    dir <- file.path(data_directory, "fullcam", dir_name)
    file_name <- if (highres) "New_M_2019.nc" else "New_M_2019_1deg.nc"
    return(file.path(dir, file_name))
}

#'
#' Ensure that the required files for the Above-Ground Biomass benchmark exist
#' in the given data directory.
#'
#' @param data_path Base path to data files.
#' @param highres True to read the high-resolution (0.0025°) dataset. False to
#'                read the low-resolution (1°) dataset.
#' @name validate_agb
#' @rdname validate_agb
#' @keywords internal
#'
validate_agb <- function(data_directory, highres = FALSE) {
    fullcam_path <- get_fullcam_filepath(data_directory, highres)
    if (!file.exists(fullcam_path)) {
        stop("Unable to locate FullCAM Above-Ground Biomass data file: '"
             , fullcam_path, "'")
    }
}
