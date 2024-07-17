
#'
#' Do BoM LAI benchmarks.
#'
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
benchmark_bom_lai <- function(settings,
                              params,
                              tables_list) {

    # Mean maximum annual LAI for each dataset.
    maps <- list()

    # Trend in LAI for each dataset.
    trends <- list()

    # Mean LAI for each month of the year, for each dataset.
    seasonals <- list()

    areas <- list()

    # Read observed data and calculate annual mean over the time period.
    bom_data <- read_bom_lai(settings$data_path)

    do_aggregation <- function(field) {
        name <- field@source@name

        # Longterm trend.
        trends[[name]] <<- DGVMBenchmarks::calcLinearTrend(field)

        # Mean LAI each month.
        seasonals[[name]] <<- DGVMTools::aggregateYears(field, "mean")

        # Maximum LAI each year.
        year_max <- DGVMTools::aggregateSubannual(field, "max", "Year")

        # Mean maximum annual LAI.
        mean_lai <- DGVMTools::aggregateYears(year_max, "mean")

        # m2_to_megam2 <- 1/1e+12
        # lons <- sort(unique(field@data$Lon))
        # lats <- sort(unique(field@data$Lat))
        # area <- areaWeightedTotal(mean_lai, "m^2", lons, lats)
        # areas[[name]] <<- area@data * m2_to_megam2

        maps[[name]] <<- mean_lai
    }

    # Aggregation/analysis of observations.
    do_aggregation(bom_data)

    # Create benchmark object.
    benchmark <- new("DaveBenchmark"
                     , id = "bom_lai"
                     , name = "BoM Total LAI"
                     , description = "BoM Total LAI estimates"
                     , simulation = "tellaus"
                     , guess_var = "mlai"
                     , guess_layers = "mlai"
                     , unit = "m2/m2"
                     , agg.unit = "m2/m2"
                     , datasets = list(bom_data)
                     , first.year = min(bom_data@data$Year)
                     , last.year = max(bom_data@data$Year)
                     , metrics = settings$metrics
                     , simulation_format = "GUESS"
                     , dataset_source = "<BoM reference should go here>")

    # Define summary lines.
    summary <- make_summary_line(benchmark, settings$summary_col_names)

    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")

    # Iterate over simulation sources.
    for (simulation in settings$simulations) {
        # Check if file is present. If not, skip this simulation.
        # TBI: support for compressed output files.
        if (!has_output(simulation, benchmark@guess_var)) {
            warning("No ", benchmark@guess_var, " data found for simulation "
                    , simulation@name)
            next
        }

        predictions <- DGVMTools::getField(source = simulation
                                           , benchmark@guess_var
                                           , first.year = benchmark@first.year
                                           , last.year = benchmark@last.year
                                           , verbose = verbose)
        do_aggregation(predictions)
    }

    comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark, maps
                                                         , trends, seasonals
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
    result$areas <- areas
    result$comparisons <- comparisons
    result$tables <- overall_tables
    result$benchmark <- benchmark
    return(result)
}

#'
#' Read BoM LAI from the specified data file, or from the default location if
#' no data file is provided.
#'
#' @param data_path: Path to the directory containing data.
#' @name read_bom_lai
#' @rdname read_bom_lai
#' @import DGVMTools
#' @import data.table
#' @import ncdf4
#' @export
#' @return A DGVMTools::Field object containing the BoM LAI data.
#' @author Drew Holzworth \email{d.holzworth@@westernsydney.edu.au}
#'
read_bom_lai <- function(data_path) {
    data_file <- get_bom_understory_location(data_path)

    if (!file.exists(data_file)) {
        stop("Unable to locate BoM LAI data. File not found: ", data_file)
    }

    # Create a DGVMTools::Source object.
    source <- DGVMTools::defineSource("bom_lai", "BoM LAI"
                                      , format = DGVMTools::NetCDF
                                      , dir = dirname(data_file))

    # Read data from the input file.
    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")
    field <- DGVMTools::getField(source, layers = "Band1", quant = "Band1"
                                 , file.name = basename(data_file)
                                 , verbose = verbose)
    DGVMTools::renameLayers(field, "Band1", "mlai")
    return(field)
}

#'
#' Get the location of the BoM understory LAI data that is shipped with this
#' package.
#'
#' @keywords internal
#' @return Path to the NetCDF file containing BoM LAI data.
#' @param author Drew Holzworth
#'
get_bom_understory_location <- function(data_path) {
    filename <- "lai_rec.nc"
    return(get_bom_location(data_path, filename))
}

#'
#' Get the location of the BoM overstory LAI data that is shipped with this
#' package.
#'
#' @keywords internal
#' @return Path to the NetCDF file containing BoM LAI data.
#' @param author Drew Holzworth
#'
get_bom_overstory_location <- function(data_path) {
    filename <- "lai_per.nc"
    return(get_bom_location(data_path, filename))
}

#'
#' Get the location of the BoM total LAI data that is shipped with this
#' package.
#'
#' @keywords internal
#' @return Path to the NetCDF file containing BoM LAI data.
#' @param author Drew Holzworth
#'
get_bom_total_location <- function(data_path) {
    filename <- "lai_per.nc"
    return(get_bom_location(data_path, filename))
}

#'
#' Get the location of the BoM LAI data.
#'
#' @param data_path: Path to the data directory provided alongside this package.
#' @param filename: Filename, which depends on whether we want understory,
#' overstory, or total LAI.
#'
#' @keywords internal
#' @return Path to the NetCDF file containing BoM LAI data.
#' @param author Drew Holzworth
#'
get_bom_location <- function(data_path, filename) {
    dir <- file.path(data_path, "bom_lai")
    if (!dir.exists(dir)) {
        dir <- data_path
    }
    return(file.path(dir, filename))
}
