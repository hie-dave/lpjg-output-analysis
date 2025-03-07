#'
#' Do GPP benchmark, using Auseflux and GOSIF GPP products. The GOSIF GPP will
#' be cropped to only contain Australia.
#'
#' @param params Benchmark parameters
#' @param settings Benchmark settings
#' @param tables Benchmark tables
#'
#' @name benchmark_bom_lai
#' @rdname benchmark_bom_lai
#' @import DGVMTools
#' @import DGVMBenchmarks
#' @import data.table
#' @import dplyr
#' @export
#' @return Returns a list containing the following named items:
#' - maps: Mean of yearly total GPP for each gridcell
#' - trends: Temporal trend in GPP for each gridcell
#' - seasonals: Gridded mean monthly GPP for each month over the time period
#' - comparisons: Output of fullSpatialComparison()
#' - tables: Benchmark tables
#' - benchmark: The benchmark object containing standard metadata
#'
benchmark_gpp <- function(params, settings, tables) {
    # Note: auseflux and gosif gpp are both in gC/m2/month.
    gosif <- read_gosif(params$data_directory)
    auseflux_gpp <- read_auseflux("GPP", params$data_directory)

    data_fields <- list()
    data_fields[[gosif@source@name]] <- gosif
    data_fields[[auseflux_gpp@source@name]] <- auseflux_gpp

    first_year <- max(sapply(data_fields, \(f) min(f@data$Year)))
    last_year <- min(sapply(data_fields, \(f) max(f@data$Year)))

    benchmark <- new("DaveBenchmark",
                     id = "GPP",
                     name = "Australian GPP",
                     description = "Australian GPP",
                     simulation = "tellaus",
                     guess_var = "mgpp",
                     guess_layers = "mgpp",
                     unit = "kg m^-2 y^-1",
                     agg.unit = "PgC y^-1",
                     datasets = data_fields,
                     first.year = first_year,
                     last.year = last_year,
                     metrics = settings$metrics,
                     simulation_format = "GUESS",
                     dataset_source = "<AusEFlux reference should go here>")

    summary_lines <- make_summary_line(benchmark, summary_col_names)

    maps <- list()
    trends <- list()
    seasonal <- list()
    # totals <- list()

    # Coordinates defining a rough box around Australia.
    min_lon <- 110
    max_lon <- 155
    min_lat <- -45
    max_lat <- -10

    aggregate <- function(field) {
        name <- field@source@name

        # Calculate slope.
        trends[[name]] <<- calcLinearTrend(field)

        # Get mean of yearly totals.
        maps[[name]] <<- aggregateYears(aggregateSubannual(field, "sum"))

        # Get mean of each monthly value across the time period.
        seasonal[[name]] <<- aggregateYears(field)

        # Get total GPP over the time period.
        # Note: disabling due to bug in DGVMTools.
        #
        # KG_TO_PG <- 1/1e+12
        # lons <- sort(unique(field@data$Lon))
        # lats <- sort(unique(field@data$Lat))
        # total <- areaWeightedTotal(maps[[name]], "m^2", lons, lats)
        # totals[[name]] <- total@data * KG_TO_PG
    }

    for (field in benchmark@datasets) {
        # Trim to the overlapping time period.
        field <- selectYears(field, first_year, last_year)

        # GOSIF is global, so let's cut out Australia.
        d <- field@data
        d <- d[d$Lon > min_lon & d$Lon < max_lon, ]
        d <- d[d$Lat > min_lat & d$Lat < max_lat, ]
        field@data <- d
        rm(d)

        # Remove missing values.
        field@data <- field@data[field@data$mgpp != -9999.0, ]

        # Aggregate to 1-degree spatial resolution.
        field@data <- aggregate_to_1deg(field@data)

        # Convert to kg.
        field <- layerOp(field, "divc", "mgpp", "mgpp", 1000)
        field@quant@units <- "kgC/m^2"

        aggregate(field)
        rm(field)
    }

    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_INFORMATION")

    for (src in settings$simulations) {
        # Skip this simulation if it doesn't have the required output.
        if (!has_output(src, benchmark@guess_var)) {
            next()
        }

        # Read the output file from this simulation.
        field <- getField(src, benchmark@guess_var, first.year = first_year,
                          last.year = last_year, verbose = verbose)

        # Aggregate to 1-degree spatial resolution.
        field@data <- aggregate_to_1deg(field@data)

        # Note: mgpp output is in kgC/m2, so no need for unit conversion.
        aggregate(field)
        rm(field)
    }

    # Ensure that at least one simulation had the required output.
    if (length(maps) == length(data_fields)) {
        stop(paste0("No simulations have output for ", benchmark@guess_var))
    }

    new_name <- params$new_name
    old_name <- params$old_name

    if (!(new_name %in% names(maps))) {
        new_name <- NULL
    }
    if (!(old_name %in% names(maps))) {
        old_name <- NULL
    }

    comparisons <- fullSpatialComparison(benchmark, maps, trends, seasonal,
                                         new_name, old_name)

    # tables$totals <- rbind(tables$totals, summary_lines)
    m <- make_metric_table(benchmark, comparisons$Values, settings$simulations)
    tables$metrics <- rbind(tables$metrics, m)

    result <- list()
    result$maps <- maps
    result$trends <- trends
    result$seasonals <- seasonal
    result$comparisons <- comparisons
    result$tables <- tables
    result$benchmark <- benchmark
    # result$totals <- totals

    return(result)
}

#'
#' Read GOSIF GPP from the specified data path.
#'
#' @param data_path Path to data files
#'
#' @name read_gosif
#' @rdname read_gosif
#' @import DGVMTools
#' @keywords internal
#' @return Returns a [DGVMTools::Field] object containing GOSIF global monthly GPP
#'
read_gosif <- function(data_path) {
    gosif_file_name <- "GOSIF_GPP.txt.gz"
    gosif_dir <- file.path(data_path, "gosif_gpp")
    gosif_src <- defineSource(id = "GOSIF_GPP", name = "GOSIF GPP",
                              dir = gosif_dir, format = GUESS)

    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_INFORMATION")
    field <- getField(gosif_src, "mgpp", file.name = gosif_file_name,
                      verbose = verbose)
    return(field)
}
