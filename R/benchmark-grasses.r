#'
#' Do C3/C4 grass distribution benchmarks.
#'
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
benchmark_grasses <- function(settings, params, tables) {
    if (!requireNamespace("DGVMBenchmarks", quietly = TRUE)) {
        warning("DGVMBenchmarks package is required for grass distribution benchmarking")
        return(NULL)
    }

    default_var <- "dave_lai"
    verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")
    years <- settings$grass_dist_years

    calc_c4_frac <- function(data) {
        varieties <- c(c("_perennial", "_annual", ""))
        c4_pfts <- paste0("C4G", varieties)
        c3_pfts <- paste0("C3G", varieties)

        suffices <- c("", "_mean")
        c4_pfts <- unlist(lapply(suffices, function(x) paste0(c4_pfts, x)))
        c3_pfts <- unlist(lapply(suffices, function(x) paste0(c3_pfts, x)))

        c4_pfts <- c4_pfts[which(c4_pfts %in% names(data))]
        c3_pfts <- c3_pfts[which(c3_pfts %in% names(data))]
        all_pfts <- c(c4_pfts, c3_pfts)

        newcol <- "c4_frac"

        data$c4_frac <- rowSums(data[, ..c4_pfts]) / rowSums(data[, ..all_pfts])
        data <- data[!is.na(data$c4_frac), ]
        return(data)
    }

    period_maps <- list()
    diffs <- list()
    means <- list()
    seasonals <- list()
    trends <- list()

    first_year <- 1e6
    last_year <- -1e6

    do_aggregation <- function(field, name) {
        first_year <<- min(first_year, field@first.year)
        last_year <<- max(last_year, field@last.year)

        # Mean value each year.
        year_max <- DGVMTools::aggregateSubannual(field, "mean", "Year")

        # Mean c4 fraction over the full time period.
        mean_lai <- DGVMTools::aggregateYears(year_max, "mean")

        if (nrow(mean_lai@data) > 0) {
            period_maps[[name]] <<- mean_lai
        }
    }

    for (simulation in settings$simulations) {
        var <- default_var
        if (!has_output(simulation, var)) {
            log_warning("No daily LAI (", var, ") data found for simulation '", simulation@name, "'. Falling back to annual LAI (not ideal!)")
            var <- "lai"
        }
        if (!has_output(simulation, var)) {
            log_warning("No annual LAI (", var, ") data found for simulation '", simulation@name, "'. This simulation will not be included in the C3/C4 grass distribution benchmark.")
            next
        }

        predictions <- DGVMTools::getField(simulation, var, verbose = verbose)
        predictions@data <- calc_c4_frac(predictions@data)
        predictions@quant@name <- "C4 Fraction"
        predictions@quant@units <- "0-1"

        predictions <- DGVMTools::selectLayers(predictions, "c4_frac")

        period0 <- DGVMTools::selectYears(predictions, years[1], years[2])
        period1 <- DGVMTools::selectYears(predictions, years[3], years[4])

        name0 <- paste0(simulation@name, "_period0")
        name1 <- paste0(simulation@name, "_period1")

        do_aggregation(period0, name0)
        do_aggregation(period1, name1)

        # It's possible that some gridcells may be missing from one or both
        # data frames. Subtracting two data frames with different rows is going
        # to cause problems, so we need to merge them.
        if (name1 %in% names(period_maps) && name0 %in% names(period_maps)) {
            p1 <- period_maps[[name1]]@data
            p0 <- period_maps[[name0]]@data

            # Calculate change in C4 fraction between the two periods.
            merged <- base::merge(p1, p0, by = c("Lon", "Lat"), all = TRUE)
            merged$c4_frac <- merged$c4_frac.x - merged$c4_frac.y
            merged <- merged[, c("Lon", "Lat", "c4_frac")]

            diff <- period_maps[[name1]]
            diff@data <- merged
            diffs[[simulation@name]] <- diff
        }

        meanfrac <- DGVMTools::aggregateSubannual(predictions)
        meanfrac <- DGVMTools::aggregateYears(meanfrac)
        means[[simulation@name]] <- meanfrac

        if (predictions@subannual.resolution == "Year") {
            # Annual output - can't do seasonal analysis.
            seasonals[[simulation@name]] <- NULL
            trends[[simulation@name]] <- NULL
        } else {
            seasonal <- DGVMTools::aggregateSubannual(predictions, target = "Month")
            seasonals[[simulation@name]] <- seasonal

            trend <- DGVMBenchmarks::calcLinearTrend(predictions)
            trends[[simulation@name]] <- trend
        }
    }

    if (length(period_maps) == 0) {
        stop("No simulations have LAI outputs suitable for a C3/C4 grass distribution analysis. Must have at least file_lai, or, better still, file_dave_lai")
    }

    benchmark <- new("DaveBenchmark"
                     , id = "c4_distribution"
                     , name = "C3/C4 Grass Distribution"
                     , description = "Distribution of C3 and C4 Grasses"
                     , simulation = "tellaus"
                     , guess_var = "c4_frac"
                     , guess_layers = "c4_frac"
                     , unit = "0-1"
                     , agg.unit = "0-1"
                     , datasets = list()
                     , first.year = predictions@first.year
                     , last.year = predictions@last.year
                     , metrics = settings$metrics
                     , simulation_format = "GUESS"
                     , dataset_source = "<Add reference once we have data>")

    comparisons <- NULL
    if (length(trends) > 0) {
        name0 <- names(means)[[1]]
        name1 <- names(means)[[2]]
        comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark, means
                                                            , trends, seasonals
                                                            , name0)#, name1)

        for (i in seq_along(comparisons[["Seasonal"]])) {
            name <- comparisons[["Seasonal"]][[i]]@name
            name <- sub("Seasonal comparison ", "", name)
            comparisons[["Seasonal"]][[i]]@name <- name
        }
    }

    # TODO: generate metric table once we have some observations.
    # This requires us to set benchmark@datasets

    result <- list()
    result$period_maps <- period_maps
    result$diffs <- diffs
    result$trends <- trends
    result$comparisons <- comparisons
    result$means <- means
    result$seasonals <- seasonals
    result$tables <- tables
    result$benchmark <- benchmark
    result$first_year <- first_year
    result$last_year <- last_year
    return(result)
}
