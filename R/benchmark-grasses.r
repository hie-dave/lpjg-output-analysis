#'
#' Do c3/c4 grass distribution benchmarks.
#'
#' @param settings: Benchmark settings.
#' @param params: Benchmark parameters.
#' @param tables: Benchmark tables.
#'
#' @name benchmark_grass_dist
#' @rdname benchmark_grass_dist
#' @import DGVMTools
#' @export
#' @return A list.
#' @author Drew Holzworth \email{d.holzworth@westernsydney.edu.au}
benchmark_grass_dist <- function(settings, params, tables) {
    var <- "dave_lai"
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

    do_aggregation <- function(field, name) {
        # Mean value each year.
        year_max <- DGVMTools::aggregateSubannual(field, "mean", "Year")

        # Mean c4 fraction over the full time period.
        mean_lai <- DGVMTools::aggregateYears(year_max, "mean")

        period_maps[[name]] <<- mean_lai
    }

    for (simulation in settings$simulations) {
        if (!has_output(simulation, "mlai")) {
            warning("No ", var, " data found for simulation ", simulation@name)
            next
        }

        predictions <- DGVMTools::getField(simulation, var, verbose = verbose)
        predictions@data <- calc_c4_frac(predictions@data)

        predictions <- DGVMTools::selectLayers(predictions, "c4_frac")

        period0 <- DGVMTools::selectYears(predictions, years[1], years[2])
        period1 <- DGVMTools::selectYears(predictions, years[3], years[4])

        name0 <- paste0(simulation@name, "_period0")
        name1 <- paste0(simulation@name, "_period1")

        do_aggregation(period0, name0)
        do_aggregation(period1, name1)

        diff <- period_maps[[name1]]
        p1 <- period_maps[[name1]]@data$c4_frac
        p0 <- period_maps[[name0]]@data$c4_frac
        dlt <- p1 - p0
        diff@data$c4_frac <- dlt
        diffs[[simulation@name]] <- diff

        meanfrac <- DGVMTools::aggregateSubannual(predictions)
        meanfrac <- DGVMTools::aggregateYears(meanfrac)
        means[[simulation@name]] <- meanfrac

        seasonal <- DGVMTools::aggregateSubannual(predictions, target = "Month")
        seasonals[[simulation@name]] <- seasonal

        trend <- DGVMBenchmarks::calcLinearTrend(predictions)
        trends[[simulation@name]] <- trend
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

    name0 <- names(means)[[1]]
    name1 <- names(means)[[2]]
    comparisons <- DGVMBenchmarks::fullSpatialComparison(benchmark, means
                                                         , trends, seasonals
                                                         , name0, name1)

    result <- list()
    result$period_maps <- period_maps
    result$diffs <- diffs
    result$trends <- trends
    result$comparisons <- comparisons
    result$means <- means
    result$seasonals <- seasonals
    return(result)
}