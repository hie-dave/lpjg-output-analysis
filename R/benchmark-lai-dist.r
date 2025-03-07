
#'
#' Compare a layer between the specified fields.
#'
#' @param fields List of fields.
#' @param i index of the first field to be compared (1-indexed)
#' @param j index of the second field to be compared (1-indexed)
#'
#' @keywords internal
#'
compare <- function(fields, i, j, lyr) {
    return(compareLayers(fields[[i]], fields[[j]], lyr, show.stats = FALSE))
}

#'
#' Compare a layer between all combinations of the fields.
#'
#' E.g. for 3 fields: compare fields 1&2, 1&3, 2&3.
#'
#' @param fields The fields to be compared.
#' @param lyr The layer to be compared in the fields. Must exist in all fields.
#'
#' @export
#'
compare_all <- function(fields, lyr) {
    result <- list()
    combinations <- combn(seq_along(fields), 2)
    for (combination in split(combinations, col(combinations))) {
        i <- combination[[2]]
        j <- combination[[1]]
        comparison <- compare(fields, i, j, lyr)
        name <- paste(names(fields)[[i]], "-", names(fields)[[j]])
        result[[name]] <- comparison
    }
    return(result)
}

#'
#' Benchmark species LAI distribution.
#'
#' Note that the subannual (monthly) LAI output does not distinguish between
#' PFTs, so this benchmark only considers annual LAI.
#'
#' @param settings Benchmark settings.
#' @param params Benchmark parameters.
#' @param tables Benchmark tables (rbind-ed and outputted as result$tables).
#'
#' @export
#'
benchmark_lai_dist <- function(settings, params, tables) {
    fields <- list()

    add_field <- function(field) {
        aggregated <- aggregateYears(aggregateSubannual(field))
        fields[[field@source@name]] <<- aggregated
    }

    bom_data <- read_bom_tree_dist(params$data_directory)
    add_field(bom_data)

    for (simulation in settings$simulations) {
        field <- read_guess(simulation, fields)
        add_field(field)
        rm(field)
    }

    first_year <- max(sapply(fields, \(x) x@first.year))
    last_year <- min(sapply(fields, \(x) x@last.year))

    comparisons <- list()

    comparisons$lai <- compare_all(fields, "total")
    comparisons$tree_lai <- compare_all(fields, "tree_total")
    comparisons$grass_lai <- compare_all(fields, "grass_total")
    # comparisons$shrub_lai <- compare_all(fields, "shrub_total")

    comparisons$tree_frac <- compare_all(fields, "tree_frac")
    comparisons$grass_frac <- compare_all(fields, "grass_frac")
    # comparisons$shrub_frac <- compare_all(fields, "shrub_frac")

    benchmark <- new("DaveBenchmark"
                     , id = "LAI"
                     , name = "BoM LAI"
                     , description = "LAI Distribution based on BoM estimates"
                     , simulation = "tellaus"
                     , guess_var = "LAI"
                     , guess_layers = "LAI"
                     , unit = "m2/m2"
                     , agg.unit = "m2/m2"
                     , datasets = list(bom_data)
                     , first.year = first_year
                     , last.year = last_year
                     , metrics = settings$metrics
                     , simulation_format = "GUESS"
                     , dataset_source = "<BoM reference should go here>")

    # Create metrics table.
    m <- make_metric_table(benchmark, comparisons$lai, settings$simulations)
    tables[["metrics"]] <- rbind(tables[["metrics"]], m)

    result <- list()
    result$maps <- fields
    result$comparisons <- comparisons
    result$benchmark <- benchmark
    result$tables <- tables

    return(result)
}

#'
#' Calculate the sum of the specified columns.
#'
#' @param df A data frame.
#' @param pfts Names of columns to be summed.
#'
#' @keywords internal
#'
sum_pfts <- function(df, pfts) {
    pfts <- pfts[which(pfts %in% names(df))]
    if (length(pfts) < 1) {
        return(rep(0, nrow(df)))
    }
    return(rowSums(df[, ..pfts]))
}

#'
#' Read lpj-guess LAI outputs.
#' 
#' This function reads LPJ-GUESS outputs and post-processes them to calculate
#' species distribution.
#'
#' @param src A [DGVMTools::Source] object.
#'
#' @keywords internal
#'
read_guess <- function(src, fields) {
    # Note: monthly LAI output does not divide LAI between PFTs; it only
    # shows total gridcell-level LAI.
    c4_pfts <- c("C4G", "C4G_annual", "C4G_perennial")
    c3_pfts <- c("C3G", "C3G_annual", "C3G_perennial")
    grass_pfts <- c(c3_pfts, c4_pfts)
    shrub_pfts <- c("BLSE", "BLSS")
    tree_pfts <- c("BNE", "BINE", "BNS", "TeNE", "TeBS", "IBS", "TeBE", "TrBE"
                   , "TrIBE", "TrBR")

    field <- getField(src, "lai")

    df <- field@data
    df$grass_total <- sum_pfts(df, grass_pfts)
    df$shrub_total <- sum_pfts(df, shrub_pfts)
    df$tree_total <- df$Total - df$grass_total - df$shrub_total

    df$grass_frac <- df$grass_total / df$Total
    df$shrub_frac <- df$shrub_total / df$Total
    df$tree_frac <- df$tree_total / df$Total

    df$c4_total <- sum_pfts(df, c4_pfts)
    df$c4_frac <- df$c4_total / df$grass_total

    # Aggregate to 1-degree if necessary.
    if (length(fields) > 0) {
        baseline <- fields[[1]]
        input_res_lon <- median(diff(sort(unique(baseline@data$Lon))))
        input_res_lat <- median(diff(sort(unique(baseline@data$Lat))))

        res_lon <- median(diff(sort(unique(df$Lon))))
        res_lat <- median(diff(sort(unique(df$Lat))))

        if (res_lon != input_res_lon || res_lat != input_res_lat) {
            lons <- sort(unique(baseline@data$Lon))
            lats <- sort(unique(baseline@data$Lat))
            df <- aggregate_to(df, lons, lats)
        }
    }

    field@data <- df
    rm(df)

    renameLayers(field, "Total", "total")

    return(field)
}

#'
#' Read BoM LAI estimates.
#'
#' Read BoM LAI estimates and post-process them to calculate species
#' distribution.
#'
#' @param data_path Path to data directory.
#'
#' @keywords internal
#'
read_bom_tree_dist <- function(data_path) {
    # Read BoM data.
    recurrent <- read_bom_lai(data_path, "rec")
    persistent <- read_bom_lai(data_path, "per")
    total <- read_bom_lai(data_path, "tot")

    # Copy data into a single field.
    renameLayers(total, "mlai", "total")
    total <- copyLayers(recurrent, total, "mlai", "grass_total")
    total <- copyLayers(persistent, total, "mlai", "tree_total")

    # Compute grass and tree fractions.
    total <- layerOp(total, "/", c("grass_total", "total"), "grass_frac")
    total <- layerOp(total, "/", c("tree_total", "total"), "tree_frac")

    rm(persistent)
    rm(recurrent)

    return(total)
}
