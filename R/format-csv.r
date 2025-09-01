
#'
#' List all CSV *.csv files in a source directory
#'
#' Simply lists all CSV output variables (stored as .csv files) available in a
#' directory.
#'
#' @param source A \code{\linkS4class{Source}} containing the meta-data about
#' the CSV source.
#' @param names Logical, if TRUE return a character vector of names of
#' available quantities, if FALSE return a list of the actual Quantities.
#'
#' @return A list of all the .csv files present, with the ".csv" removed.#'
#' @export
#'
available_quantities_csv <- function(source, names) {
    directory <- source@dir

    # First get the list of *.csv files present.
    files <- list.files(directory, ".csv$")
    files <- append(files, list.files(directory, ".csv.gz$"))

    # Now strip the .csv file extension out the get the variable name.
    vars <- unlist(lapply(files, FUN = function(f) gsub("\\.csv$", "", f)))

    return(vars)
}

#'
#' List the available layers (columns) in a CSV file.
#'
#' @param source A \code{\linkS4class{Source}} containing the meta-data about
#' the CSV source.
#' @param quant A \code{\linkS4class{Quantity}} object to specify what quantity
#' should be opened.
#'
#' @return A character vector of layer names available for this quantity.
#' @export
#'
available_layers_csv <- function(source, quant) {
    file_path <- get_file_path(source, quant)
    log_diag("[available_layers_csv] Reading file ", file_path)
    dt <- data.table::fread(file_path, nrows = 0)
    log_diag("[available_layers_csv] Read ", ncol(dt), " columns")
    return(colnames(dt))
}

#'
#' Get the name of a site from its latitude and longitude.
#'
#' @param lat The latitude of the site
#' @param lon The longitude of the site
#' @param all_sites A data.table containing the latitude, longitude and name of all sites
#' @param epsilon Optional tolerance for floating point comparison (default: 1e-6)
#' @param max_distance Maximum allowable distance between coordinates (default: 0.1 degrees)
#'   If the closest site is farther than this distance, returns NULL
#'
#' @keywords internal
#' @return Character string with site name or NULL if no site is within max_distance
#'
get_site_name <- function(lat, lon, all_sites, epsilon = 1e-6, max_distance = 0.1) {
    all_sites <- as.data.table(all_sites)

    # First try with epsilon comparison
    result <- all_sites[abs(Lat - lat) < epsilon & abs(Lon - lon) < epsilon, .(Name)]

    # If we found a match with epsilon
    if (nrow(result) > 0) {
        return(result[[1]])
    }

    # If no match with epsilon, find the closest site by Euclidean distance
    all_sites[, dist := sqrt((Lat - lat)^2 + (Lon - lon)^2)]
    closest <- all_sites[order(dist)][1]

    # Check if the closest site is within the maximum allowed distance
    if (closest$dist > max_distance) {
        # Remove the temporary distance column
        all_sites[, dist := NULL]
        log_warning("Site ", lat, ", ", lon, " is not close enough to any site in the gridlist (maximum distance: ", max_distance, ")")
        return(NULL)  # No site is close enough
    }

    # Get the name of the closest site
    closest_site_name <- closest$Name

    # Remove the temporary distance column
    all_sites[, dist := NULL]

    return(closest_site_name)
}

#'
#' Get the file name for a given quantity
#'
#' @param source A \code{\linkS4class{Source}} containing the meta-data about
#' the CSV source.
#' @param quant A \code{\linkS4class{Quantity}} object to specify what quantity
#' should be opened.
#'
#' @return A character string holding the name of the file.
#' @keywords internal
#'
get_file_path <- function(source, quant) {
    return(file.path(source@dir, paste0(quant@id, ".csv")))
}

#'
#' Filter a data.table by site
#'
#' @param dt A data.table to filter
#' @param sites A character vector of site names to keep
#' @param lat_col The name of the column containing latitude data
#' @param lon_col The name of the column containing longitude data
#' @param site_col The name of the column containing site names
#'
#' @keywords internal
#' @return A data.table filtered by site
#'
filter_sites <- function(dt, sites, lat_col, lon_col, site_col) {
    # Checked by caller - this should never happen.
    if (is.null(sites)) log_error("No sites specified")

    # Right now, the data table could have either lat/lon columns, or a site
    # name column. The first step is to get it into a consistent state, by
    # adding a site name column.
    if (is.null(site_col)) {
        site_col <- "site"
        all_sites <- read_ozflux_sites()
        dt[, site_col := get_site_name(lat_col, lon_col, all_sites)]
    }

    # Now we can filter by site.
    dt <- dt[site_col %in% sites]
    return(dt)
}

#'
#' Get the list of sites defined by a spatial extent.
#'
#' @param target.STAInfo An STAInfo object defining the spatial-temporal-annual extent over which we want the data
#'
#' @keywords internal
#' @return A character vector of site names
#'
filter_stainfo <- function(target_stainfo) {
    sites <- read_ozflux_sites()
    spatial_extent_id <- target_stainfo@spatial.extent.id
    spatial_extent <- target_stainfo@spatial.extent
    if (!is.null(spatial_extent_id) && length(spatial_extent_id) > 0) {
        return(sanitise_spatial_extent_id(spatial_extent_id, sites))
    } else if (!is.null(spatial_extent) && spatial_extent != FALSE) {
        # For some reason, FALSE gets passed in for spatial.extent if no value
        # is given.
        return(sanitise_spatial_extent(spatial_extent, sites))
    } else {
        log_debug("spatial.extent and spatial.extent.id not specified.")
        return(NULL)
    }
}

#'
#' Get a Field for CSV
#'
#'
#' An internal function that reads data from a CSV .csv file.
#'
#' @param source A \linkS4class{Source} containing the meta-data about the CSV source
#' @param quant A Quantity object to specify what quantity should be opened.
#' @param layers A character string (or a vector of character strings) specifying which variables from the CSV file are to be read.
#' NULL (default) means read all.
#' @param target.sta.info An STAInfo object defining the spatial-temporal-annual extent over which we want the data
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is automatically generated
#' @param lat_col The name of the column containing latitude data
#' @param lon_col The name of the column containing longitude data
#' @param site_col The name of the column containing site names. Must specify this OR lat_col and lon_col.
#' @param time_col The name of the column containing time data
#' @param sites A character vector of site names to read. If NULL, read all sites.
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.table containing the data, and secondly the STAInfo for the data that we have
#' @keywords internal
#'
get_field_csv <- function(source,
                         quant,
                         layers = NULL,
                         target.STAInfo,
                         file.name,
                         verbose = FALSE,
                         lat_col = "Lat",
                         lon_col = "Lon",
                         site_col = NULL,
                         time_col = "date",
                         sites = NULL) {

    log_diag("[get_field_csv] Called with source: ", source@dir,
             ", quant: ", quant@id,
             ", layers: ", layers,
             ", target.STAInfo: ", target.STAInfo@spatial.extent.id,
             ", file.name: ", file.name,
             ", verbose: ", verbose,
             ", lat_col: ", lat_col,
             ", lon_col: ", lon_col,
             ", site_col: ", site_col,
             ", time_col: ", time_col,
             ", sites: ", sites)

    file_path <- get_file_path(source, quant)
    log_diag("[get_field_csv] Reading file ", file_path)

    verbose <- get_log_level() >= get_global("LOG_LEVEL_DIAGNOSTIC")
    dt <- data.table::fread(file_path, verbose = verbose, showProgress = verbose)
    log_diag("[get_field_csv] Read ", nrow(dt), " rows")

    if (!is.null(layers)) {
        log_diag("[get_field_csv] Filtering by layers: ", paste0(layers, collapse = ", "))
        if (any(!layers %in% colnames(dt))) {
            log_error("Some of the layers specified are not present in the file: ", paste0(layers, collapse = ", "))
        }
        layers_to_keep <- c(time_col)
        if (is.null(site_col)) {
            layers_to_keep <- c(layers_to_keep, lat_col, lon_col)
        } else {
            layers_to_keep <- c(layers_to_keep, site_col)
        }
        layers_to_keep <- c(layers_to_keep, layers)
        dt <- dt[, layers_to_keep]
    }

    if (is.null(sites)) {
        log_diag("[get_field_csv] No sites specified. Therefore, all sites will be read.")
        sites <- filter_stainfo(target.STAInfo)
        log_diag("[get_field_csv] Found sites: ", paste0(sites$Name, collapse = ", "))
    }

    if (!is.null(sites)) {
        log_diag("[get_field_csv] Filtering by sites: ", paste0(sites$Name, collapse = ", "))
        nrow <- nrow(dt)
        dt <- filter_sites(dt, sites, lat_col, lon_col, site_col)
        log_diag("[get_field_csv] Filtered from ", nrow, " to ", nrow(dt), " rows")
    }

    field_id <- DGVMTools::makeFieldID(
        source = source,
        quant.string = quant@id,
        sta.info = target.STAInfo
    )

    fld <- new("Field",
        id = field_id,
        data = dt,
        quant = quant,
        source = source,
        target.STAInfo)
    return(fld)
}

################################################################################
# CSV FORMAT
################################################################################

#' @description \code{CSV} - a Format object defined here for reading CSV files.
#'
#' @format A \code{\linkS4class{Format}} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @export
CSV <- new("Format",
    # Unique ID.
    id = "CSV",

    # Function to list all quantities available in a run.
    availableQuantities = available_quantities_csv,

    # Function to read a field.
    getField = get_field_csv,

    # Default global layers - empty list.
    predefined.layers = list(),

    # The CSV format is too flexible to define quantities in advance.
    quantities = list()
)
