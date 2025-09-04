
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
get_site_names_by_coord <- function(lats, lons, all_sites, max_distance = 0.1) {

    # Ensure all_sites is a data.table for the non-equi join.
    all_sites <- data.table::as.data.table(all_sites)
    # Create a data.table from the input coordinates with a unique ID.
    coords_dt <- data.table::data.table(
        temp_id = seq_along(lats),
        lat_in = lats,
        lon_in = lons
    )

    # 1. Create search boundaries for each observation.
    coords_dt[, `:=`(
        min_lat = lat_in - max_distance, max_lat = lat_in + max_distance,
        min_lon = lon_in - max_distance, max_lon = lon_in + max_distance
    )]

    # 2. Use a non-equi join to find candidate sites within the bounding box of
    #    each coordinate.
    candidates <- all_sites[coords_dt,
        on = .(Lat >= min_lat, Lat <= max_lat, Lon >= min_lon, Lon <= max_lon),
        nomatch = 0,
        .(
            site_name = x.Name,
            temp_id = i.temp_id,
            dist = sqrt((x.Lat - i.lat_in)^2 + (x.Lon - i.lon_in)^2)
        )
    ]

    # 3. For each original coordinate, find the single closest candidate.
    if (nrow(candidates) == 0) {
        return(rep(NA_character_, nrow(coords_dt)))
    }
    data.table::setorder(candidates, temp_id, dist)
    nearest_sites <- candidates[, .SD[1], by = temp_id]

    # 4. Join the results back to the original coordinates to ensure correct
    #    order and length.
    result_dt <- nearest_sites[coords_dt, on = .(temp_id)]

    # 5. Set site name to NA if the closest site was still too far (a corner
    #    case in the bounding box).
    result_dt[dist > max_distance, site_name := NA_character_]

    # 6. Return the character vector of site names.
    return(result_dt$site_name)
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
#' Convert a character vector of date strings to Date objects.
#'
#' @param date_strings A character vector of date strings.
#' @param date_fmt The format of the date strings. E.g. "%Y-%m-%d".
#' @return A Date vector.
#' @keywords internal
#'
convert_to_date <- function(date_strings, date_fmt) {
    fasttime_supported_formats <- c("%Y-%m-%d",
                                    "%Y-%m-%d %H:%M:%S",
                                    "%Y-%m-%d %H:%M:%OS")
    have_fasttime <- requireNamespace("fasttime", quietly = TRUE)
    dates <- c()
    if (!is.null(date_fmt) && date_fmt %in% fasttime_supported_formats && have_fasttime) {
        log_diag("Using fasttime to convert dates")
        dates <- fasttime::fastDate(date_strings)
    } else {
        log_diag("Using base R to convert dates. If this is slow, consider installing the fasttime package and converting dates to yyyy-MM-dd.")
        dates <- as.Date(date_strings, format = date_fmt)
    }
    if (NA %in% dates) {
        msg <- "Some dates could not be parsed: "
        msg <- paste(msg, date_strings[which(NA %in% dates)], collapse = ", ")
        if (is.null(date_fmt)) {
            msg <- paste0(msg, "\nNo date format was specified")
        } else {
            msg <- paste0(msg, "\nDate format: ", date_fmt)
        }
        log_error(msg)
    }
    return(dates)
}

get_lat <- function(site_name, all_sites) {
    return(all_sites$Lat[all_sites$Name == site_name])
}

get_lon <- function(site_name, all_sites) {
    return(all_sites$Lon[all_sites$Name == site_name])
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
#' @param detect_ozflux_site Whether to detect OzFlux site names in the data. If TRUE, will lookup site name based on either site column or longitude/latitude. This will normally be FALSE when not filtering by site, but can be manually be set to TRUE if the user knows the data is site-level data and that the site name can therefore be inferred.
#' @param date_fmt The format of the date column. E.g. %Y-%m-%d.
#' @return A list containing firstly the data.table containing the data, and secondly the STAInfo for the data that we have
#' @keywords internal
#'
get_field_csv <- function(source,
                          quant,
                          layers = NULL,
                          target.STAInfo = NULL,
                          file.name = NULL,
                          lat_col = "Lat",
                          lon_col = "Lon",
                          site_col = NULL,
                          time_col = "date",
                          sites = NULL,
                          detect_ozflux_site = !is.null(site_col),
                          date_fmt) {

    quant <- sanitise_variable(quant)

    log_diag("[get_field_csv] Called with source: ", source@dir,
             ", quant: ", quant@id,
             ", layers: ", layers,
             ", target.STAInfo: ", target.STAInfo,
             ", file.name: ", file.name,
             ", lat_col: ", lat_col,
             ", lon_col: ", lon_col,
             ", site_col: ", site_col,
             ", time_col: ", time_col,
             ", sites: ", sites)

    if (is.null(file.name)) {
        log_diag("[get_field_csv] No file name specified. Therefore, using default file name.")
        file_path <- get_file_path(source, quant)
    } else if (!file.exists(file.name)) {
        file_path <- file.path(source@dir, file.name)
        if (!file.exists(file_path)) {
            log_info("[get_field_csv] File ", file.name, " does not exist. Therefore, file name will be inferred from quantity ID.")
            file_path <- get_file_path(source, quant)
        }
    } else {
        file_path <- file.name
    }
    log_diag("[get_field_csv] Reading file ", file_path)

    verbose <- get_log_level() >= get_global("LOG_LEVEL_DIAGNOSTIC")
    dt <- data.table::fread(file_path, verbose = verbose, showProgress = verbose)
    initial_address <- data.table::address(dt)
    log_diag("[get_field_csv] Read ", nrow(dt), " rows. Initial address: ", initial_address)

    if (!is.null(layers)) {
        is_named_layers <- !is.null(names(layers))
        input_col_names <- if (is_named_layers) unname(layers) else layers

        log_diag("[get_field_csv] Filtering by layers: ", paste0(input_col_names, collapse = ", "))
        if (any(!input_col_names %in% colnames(dt))) {
            missing_cols <- input_col_names[!input_col_names %in% colnames(dt)]
            log_error("Some of the layers specified are not present in the file: ", paste0(missing_cols, collapse = ", "))
        }

        cols_to_keep <- c(time_col, input_col_names)
        if (is.null(site_col)) {
            cols_to_keep <- c(cols_to_keep, lat_col, lon_col)
        } else {
            cols_to_keep <- c(cols_to_keep, site_col)
        }
        cols_to_remove <- setdiff(colnames(dt), cols_to_keep)
        if (length(cols_to_remove) > 0) {
            dt[, (cols_to_remove) := NULL]
        }
        log_diag("[get_field_csv] Address after column removal: ", data.table::address(dt))

        if (is_named_layers) {
            log_diag("[get_field_csv] Renaming layers.")
            data.table::setnames(dt, old = unname(layers), new = names(layers))
            log_diag("[get_field_csv] Address after renaming layers: ", data.table::address(dt))
        }
    }

    # Convert date column to Date object using fasttime.
    dt[, (time_col) := convert_to_date(get(time_col), date_fmt)]
    log_diag("[get_field_csv] Address after date conversion: ", data.table::address(dt))

    if (is.null(sites) && !is.null(target.STAInfo)) {
        log_diag("[get_field_csv] No sites specified. Therefore, all sites will be read.")
        sites <- filter_stainfo(target.STAInfo)
        site_names <- paste0(sites$Name, collapse = ", ")
        log_diag("[get_field_csv] Found sites: ", site_names)
    }

    if (!is.null(sites) || detect_ozflux_site) {
        site_names <- paste0(sites, collapse = ", ")
        log_diag("[get_field_csv] Filtering by sites: ", site_names)
        n <- nrow(dt)

        all_sites <- read_ozflux_sites()
        if (is.null(site_col)) {
            site_col <- "Site"
            dt[, (site_col) := get_site_names_by_coord(get(lat_col), get(lon_col), all_sites)]
        } else if (!(site_col %in% colnames(dt))) {
            log_error("Site column ", site_col, " not found in data file ", file_path)
        } else {
            # Data contains a site column. We should create Lon/Lat columns.
            lat_col <- "Lat"
            lon_col <- "Lon"
            idx <- match(dt[[site_col]], all_sites$Name)
            dt[, `:=`(
                Lat = all_sites$Lat[idx],
                Lon = all_sites$Lon[idx]
            )]
        }

        # Now we can filter by site.
        if (!is.null(sites)) {
            dt <- dt[get(site_col) %in% sites]
            log_diag("[get_field_csv] Filtered from ", n, " to ", nrow(dt), " rows")
            log_diag("[get_field_csv] Address after filter_sites: ", data.table::address(dt))
        }
    }

    # Create Year column.
    dt[, Year := as.integer(format(get(time_col), "%Y"))]

    # Detect temporal resolution.
    site_col_effective <- if (!is.null(site_col) && site_col %in% names(dt)) site_col else NULL

    # Count records per site-year
    if (is.null(site_col_effective)) {
        per_site_year <- dt[, .N, by = .(Year)]
    } else {
        per_site_year <- dt[, .N, by = .(Year, site = get(site_col_effective))]
    }

    temporal_resolution <- "Year"
    is_subannual <- any(per_site_year$N > 1, na.rm = TRUE)
    if (is_subannual) {
        log_diag("Data is not annual. Assuming subannual resolution.")

        # Create Day (of year) column. Note: %j is 1-366.
        dt[, Day := as.integer(format(get(time_col), "%j"))]
        temporal_resolution <- "Day"
    }

    # Remove date column.
    dt[, (time_col) := NULL]

    # Rename Lat/Lon columns to their expected names.
    base_cols <- c()
    if (lat_col != "Lat") {
        setnames(dt, old = lat_col, new = "Lat")
    }
    if (lon_col != "Lon") {
        setnames(dt, old = lon_col, new = "Lon")
    }

    # Rename Site to its expected name.
    if (!is.null(site_col_effective) && site_col_effective != "Site") {
        setnames(dt, old = site_col_effective, new = "Site")
    }

    # Reorder columns to: Lon, Lat, Year, Day (if present), then the rest.
    base_cols <- c("Lon", "Lat", "Year")
    if ("Day" %in% names(dt)) base_cols <- c(base_cols, "Day")
    if (!is.null(site_col_effective)) base_cols <- c(base_cols, "Site")
    extra_cols <- setdiff(names(dt), base_cols)
    data.table::setcolorder(dt, c(base_cols, extra_cols))

    if (is.null(target.STAInfo)) {
        target.STAInfo <- new("STAInfo",
                              first.year = min(dt[["Year"]]),
                              last.year = max(dt[["Year"]]),
                              year.aggregate.method = "none",
                              subannual.resolution = temporal_resolution,
                              subannual.original = temporal_resolution)
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
    if (data.table::address(dt) != initial_address) {
        log_warning("[get_field_csv] Address of data table changed during function execution. Run again with log_level = 3 to diagnose. This is not a huge problem, but indicates an inefficiency in the code.")
    }
    return(fld)
}

################################################################################
# CSV FORMAT
################################################################################

#'
#' @description \code{CSV} - a Format object defined here for reading CSV files.
#'
#' @format A \code{\linkS4class{Format}} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @export
#'
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
