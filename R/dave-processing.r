# Name of the ozflux observed data file included this package's inst/data dir.
set_global("obs_file", "ozflux-obs.nc")

# Name of the observed data layer.
set_global("obs_lyr", "observed")

# Import required functions from methods package
#' @importFrom methods is new callGeneric

# Tolerance used for floating point comparisons between spatial coordinates when
# merging data from different sources. Two grid points are considered identical
# if their latitude and longitude are different by less than this amount.
set_global("merge_tol", 0.01)

# Same as merge_tol, expressed as number of decimal places.
set_global("merge_ndp", ceiling(abs(log10(get_global("merge_tol")))))

#'
#' Check if the given layer exists in the given source.
#'
#' @param source A [DGVMTools::Source] object
#' @param layer Name of a layer (e.g. "mlai")
#'
#' @keywords internal
#'
has_output <- function(source, layer) {
    path <- file.path(source@dir, paste0(layer, ".out"))
    gz_path <- paste0(path, ".gz")
    return(file.exists(path) || file.exists(gz_path))
}

trim_dave <- function(text) {
    return(gsub("dave_", "", text))
}

get_observed_source <- function() {
    obs_dir <- system.file("data", package = get_global("dave_pkgname"))
    log_debug("Loading observed source from dir: '", obs_dir, "'...")
    return(DGVMTools::defineSource(get_global("obs_lyr"), "Ozflux", format = DGVMTools::NetCDF
                                   , dir = obs_dir))
}

get_layer_names <- function(
        var,
        nvar,
        layers,
        source_name,
        nlayer = length(layers)) {

    if (nvar == 1) {
        if (nlayer == 1) {
            return(source_name)
        } else {
            return(paste0(source_name, "_", layers))
        }
    } else {
        id <- trim_dave(var@id)
        if (nlayer == 1) {
            return(paste0(source_name, "_", id))
        } else {
            return(paste0(source_name, "_", id, "_", layers))
        }
    }
}

get_layer_names_for_sources <- function(
        var,
        nvar,
        layers,
        sources,
        nlayer = length(layers)) {
    if (length(sources) == 1 && nvar == 1) {
        return(layers)
    }

    layer_names <- c()
    for (source in sources) {
        lyrs <- get_layer_names(var, nvar, layers, source@name, nlayer = nlayer)
        layer_names <- c(layer_names, lyrs)
    }
    return(layer_names)
}

get_aggregate_layers <- function() {
    return(c("total", "Total", "mean", "Mean"))
}

get_ignored_layers <- function() {
    return(c(
        "year",
        "month",
        "day",
        "timestep",
        "patch",
        "stand"
    ))
}

get_default_layers <- function(source, var, sites = NULL) {
    available <- available_layers_ozflux(source, var, sites)
    if (length(available) < 1) {
        log_error("Unknown layers available for quantity ", var@id)
    }

    to_try <- get_aggregate_layers()
    for (l in to_try) {
        if (l %in% available) {
            # Note: get_field_ozflux() previously normalised Total -> total.
            return(l)
        }
    }
    last_layer <- available[[length(available)]]
    log_warning("Unsure which layers to plot for variable ", var@name
                , "; therefore the last layer (", last_layer
                , ") will be plotted.")
    return(last_layer)
}

# Get the plottable layers present in the given data, ignoring layers such as
# year, day, total, etc.
get_layers <- function(
        source,
        var,
        sites = NULL,
        ignore_total = TRUE,
        ignore_mean = TRUE) {

    ignored_layers <- get_ignored_layers()
    if (ignore_total) {
        ignored_layers <- c(ignored_layers, "total", "Total", "TOTAL")
    }
    if (ignore_mean) {
        ignored_layers <- c(ignored_layers, "mean", "Mean", "MEAN")
    }
    if ("Name" %in% names(sites)) {
        sites <- sites[["Name"]]
    }
    available <- available_layers_ozflux(source, var, sites)
    return(setdiff(available, ignored_layers))
}

is_leap <- function(year) {
    (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
}

get_obs_layer <- function(quant) {
    # The ozflux output file names from lpj-guess DAVE are prefixed with
    # "dave_" but the variable names in the observed data file don't have
    # this prefix.
    name <- tolower(trim_dave(quant@id))

    # If using the registry, check if any reader has a mapping for this variable
    if (requireNamespace("methods", quietly = TRUE)) {
        var_id <- name
        readers <- find_readers_for_var(var_id)
        if (length(readers) > 0) {
            # Use the first available reader's mapping
            reader_id <- names(readers)[1]
            reader <- readers[[reader_id]]
            if (var_id %in% names(reader@model_var_mapping)) {
                # Return the original variable ID, not the mapped one,
                # since we'll use the mapping when reading the data
                return(var_id)
            }
        }
    }

    # Fall back to the original behavior
    if (name == "live_biomass") {
        return("live_biomass")
    }
    if (name == "dead_biomass") {
        return("dead_biomass")
    }
    return(name)
}

#'
#' Read data for the specified variable.
#'
#' @param sources The data sources from which to read (see: [sanitise_source]).
#' @param vars The variables to be read (see: [sanitise_variable]).
#' @param sites The ozflux sites for which data will be read (see:
#'               [sanitise_ozflux_sites]).
#' @param layers Names of layers to be read.
#' @param correct_leaps Never set to TRUE unless you know what you're doing.
#' @param show_all_observations If true, all observations will be returned (ie
#'                               the predicted data may contain NA values). If
#'                               false, only the observations which have a
#'                               matching prediction will be returned.
#' @param show_all_predictions If true, all predictions will be returned (ie
#'                              the observed data may contain NA values). If
#'                              false, only the predictions which have a
#'                              matching observation will be returned.
#' @keywords internal
#'
#' @return A single [DGVMTools::Field] with a layer of observations, and one
#'         layer for each source specified in sources.
#' @export
#'
#' @seealso [ObservationReader], [find_readers_for_var], [register_reader]
#'
read_data <- function(sources
                       , vars
                       , sites = NULL
                       , layers = NULL
                       , correct_leaps = FALSE
                       , show_all_observations = TRUE
                       , show_all_predictions = TRUE
                       , read_obs = TRUE) {

    log_debug("[read_data] Sanitising input sources...")
    sources <- sanitise_sources(sources)
    log_debug("[read_data] Sanitising input quantities...")
    vars <- sanitise_variables(vars)
    log_debug("[read_data] Sanitising input sites...")
    sites <- sanitise_ozflux_sites(sites, sources[[1]]@dir)

    # if (is.data.frame(site)) {
    #     if (nrow(site) == 1) {
    #         site <- site[1, ]
    #     } else {
    #         site <- NULL
    #     }
    # }

    data <- NULL
    nvar <- length(vars)
    original_layer <- layers

    for (var in vars) {

        # Get the observation layer name for this variable
        var_id <- trim_dave(var@id)
        obs_lyr <- var_id

        layers <- original_layer
        if (is.null(layers)) {
            layers <- obs_lyr
        }

        # Find readers that support this variable using the registry
        log_diag("Searching for readers which contain variable ", var_id)
        readers <- find_readers_for_var(var_id)

        if (read_obs && length(readers) > 0) {
            log_debug("Found ", length(readers), " reader(s) for variable ", var_id)

            # Process all readers that support this variable
            for (reader_id in names(readers)) {
                reader <- readers[[reader_id]]

                log_debug("Reading field ", var_id, " using reader ", reader@src@id, "...")

                # Read the observation data using the reader's read_func
                # Use direct slot access to avoid S4 method dispatch issues
                obs_field <- reader@read_func(var_id, sites = sites$Name)

                # For now, use reader_id as the layer name. Should revisit this.
                if (is.null(data)) {
                    data <- obs_field
                    if (layers %in% names(data)) {
                        DGVMTools::renameLayers(data, layers, reader_id)
                    } else {
                        # Get first layer that's not "Site"
                        lyrs <- names(data)[names(data) != "Site"]
                        if (length(lyrs) > 0) {
                            DGVMTools::renameLayers(data, lyrs[1], reader_id)
                        }
                    }
                } else {
                    layers_old <- layers
                    if (!(layers_old %in% names(obs_field))) {
                        layers_old <- names(obs_field)[names(obs_field) != "Site"][1]
                    }
                    data <- DGVMTools::copyLayers(obs_field, data, layers_old
                        , new.layer.names = reader_id
                        , tolerance = get_global("merge_tol"), keep.all.from = show_all_observations
                        , keep.all.to = show_all_observations)#, allow.cartesian = TRUE
                }
            }
        }

        # If no readers were found, log a warning
        if (length(readers) == 0) {
            log_warning("No observation readers found for variable '", var_id, "'")
        }

        layers <- original_layer
        if (is.null(layers)) {
            layers <- get_default_layers(sources[[1]], var, sites$Name)
        }
        log_diag("Reading data for layers: ", layers)

        # Read outputs of this variable from each configured source.
        num_decimal_places <- get_global("merge_ndp")
        for (source in sources) {
            # fixme: not all of the dave output files have a total column, and even
            # if they do this is a rather ugly workaround for the fact that some
            # are individual-level outputs while some are patch-level outputs.
            args <- list()
            args$source <- source
            args$layers <- layers
            args$quant <- var
            if (source@format@id == "OZFLUX") {
                args$decimal.places <- num_decimal_places
            }
            args$verbose <- FALSE
            if (!is.null(sites)) {
                if (is.data.frame(sites) && source@format@id == "OZFLUX") {
                    args$sites <- sites
                } else {
                    args$spatial.extent.id <- sites$Name
                    args$spatial.extent <- c(sites$Lon, sites$Lat)
                }
            }
            predictions <- do.call(DGVMTools::getField, args)
            if (is_known_quantity(var@id)) {
                predictions@quant <- var
            }
            if (nrow(predictions@data) == 0) {
                stop("Failed to read data for source ", source@id)
            }

            log_debug("Successfully read data from source ", source@name
                , " for variable ", var@name)

            pn <- names(predictions@data)
            if ("Year" %in% pn && "Day" %in% pn && correct_leaps) {
                log_diag("Fixing day of year to account for leap days...")
                # Use with= to avoid 'no visible binding' lint warnings
                predictions@data[is_leap(predictions@data$Year) & predictions@data$Day > 59, Day := predictions@data$Day + 1]
                log_debug("Successfully performed leap year conversion")
            }

            layer_names <- get_layer_names(var, nvar, layers, source@name)
            if (is.null(data) || nrow(data@data) == 0) {
                # IE no observations
                data <- predictions
                if (length(sources) > 1 || length(vars) > 1) {
                    DGVMTools::renameLayers(data, layers, layer_names)
                }
            } else {
                nr <- nrow(data@data)
                data <- DGVMTools::copyLayers(predictions, data, layers
                    , new.layer.names = layer_names
                    , tolerance = get_global("merge_tol"), keep.all.from = show_all_predictions
                    , keep.all.to = show_all_observations)#, allow.cartesian = TRUE
                # Note: if using a "non-standard" ozflux site (such as Abisko),
                # we will have observations for all standard sites but not this
                # one. If show_all_observations is TRUE, copyLayers() will emit
                # a data frame containing NA lat/lon values for the predictions.
                #
                # That needs to be treated the same as an empty data frame.
                if ( (nrow(data@data) == 0 && nr > 0 && nrow(predictions@data) > 0) ||
                     any(is.na(data@data$Lon)) ) {
                    # No observations for this site.
                    log_warning("No observations found for site ", sites$Name)
                    data <- predictions
                    if (length(source) > 1) {
                        DGVMTools::renameLayers(data, layers, layer_names)
                    }
                }
                log_debug("Successfully merged data from source ", source@name
                    , " for variable ", var@name)
            }
        }
    }

    # Remove day column if all values are last day of year.
    if ("Day" %in% names(data@data) && all(data@data$Day == 364)) {
        data@data$Day <- NULL
    }

    log_debug("Successfully read all data")

    return(data)
}

#'
#' Extract data for the specified gridcell.
#'
#' @param data The input data as obtained from [read_data]
#' @param lat Latitude of the gridcell
#' @param lon Longitude of the gridcell.
#' @param site_name Name of the ozflux site (optional).
#'
#' @return Returns a [DGVMTools::Source] object containing data only for the
#' specified location.
#' @keywords internal
#'
get_gridcell <- function(data, lat, lon, site_name = NULL) {
  log_debug("Extracting data for gridcell", ifelse(!is.null(site_name)
      , paste0(" ", site_name), ""), " (", lon, ", ", lat, ")")
  gc <- DGVMTools::selectGridcells(data, c(lon, lat)
      , spatial.extent.id = site_name, decimal.places = get_global("merge_ndp"))
  log_debug("Successfully extracted ", nrow(gc@data), " rows of data for gridcell ", site_name, ")")
  return(gc)
}

#'
#' Get the list of temporal plottable variables from the observed .nc file.
#'
#' @return Returns a list of [DGVMTools::Quantity] objects.
#' @keywords internal
#'
get_observed_vars <- function() {
    # fixme - should get this from the observed .nc file.

    return(list(
      DGVMTools::defineQuantity("gpp", "GPP", "gC m^-2 day^-1")
    , DGVMTools::defineQuantity("resp", "Respiration", "gC m^-2 day^-1")
    , DGVMTools::defineQuantity("nee", "NEE", "gC m^-2 day^-1")
    , DGVMTools::defineQuantity("aet", "ET", "mm day^-1")
    , DGVMTools::defineQuantity("lai", "LAI", "m^2 m^-2")
    , DGVMTools::defineQuantity("aboveground_tree_biomass", "Above-Ground Live Tree Biomass", "kg/m2")
    , DGVMTools::defineQuantity("dead_biomass", "??Above-Ground Dead Biomass??", "kg/m2")
    , DGVMTools::defineQuantity("height", "Height", "m")
    , DGVMTools::defineQuantity("diameter", "Diameter", "m")
    ))
}

get_ylim <- function(data, common_yaxis = TRUE) {
    if (!common_yaxis) {
        return(NULL)
    }
    ymin <- 1e300
    ymax <- -1e300
    for (lyr_name in names(data)) {
        v <- data@data[which(!is.na(data@data[[lyr_name]])), ][[lyr_name]]
        ymin <- min(ymin, min(v))
        ymax <- max(ymax, max(v))
    }
    return(c(ymin, ymax))
}

