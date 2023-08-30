# Name of the ozflux observed data file included this package's extdata.
set_global("obs_file", "ozflux-obs.nc")

# Name of the observed data layer.
set_global("obs_lyr", "observed")

# Tolerance used for floating point comparisons between spatial coordinates when
# merging data from different sources. Two grid points are considered identical
# if their latitude and longitude are different by less than this amount.
set_global("merge_tol", 0.01)

# Same as merge_tol, expressed as number of decimal places.
set_global("merge_ndp", ceiling(abs(log10(get_global("merge_tol")))))

read_observed_source <- function() {
	obs_dir <- system.file("extdata", package = get_global("dave_pkgname"))
	log_debug("Loading observed source...")
	return(DGVMTools::defineSource("obs", "Ozflux", format = DGVMTools::NetCDF
		, dir = obs_dir))
}

get_layer_name <- function(layer, source) {
	return(paste0(layer, "_", source@name))
}

#'
#' Read data for the specified variable.
#'
#' @param var: The variable (a DGVMTools::Quantity).
#' @param sources: Input sources (\seealso{\link{sanitise_sources}}
#' @param site: Optional name of the ozflux site for which data should be read.
#'              If NULL, data for all sites will be returned.
#' @param layers: Names of layers to be read.
#'
#' @return A single DGVMTools::Field with a layer of observations, and one layer
#' for each source specified in sources.
#' @author Drew Holzworth
#' @keywords internal
#'
read_data <- function(
	var,
	sources,
	site = NULL,
	layers = NULL) {

	sources <- sanitise_sources(sources)
	var <- sanitise_variable(var)

	# The ozflux output file names from lpj-guess DAVE are prefixed with "dave_"
	# but the variable names in the observed data file don't have this prefix.
	lyr_name <- gsub("dave_", "", var@id)

	if (is.null(layers)) {
		layers <- lyr_name
	}

	# Read all observations for this variable.
	has_obs <- lyr_name %in% lapply(get_observed_vars(), function(x) x@id)
	if (has_obs) {
		obs_source <- read_observed_source()
		log_debug("Reading field ", lyr_name, " from observed source...")
		data <- DGVMTools::getField(source = obs_source, quant = lyr_name
			, layers = layers, file.name = get_global("obs_file")
			, verbose = get_global("log_level") >= get_global("LOG_LEVEL_DEBUG"))

		log_debug("Successfully read observed data for variable ", var@name)

		# Rename the (for now only) data layer from lyr_name to "observed", to avoid
		# conflicts with the prediction layers with which we're about to read/merge.
		DGVMTools::renameLayers(data, lyr_name, get_global("obs_lyr"))
	} else {
		log_warning("No observed data found for variable '", lyr_name, "'")
	}

	# Read outputs of this variable from each configured source.
	num_decimal_places <- get_global("merge_ndp")
	if (is.null(layers)) {
		layers <- "total"
	}
	for (source in sources) {
		# fixme: not all of the dave output files have a total column, and even
		# if they do this is a rather ugly workaround for the fact that some
		# are individual-level outputs while some are patch-level outputs.
		args <- list()
		args$source <- source
		args$layers <- layers
		args$quant <- var@id
		args$decimal.places <- num_decimal_places
		if (!is.null(site)) {
			args$spatial.extent.id <- site$name
			args$spatial.extent <- c(site$lon, site$lat)
		}
		predictions <- do.call(DGVMTools::getField, args)

		log_debug("Successfully read   data from source ", source@name
			, " for variable ", var@name)
		if (length(layers) == 1) {
			layer_names <- source@name
		} else {
			layer_names <- get_layer_name(layers, source)
		}
		if (has_obs) {
			data <- DGVMTools::copyLayers(predictions, data, layers
				, new.layer.names = layer_names
				, tolerance = get_global("merge_tol"), keep.all.from = FALSE
				, keep.all.to = FALSE)
			log_debug("Successfully merged data from source ", source@name
				, " for variable ", var@name)
		} else {
			data <- predictions
			DGVMTools::renameLayers(data, layer_names)
		}
	}

	log_debug("Successfully read all data")

	return(data)
}

#'
#' Extract data for the specified gridcell.
#'
#' @param data: The input data as obtained from \seealso{\link{read_data}}
#' @param lat: Latitude of the gridcell
#' @param lon: Longitude of the gridcell.
#' @param site_name: Name of the ozflux site (optional).
#'
#' @return Returns a \seealso{\link{DGVMTools::Source}} object containing data
#' only for the specified location.
#' @author Drew Holzworth
#' @keywords internal
#'
get_gridcell <- function(data, lat, lon, site_name = NULL) {
  log_debug("Extracting data for gridcell", ifelse(!is.null(site_name)
  	, paste0(" ", site_name), ""), " (", lon, ", ", lat, ")")
  gc <- DGVMTools::selectGridcells(data, c(lon, lat)
  	, spatial.extent.id = site_name, decimal.places = get_global("merge_ndp"))
  log_debug("Successfully extracted gridcell data")
  return(gc)
}

#'
#' Get the list of temporal plottable variables from the observed .nc file.
#'
#' @return Returns a list of \seealso{\link{DGVMTools::Quantity}} objects.
#' @author Drew Holzworth
#' @keywords internal
#'
get_observed_vars <- function() {
	# fixme - should get this from the observed .nc file.
	return(list(
	  DGVMTools::defineQuantity("gpp", "GPP", "gC m^-2 day^-1")
	, DGVMTools::defineQuantity("resp", "Respiration", "gC m^-2 day^-1")
	, DGVMTools::defineQuantity("nee", "NEE", "gC m^-2 day^-1")
	, DGVMTools::defineQuantity("et", "ET", "mm day^-1")
	, DGVMTools::defineQuantity("lai", "LAI", "m^2 m^-2")
	# , defineQuantity("cmass", "AboveGround Biomass", "kgC/m2")
	))
}

get_ylim <- function(data) {
	ymin <- 1e300
	ymax <- -1e300
    for (lyr_name in names(data)) {
        v <- data@data[which(!is.na(data@data[[lyr_name]])), ][[lyr_name]]
        ymin <- min(ymin, min(v))
        ymax <- max(ymax, max(v))
    }
    return(c(ymin, ymax))
}
