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
	return(defineSource("obs", "Ozflux", format = NetCDF, dir = obs_dir))
}

#'
#' Read data for the specified variable.
#'
#' @param var: The variable (a DGVMTools::Quantity).
#' @param sources: Input sources (\seealso{\link{sanitise_sources}}
#'
#' @return A single DGVMTools::Field with a layer of observations, and one layer
#' for each source specified in sources.
#' @author Drew Holzworth
#' @keywords internal
#'
read_data <- function(var, sources) {
	sources <- sanitise_sources(sources)
	var <- sanitise_variable(var)

	# The ozflux output file names from lpj-guess DAVE are prefixed with "dave_"
	# but the variable names in the observed data file don't have this prefix.
	lyr_name <- gsub("dave_", "", var@id)

	# Read all observations for this variable.
	obs_source <- read_observed_source()
	log_debug("Reading field ", lyr_name, " from observed source...")
	data <- getField(source = obs_source, quant = lyr_name, layers = lyr_name
	, file.name = get_global("obs_file")
	, verbose = get_global("log_level") >= get_global("LOG_LEVEL_DEBUG"))

	log_debug("Successfully read observed data for variable ", var@name)

	# Rename the (for now only) data layer from lyr_name to "observed", to avoid
	# conflicts with the prediction layers with which we're about to read/merge.
	renameLayers(data, lyr_name, get_global("obs_lyr"))

	# Read outputs of this variable from each configured source.
	for (source in sources) {
		# fixme: not all of the dave output files have a total column, and even
		# if they do this is a rather ugly workaround for the fact that some
		# are individual-level outputs while some are patch-level outputs.
		col <- "total"
		predictions <- getField(source = source, layers = col, quant = var@id)
		log_debug("Successfully read   data from source ", source@name
			, " for variable ", var@name)
		data <- copyLayers(predictions, data, col, new.layer.names = source@name
		, tolerance = get_global("merge_tol"), keep.all.from = FALSE
		, keep.all.to = FALSE)
		log_debug("Successfully merged data from source ", source@name
			, " for variable ", var@name)
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
  gc <- selectGridcells(data, c(lon, lat), spatial.extent.id = site_name
      , decimal.places = get_global("merge_ndp"))
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
	  defineQuantity("dave_gpp", "GPP", "gC m^-2 day^-1")
	, defineQuantity("dave_resp", "Respiration", "gC m^-2 day^-1")
	, defineQuantity("dave_nee", "NEE", "gC m^-2 day^-1")
	, defineQuantity("dave_et", "ET", "mm day^-1")
	, defineQuantity("dave_lai", "LAI", "m^2 m^-2")
	# , defineQuantity("cmass", "AboveGround Biomass", "kgC/m2")
	))
}
