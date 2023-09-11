
# Name of the package.
set_global("dave_pkgname", "daveanalysis")

# Ozflux gridcells. This is a data frame with 3 columns (Lon, Lat, Name).
set_global("ozflux_sites", NULL)

#'
#' Get a list of all known ozflux sites.
#'
#' @return Data table containing three columns: Lon, lat, Name.
#' @author Drew Holzworth
#' @keywords internal
#'
read_ozflux_sites <- function() {
	if (is.null(get_global("ozflux_sites"))) {
		pkg_name <- get_global("dave_pkgname")
		gridlist <- system.file("extdata", "ozflux.grid", package = pkg_name)
		gridcells <- read.csv(gridlist, sep = " ", header = FALSE)
		names(gridcells) <- c("Lon", "Lat", "Name")
		set_global("ozflux_sites", gridcells)
	}
	return(get_global("ozflux_sites"))
}

#'
#' Sanitise a single ozflux site name.
#'
#' @param site: Name of the site, or a (lon, lat) tuple.
#'
#' @return Returns the site as a named vector with (Lon, Lat, Name).
#' @keywords internal
#' @author Drew Holzworth
#'
sanitise_ozflux_site <- function(site) {
	if (class(site) == "character") {
		sites <- read_ozflux_sites()
		index <- which(sites$Name == site)
		if (length(index) < 1) {
			log_error("Unknown ozflux site: '", sites, "'")
		}
		if (length(index) > 1) {
			log_error("Site name '", site, "' somehow matches multiple sites")
		}
    	lat <- sites$Lat[index]
    	lon <- sites$Lon[index]
		name <- sites$Name[index]
	} else if (class(site) == "numeric" && length(site) == 2) {
		lon <- as.double(site[1])
		lat <- as.double(site[2])
		name <- NULL
	} else {
		log_error("Unable to interprete site: ", site)
	}
	log_debug("Resolved ozflux site '", name, "': lon=", lon, ", lat=", lat)
	return(list(Lon = lon, Lat = lat, Name = name))
}

#'
#' Sanitise the a site specifier.
#'
#' @param sites: One or more ozflux site identifiers. These may be expressed as
#' site names or (lon, lat) tuples.
#'
#' @return Returns a list of named vectors with (lon, lat, name) elements.
#' @author Drew Holzworth \email{d.holzworth@@westernsydney.edu.au}
#' @keywords internal
#'
sanitise_ozflux_sites <- function(sites) {
	result <- list()
	for (site in sites) {
		result[[length(result) + 1]] <- sanitise_ozflux_site(site)
	}
	return(result)
}

#'
#' Sanitise version inputs for plotting. Returns a list of DGVMTools::Source
#' objects.
#'
#' This is an internal helper function and is not intended for external use.
#'
#' @param sources: May be one of:
#' 					- A single DGVMTools::Source object
#' 					- List of DGVMTools::Source objects
#' 					- A single string providing path to an lpj-guess repository,
#' 					  or a subdirectory of an lpj-guess repository
#'					- A list of paths to lpj-guess repositories
#'
#' @return Returns a list of Sources.
#' @author Drew Holzworth \email{d.holzworth@@westernsydney.edu.au}
#' @keywords internal
#'
sanitise_sources <- function(sources) {
	result <- list()

	if (length(sources) == 1) {
		log_debug("Length of sources is 1. Putting it in a vector...")
		sources <- c(sources)
	}

	for (object in sources) {
		log_debug("Parsing object...")
		if (class(object)[1] == "Source") {
			log_debug("Source is a DGVMTools::Source object")
			result <- append(result, object)
		} else if (class(object) == "character") {
			log_debug("Source is a string")
			name <- basename(object)
			path <- object
			if (dir.exists(file.path(path, "benchmarks", "ozflux"))) {
				fmt <- OZFLUX
				f <- "OZFLUX"
			} else {
				fmt <- DGVMTools::GUESS
				f <- "GUESS"
			}
			log_debug("Defining source with name '", name, "', format '", f
				, "', and path '", path, "'")
			source <- DGVMTools::defineSource(id = name, format = fmt
				, dir = path)
			result <- append(result, source)
		} else {
			msg <- "Input cannot be parsed as a version and will be ignored: "
			log_info("class(object) = ", class(object))
			print(object)
			warning(msg, object)
		}
	}

	if (length(result) >= 1) {
		return(result)
	}

	log_error("No versions were provided")
}

# Attempt to get units given a variable name by looking in the observed file.
get_units <- function(var_name) {
	var_name <- trim_dave(var_name)
	for (var in get_observed_vars()) {
		if (var@id == var_name) {
			return(var@units)
		}
	}
	return("")
}

sanitise_variable <- function(var) {
	if (class(var)[1] == "Quantity") {
		# Input is already a Quantity object.
		return(var)
	}

	if (class(var) == "character") {
		# Input is a string.

		# Case insensitivity...ew! (but it allows people to pass in "LAI")
		units <- get_units(var)
		return(DGVMTools::defineQuantity(tolower(var), var, units))
	}

	log_error("Unable to parse object as source: ", var)
}

#'
#' Sanitise a list of variables provided by the user.
#'
#' @param vars: List of variables provided by the user.
#'
#' @return Returns a list of \seealso{\link{DGVMTools::Quantity}} objects.
#' @keywords internal
#' @author Drew Holzworth
#'
sanitise_variables <- function(vars) {
	result <- list()
	for (var in vars) {
		result <- append(result, sanitise_variable(var))
	}
	return(result)
}

#'
#' Get the canonical name of an ozflux site.
#'
#' Return the name of the specified ozflux site, or raise an error if the site
#' name is invalid.
#'
#' @param spatial_extent_id: Name of the site specified by the user.
#' @param sites: List of valid sites.
#'
#' @return Returns the name of the ozflux site.
#' @keywords internal
#' @author Drew Holzworth
#'
sanitise_spatial_extent_id <- function(spatial_extent_id, sites) {
	if (!(is.character(spatial_extent_id)))
		log_error("Invalid spatial.extent.id: must be a string")
	if (!(spatial_extent_id %in% sites))
		log_error("Invalid spatial_extent: '", spatial_extent_id
			, "'. This must be the name of an ozflux site (ie one of: "
			, paste0(sites, collapse = " "), ")")
	log_debug("site = '", spatial_extent_id, "'")
	return(spatial_extent_id)
}

#'
#'
#' Get the canonical name of an ozflux site.
#'
#' Return the name of the ozflux site specified by the given spatial extent.
#' Raises an error if the site name is invalid.
#'
#' @param spatial_extent: The spatial extent representing the site, supplied as
#'                        a raster::extent object or an object from which a
#'                        raster::extent object can be derived - eg. a Raster*
#'                        object or another Field object.
sanitise_spatial_extent <- function(spatial_extent, sites) {
	# TBI
	log_error("OZFLUX support for spatial.extent is not yet implemented. "
		, "Either setup and use a GUESS source, or specify spatial.extent.id "
		, "instead. If specified, spatial.extent.id should be the name of an "
		, "ozflux site.")
}
