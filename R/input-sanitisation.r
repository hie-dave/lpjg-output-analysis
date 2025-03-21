
# Name of the package.
set_global("dave_pkgname", "daveanalysis")

# Ozflux gridcells. This is a data frame with 3 columns (Lon, Lat, Name).
set_global("ozflux_sites", NULL)

call_foreach <- function(x, fun, ignore_null = TRUE) {
	if (is.vector(x) || is.list(x)) {
		result <- lapply(x, fun)
	} else {
		result <- list(fun(x))
	}
	if (ignore_null) {
		result <- Filter(Negate(is.null), result)
	}
	return(result)
}

#'
#' Get a list of all known ozflux sites.
#'
#' @return Data table containing three columns: Lon, lat, Name.
#' @export
#'
read_ozflux_sites <- function() {
	if (is.null(get_global("ozflux_sites"))) {
		pkg_name <- get_global("dave_pkgname")
		gridlist <- system.file("data", "ozflux.grid", package = pkg_name)
		log_debug("Reading gridlist '", gridlist, "''...")
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
#'
sanitise_ozflux_site <- function(site) {
	log_debug("Sanitising ozflux site: ", site)
	if (class(site) == "character") {
		log_debug("Site is a character vector. Probably. ¿Maybe(?).")
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
		# exact coords printed below
		log_debug("Site is numeric. Let's assume it's in lon, lat order")
		name <- NULL
	} else {
		log_error("Unable to interprete site: ", site)
	}
	log_diag("Resolved ozflux site '", name, "': lon=", lon, ", lat=", lat)
	return(list(Lon = lon, Lat = lat, Name = name))
}

#'
#' Sanitise the a site specifier.
#'
#' @param sites: One or more ozflux site identifiers. If NULL, all sites will be
#' used. The sites may be expressed as:
#' - Site names
#' - (lon, lat) tuples
#' - A dataframe with (Lon, Lat, Name) columns, and one row per site
#'
#' @return Returns a dataframe with (Lon, Lat, Name) columns, and one row per
#' site.
#' @keywords internal
#'
sanitise_ozflux_sites <- function(sites) {
	# Return a dataframe of all sites if sites is NULL.
	log_debug("Sanitising ozflux sites...")
	if (is.null(sites)) {
		log_debug("No sites were provided, so all sites will be used")
		return(read_ozflux_sites())
	}

	if (is.data.frame(sites) && ncol(sites) == 3 && "Name" %in% names(sites)
			&& "Lon" %in% names(sites) && "Lat" %in% names(sites)) {
		log_debug("Sites is a dataframe with Lat/Lon columns. This is fine")
		return(sites)
	}

	log_debug("Not sure what sites is, so let's assume it's a list")
	result <- call_foreach(sites, sanitise_ozflux_site)
	if (length(result) < 1) {
		log_error("No sites were specified. To plot all sites, use NULL")
	}

	# Convert to dataframe.
	result <- do.call(rbind, lapply(result, as.data.frame))
	rownames(result) <- NULL
	result <- as.data.frame(result)

	return(result)
}

#'
#' Sanitise version inputs for plotting. Returns a list of [DGVMTools::Source]
#' objects.
#'
#' This is an internal helper function and is not intended for external use.
#'
#' @param source: May be one of:
#' - A [[DGVMTools::Source]] object
#' - Path to an lpj-guess repository
#' - Path to a directory containing LPJ-Guess output files
#'
#' @return Returns a list of [[DGVMTools::Source]] objects.
#' @keywords internal
#'
sanitise_source <- function(source) {
	if (class(source)[1] == "Source") {
		log_debug("Source is a DGVMTools::Source object")
		return(source)
	} else if (class(source) == "character") {
		log_debug("Source is a string")
		name <- basename(source)
		path <- source
		if (dir.exists(file.path(path, "benchmarks", "ozflux"))) {
			fmt <- OZFLUX
			f <- "OZFLUX"
		} else {
			fmt <- DGVMTools::GUESS
			f <- "GUESS"
		}
		log_debug("Defining source with name '", name, "', format '", f
			, "', and path '", path, "'")
		source <- DGVMTools::defineSource(id = name, format = fmt, dir = path)
		return(source)
	} else {
		msg <- "Input cannot be parsed as a version and will be ignored: "
		log_info("class(object) = ", class(source))
		print(source)
		warning(msg, source)
	}
}

#'
#' Sanitise version inputs for plotting. Returns a list of [DGVMTools::Source]
#' objects.
#'
#' This is an internal helper function and is not intended for external use.
#'
#' @param sources: A list of sources parsable by [sanitise_source].
#'
#' @return Returns a list of Sources.
#' @keywords internal
#'
sanitise_sources <- function(sources) {
	result <- call_foreach(sources, sanitise_source)
	if (length(result) < 1) {
		log_error("No input sources were provided")
	}
	return(result)
}

#'
#' Find a known quantity whose lpj-guess "standard" output file name is given by
#' name.
#'
#' @param name Standard name of the output file.
#'
#' @return A [DGVMTools::Quantity] object, or NULL if not found.
#' @keywords internal
#'
get_known_quantity <- function(name) {
	# Get known quantities, a list of DGVMTools::Quantity objects.
	quantities <- DGVMTools::GUESS@quantities

	# Find the first quantity (if any) with quant@id == name.
	quant <- Find(function(q) q@id == name, quantities)
	if (!is.null(quant)) {
		return(quant)
	}
	quant <- Find(function(q) q@id == name, ozflux_quantities)
	return(quant)
}

#'
#' Check if a quantity is a known quantity.
#'
#' @param name Name of a quantity.
#'
#' @return TRUE iff the quantity is a known quantity.
#' @keywords internal
#'
is_known_quantity <- function(name) {
	return(!is.null(get_known_quantity(name)))
}

# Attempt to get units given a variable name by looking in the observed file.
get_units <- function(var_name) {
	trimmed <- trim_dave(var_name)
	for (var in get_observed_vars()) {
		if (var@id == trimmed) {
			return(var@units)
		}
	}
	quant <- get_known_quantity(var_name)
	if (!is.null(quant)) {
		return(quant@units)
	}
	return("")
}

# FIXME: this is really bad
readable_name <- function(name) {
	trimmed <- trim_dave(name)
	lower <- tolower(trimmed)
	acronyms <- c("lai", "gpp", "nee", "nep", "et", "er")
	if (lower %in% acronyms) {
		return(toupper(trimmed))
	}

	# Check for known observed variables.
	for (var in get_observed_vars()) {
		if (var@id == trimmed) {
			return(var@name)
		}
	}

	# Check for known quantities.
	quant <- get_known_quantity(name)
	if (!is.null(quant)) {
		return(quant@name)
	}

	return(name)
}

#'
#' Sanitise a variable provided by the user.
#'
#' @param var: May be one of:
#' - A [[DGVMTools::Quantity]] object (for full control over units/name/filename)
#' - File name without extension of an lpj-guess output (e.g. lai)
#'
#' @return Returns a [[DGVMTools::Quantity]] object.
#' @keywords internal
#'
sanitise_variable <- function(var) {
	if (class(var)[1] == "Quantity") {
		# Input is already a Quantity object.
		log_debug("Input variable '", var@name, "' is already a Quantity")
		return(var)
	}

	if (class(var) == "character") {
		# Input is a string.

		# Case insensitivity...ew! (but it allows people to pass in "LAI")
		units <- get_units(var)
		id <- tolower(var)
		var <- readable_name(var)
		log_debug("Creating quantity from input string '", var, "'; id = '"
			, id, "', name = '", var, "', units = '", units, "'")
		return(DGVMTools::defineQuantity(id, var, units))
	}

	log_error("Unable to parse object as source: ", var)
}

#'
#' Sanitise a list of variables provided by the user.
#'
#' @param vars: List of variables provided by the user (see: [sanitise_variable]).
#'
#' @return Returns a list of [DGVMTools::Quantity] objects.
#' @keywords internal
#'
sanitise_variables <- function(vars) {
	result <- call_foreach(vars, sanitise_variable)
	if (length(result) < 1) {
		log_error("No variables were provided")
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
#'
#' @return This function is not yet implemented and will raise an error.
#' @keywords internal
#'
sanitise_spatial_extent <- function(spatial_extent, sites) {
	# TBI
	log_error("OZFLUX support for spatial.extent is not yet implemented. "
		, "Either setup and use a GUESS source, or specify spatial.extent.id "
		, "instead. If specified, spatial.extent.id should be the name of an "
		, "ozflux site.")
}
