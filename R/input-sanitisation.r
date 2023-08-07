
# Name of the package.
set_global("dave_pkgname", "daveanalysis")

# Ozflux gridcells. This is a data frame with 3 columns (Lon, Lat, Name).
set_global("ozflux_sites", NULL)

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
#' Sanitise the a site specifier.
#'
#' @param site: This may be the name of an ozflux site, or (lon, lat) tuple.
#'
#' @return Returns a named vector with (lon, lat, name) elements.
#' @author Drew Holzworth \email{d.holzworth@@westernsydney.edu.au}
#' @keywords internal
sanitise_ozflux_site <- function(site) {
	if (class(site) == "character") {
		sites <- read_ozflux_sites()
		index <- which(sites$Name == site)
		if (length(index) < 1) {
			log_error("Unknown ozflux site: '", site, "'")
		}
    	lat <- sites$Lat[index]
    	lon <- sites$Lon[index]
		name <- site
	} else if (length(site) == 2) {
		lon <- as.double(site[1])
		lat <- as.double(site[2])
		name <- NULL
	} else {
		log_error("Unable to interprete site: ", site)
	}
	log_debug("Resolved ozflux site '", site, "': lon=", lon, ", lat=", lat)
	return(list(lon = lon, lat = lat, name = name))
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
		sources <- c(sources)
	}

	for (object in sources) {
		if (class(object)[1] == "Source") {
			result <- append(result, object)
		} else if (class(object) == "character") {
			name <- basename(object)
			path <- object
			if (dir.exists(file.path(path, "benchmarks", "ozflux"))) {
				fmt <- OZFLUX
			} else {
				fmt <- DGVMTools::GUESS
			}
			log_debug("Defining source with name '", name, "', format '", fmt
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

	if (is.null(.versions) || length(.versions) < 1) {
		log_error("No versions were provided")
	}

	return(.versions)
}

sanitise_variable <- function(var) {
	if (class(var)[1] == "Quantity") {
		# Input is already a Quantity object.
		return(var)
	}

	if (class(var) == "character") {
		# Input is a string.

		# Case insensitivity...ew! (but it allows people to pass in "LAI")
		return(DGVMTools::defineQuantity(tolower(var), var, ""))
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
