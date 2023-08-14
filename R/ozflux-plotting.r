#'
#' Create a timeseries ggplot of the specified variable for all specified sites
#' for the configured sources.
#'
#' @param site: Name of the site to be plotted, or a tuple of (lon, lat).
#' @param sources: One or more sources. These can be paths to lpj-guess
#' repositories, or \seealso{\link{DGVMTools::Source}} objects. This argument is
#' optional if sources have been configured via \seealso{\link{dave_config}}.
#' @param var: The variable to be plotted. A corresponding output file must
#' exist for this variable named either var.out or dave_var.out.
#'
#' @return Returns a ggplot object.
#' @author Drew Holzworth
#' @export
#'
ozflux_ggplot <- function(site, sources, var) {
	# Get the site to be plotted.
	site <- sanitise_ozflux_site(site)

	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	var <- sanitise_variable(var)

	# Read data for this gridcell.
	data <- read_data(var, sources, site)

	# Extract data for the required grid cell.
	gridcell <- get_gridcell(data, site$lat, site$lon, site$name)

	return(plot_timeseries(gridcell))
}

#'
#' Plot a variable at the specified site using plotly.
#'
#' @param site: Name of the ozflux site, or tuple of (lon, lat).
#' @param sources: List of sources. \seealso{sanitise_sources}.
#' @param var: The variable to be plotted. \seealso{sanitise{variable}}.
#'
#' @return Returns a plotly object.
#' @author Drew Holzworth
#' @export
#'
ozflux_plotly <- function(site, sources, var) {
	return(ggplotly(ozflux_ggplot(site, sources, var)))
}
