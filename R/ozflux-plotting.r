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
#' Create a panel of ggplots, with one plot per site.
#'
#' Plots a single variable across each site, generating one plot per site.
#'
#' @param sources: Input sources.
#' @param var: The variable to be plotted.
#'
#' @return Returns a list of ggplot objects.
#' @author Drew Holzworth
#' @export
#'
ozflux_panel <- function(sources, var) {
	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	var <- sanitise_variable(var)

	# Read data for this gridcell.
	data <- read_data(var, sources)

	sites <- read_ozflux_sites()

	plots <- list()
	for (i in seq_len(nrow(sites))) {
		site <- sites[i, ]
		log_info("Plotting ", site$Name, "...")

		# Extract data for the required grid cell.
		gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)

		# Plot the gridcell.
		title <- paste(site$Name, var@name)
		plt <- plot_timeseries(gridcell) +
			ggplot2::labs(title = title, subtitle = NULL, caption = NULL) +
			ggpubr::rremove("xylab")

		plots[[length(plots) + 1]] <- plt
	}

	xlab <- "Date"
	ylab <- var@name
	if (var@units != "") {
		ylab <- paste0(ylab, " (", var@units, ")")
	}
	gp <- grid::gpar(cex = 1.3)
	panel <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE)
	panel <- ggpubr::annotate_figure(panel,
		left = grid::textGrob(ylab, rot = 90, vjust = 1, gp = gp),
		bottom = grid::textGrob(xlab, gp = gp))
	return(panel)
}

#'
#' Plot a variable at the specified site using plotly.
#'
#' @param site: Name of the ozflux site, or tuple of (lon, lat).
#' @param sources: List of sources. \seealso{\link{sanitise_sources}}.
#' @param var: The variable to be plotted. \seealso{\link{sanitise_variable}}.
#'
#' @return Returns a plotly object.
#' @author Drew Holzworth
#' @export
#'
ozflux_plotly <- function(site, sources, var) {
	return(ggplotly(ozflux_ggplot(site, sources, var)))
}
