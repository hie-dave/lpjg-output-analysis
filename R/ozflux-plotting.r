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
		plt <- plot_timeseries(gridcell)
		plt <- trim_ggplot(plt, title)

		plots[[length(plots) + 1]] <- plt
	}

	xlab <- "Date"
	ylab <- get_y_label(var)
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
	return(plotly::ggplotly(ozflux_ggplot(site, sources, var)))
}

#'
#' Create layerwise plots.
#'
#' Create a panel of plots, where each panel contains one of the layers of the
#' specified variable. One panel will be generated for each site.
#'
#' @param sites: Ozflux sites to be plotted, expressed as tuples of (lon, lat).
#' @param sources: Sources of data. Should be paths to lpj-guess repos.
#' @param var: The variable to be plotted. E.g. `"dave_sw"``.
#' @param layers: The layers to be plotted. E.g. `paste0("sw_", 0:14)`.
#'
#' @return Returns a list of ggplot objects.
#' @author Drew Holzworth
#' @export
#'
plot_layerwise <- function(sites, sources, var, layers, title = NULL) {
	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	var <- sanitise_variable(var)

	# Get the site to be plotted.
	sites <- sanitise_ozflux_sites(sites)

	# Read data for this gridcell.
	data <- read_data(var, sources, site = sites, layers = layers)

	panels <- list()
	gp <- grid::gpar(cex = 1.3)
	for (site in sites) {
		gridcell <- get_gridcell(data, site$lat, site$lon, site$name)

		plots <- list()
		for (layer in layers) {
			layers_to_plot <- c()
			for (source in sources) {
				lyr <- get_layer_name(layer, source)
				layers_to_plot <- c(layers_to_plot, lyr)
			}
			plt <- plot_timeseries(gridcell, layers = layers_to_plot)
			plt <- trim_ggplot(plt, title = layer)
			plots[[length(plots) + 1]] <- plt
		}
		panel <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE
			, legend = "bottom", align = "hv")
		xlab <- "Date"
		ylab <- get_y_label(var, site$name)
		site_title <- paste(site$name, title)
		panel <- ggpubr::annotate_figure(
			panel,
			top = grid::textGrob(site_title, gp = gp),
			left = grid::textGrob(ylab, rot = 90, vjust = 1, gp = gp),
			bottom = grid::textGrob(xlab, gp = gp))
		panels[[length(panels) + 1]] <- panel
	}
	return(panels)
}
