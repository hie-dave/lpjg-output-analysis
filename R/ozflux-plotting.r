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
#' Create timeseries plots of predictions against observations.
#'
#' Plots a single variable across predictions at each specified ozflux site,
#' against observations (if any are available), generating one plot per site. If
#' separate is TRUE, then a separate plot will be returned for each site. If
#' separate is FALSE, then a panel of all plots will be returned.
#'
#' If data is missing for any of the sites to be plotted, an error will occur.
#'
#' @param sources: One or more sources. These can be paths to lpj-guess
#' repositories, or \seealso{\link{DGVMTools::Source}} objects. This argument is
#' optional if sources have been configured via \seealso{\link{dave_config}}.
#' @param var: The variable to be plotted. This may be specified as a string
#' containing the output file name without extension (e.g. "dave_lai"), or as a
#' \seealso{\link{DGVMTools::Quantity}} object.
#' @param sites: List of sites to be plotted. These may be specified as either
#' strings (site names), or as tuples of (lon, lat). If NULL is provided, all
#' ozflux sites will be plotted.
#' @param separate: Iff TRUE, a list of the site-level plots will be returned.
#' If FALSE, all plots will be rendered to a single panel. This has no effect
#' if a single site is to be plotted.
#' @param use_plotly: Iff true, graphs will be plotted using plotly.
#' @param common_yaxis: Iff true, all plots in the panel will have the same y-
#' axis range. No effect if only 1 site is being plotted.
#'
#' @return Can return a single, or vector of, ggplot or plotly objects.
#' @author Drew Holzworth
#' @export
#'
ozflux_plot <- function(
		sources,
		var,
		sites = NULL,
		separate = FALSE,
		use_plotly = FALSE,
		common_yaxis = FALSE) {
	if (is.null(sites)) {
		sites <- read_ozflux_sites()
	} else {
		# read_ozflux_sites() will return a dataframe. sanitise_ozflux_sites()
		# will return a list. We need to convert the list into a dataframe here
		# for consistency.
		sites <- as.data.frame(sanitise_ozflux_sites(sites))
		colnames(sites) <- c("Lon", "Lat", "Name")
	}

	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	var <- sanitise_variable(var)

	# Read data for this gridcell.
	data <- read_data(var, sources)

	# Get upper/lower limits of y-axis data.
	if (common_yaxis) {
		ylim <- get_ylim(data)
	} else {
		ylim <- NULL
	}

	plots <- list()
	nsite <- nrow(sites)
	for (i in seq_len(nsite)) {
		plots[[length(plots) + 1]] <- ozflux_plot_site(data, ylim, sites[i, ]
			, separate, use_plotly, nsite, var)
	}

	if (separate) {
		return(lapply(plots, function(p) convert_plot(p, use_plotly)))
	}

	# Configure titles and axis text for the panel.
	xlab <- "Date"
	ylab <- get_y_label(data@quant)
	title <- gsub("dave_", "", data@quant@name)

	# Combine plots into a single panel.
	panel <- dave_panel(plots, xlab, ylab, title, use_plotly, sites)
	return(panel)
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
