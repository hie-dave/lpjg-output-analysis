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
#' @param vars: The variables to be plotted. These may be specified as strings
#' containing the output file names without extension (e.g. "dave_lai"), or as
#' \seealso{\link{DGVMTools::Quantity}} objects.
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
		vars,
		sites = NULL,
		separate = FALSE,
		use_plotly = FALSE,
		common_yaxis = FALSE) {
	# Sanitise the sites to be plotted.
	sites <- sanitise_ozflux_sites(sites)

	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	vars <- sanitise_variables(vars)

	# Read data for this gridcell.
	data <- read_data(vars, sources, site = sites)

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
			, separate, use_plotly, nsite, vars)
	}

	# Note: ozflux_plot_site will convert the returned plot to a plotly object
	# if necessary.

	# Iff plotting a single site, return the one and only plot.
	if (nsite == 1) {
		return(plots[[1]])
	}

	# Otherwise, generate a panel of plots (1 child plot per site).

	# Configure titles and axis text for the panel.
	xlab <- "Date"
	ylab <- get_y_label(data@quant)
	title <- get_panel_title(vars)

	# Combine plots into a single panel.
	panel <- dave_panel(plots, xlab, ylab, title, use_plotly, sites)
	return(panel)
}

#'
#' Create a 2x2 panel of plots, identical to what is seen in the ozflux
#' benchmarks.
#'
#' One series will be plotted on each graph for each source. One
#' panel will be created for each site.
#'
#' @param sources: One or more sources of data. These may be paths to LPJ-GUESS
#' repositories, paths to directories containing output files, or as
#' \seealso{\link{DGVMTools::Source}} objects.
#' @param var: The variable to be plotted. This may be specified as a string
#' containing the output file name without extension (e.g. "dave_lai"), or as
#' a DGVMTools::Quantity}} object.
#' @param sites: One or more ozflux sites. May be specified as names of sites,
#' or as tuples of (Lon, Lat). If NULL, all sites will be plotted.
#' @param use_plotly: TRUE to generate plotly outputs, FALSE to use ggplot.
#' @param common_yaxis: TRUE to use the same y-axis scales for all sites. FALSE
#' otherwise.
#'
#' @return Returns a single plot object if plotting one site, otherwise returns
#' a list of plot objects of the same length as the number of sites to be
#' plotted.
#' @author Drew Holzworth
#' @export
#'
ozflux_panel <- function(
		sources,
		var,
		sites = NULL,
		use_plotly = FALSE,
		common_yaxis = FALSE) {

	# Sanitise sites to be plotted.
	sites <- sanitise_ozflux_sites(sites)

	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	var <- sanitise_variable(var)

	# Read data for this gridcell.
	data <- read_data(list(var), sources, site = sites)

	# Get upper/lower limits of y-axis data.
	ylim <- get_ylim(data, common_yaxis)

	# Get a suitable y-axis label.
	ylab <- get_y_label(var)

	# Get panel title.
	title <- get_panel_title(list(var))

	nsite <- nrow(sites)
	plots <- list()
	for (i in seq_len(nsite)) {
		site <- sites[i, ]

		# Get the panel title for this site.
		site_title <- paste(site$Name, title)

		gc <- get_gridcell(data, site$Lat, site$Lon, site$Name)
		p <- create_plots(gc, ylab, use_plotly = use_plotly, ylim = ylim)
		panel <- create_panel(p$timeseries, p$pvo, p$subannual, use_plotly
			, ylab = ylab, title = site_title)
		plots[[length(plots) + 1]] <- panel
	}

	if (nsite == 1) {
		return(plots[[1]])
	}
	return(plots)
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
	nsite <- nrow(sites)
	for (i in seq_len(nsite)) {
		site <- sites[i, ]
		gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)

		plots <- list()
		for (layer in layers) {
			layers_to_plot <- c()
			for (source in sources) {
				lyr <- get_layer_names(layer, source)
				layers_to_plot <- c(layers_to_plot, lyr)
			}
			plt <- plot_timeseries(gridcell, layers = layers_to_plot)
			plt <- trim_ggplot(plt, title = layer)
			plots[[length(plots) + 1]] <- plt
		}
		panel <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE
			, legend = "bottom", align = "hv")
		xlab <- "Date"
		ylab <- get_y_label(var, site$Name)
		site_title <- paste(site$Name, title)
		panel <- ggpubr::annotate_figure(
			panel,
			top = grid::textGrob(site_title, gp = gp),
			left = grid::textGrob(ylab, rot = 90, vjust = 1, gp = gp),
			bottom = grid::textGrob(xlab, gp = gp))
		panels[[length(panels) + 1]] <- panel
	}
	return(panels)
}
