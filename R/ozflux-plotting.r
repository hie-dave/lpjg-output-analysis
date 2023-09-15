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

	# Get plot and axis titles.
	xlab <- "Date"
	ylab <- get_y_label(data@quant)
	title <- get_panel_title(vars)

	plots <- list()
	nsite <- nrow(sites)
	ncol <- as.integer(ceiling(sqrt(nsite)))
	for (i in seq_len(nsite)) {
		row <- sites[i, ]
		site <- c(Lon = row$Lon, Lat = row$Lat, Name = row$Name)
		plt <- ozflux_plot_site(data, ylim, site, separate, use_plotly, nsite
			, vars, xlab = xlab, ylab = ylab)
		if (nsite > 1 && !use_plotly && common_yaxis && (i - 1) %% ncol != 0) {
			plt <- plt + ggplot2::theme(
				axis.text.y = ggplot2::element_blank(),
				axis.ticks.y = ggplot2::element_blank())
		}
		plots[[length(plots) + 1]] <- plt
	}

	# Note: ozflux_plot_site will convert the returned plot to a plotly object
	# if necessary.

	# Iff plotting a single site, return the one and only plot.
	if (nsite == 1) {
		return(plots[[1]])
	}

	# Otherwise, generate a panel of plots (1 child plot per site).
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
#' Create ozflux site-level layerwise plots.
#'
#' Create a panel of plots, where each plot in the panel contains one of the
#' layers of the specified variable. One panel will be generated for each site.
#'
#' (In this nomenclature, a "layer" technically refers to a column in the output
#' file).
#'
#' @param sources: Sources of data. Should be paths to lpj-guess repos or
#' \seealso{\link{DGVMTools::Source}} objects.
#' @param var: The variable to be plotted, specified as the output filename
#' without extension (e.g. `"dave_sw"`), or as
#' \seealso{\link{DGVMTools::Quantity}} objects.
#' @param sites: Ozflux sites to be plotted, expressed as site names or as
#' tuples of (lon, lat).
#' @param layers: The layers to be plotted. E.g. `paste0("sw_", 0:14)`.
#' @param title: The desired panel titles.
#' @param separate_plots: TRUE to draw each layer in a separate plot. FALSE to
#' draw all layers on the same plot.
#' @param use_plotly: TRUE to draw plots with plotly. FALSE to use ggplot2.
#'
#' @return Returns a list of ggplot objects.
#' @author Drew Holzworth
#' @export
#'
ozflux_plot_layerwise <- function(
	sources,
	var,
	sites,
	layers,
	title = NULL,
	separate_plots = TRUE,
	use_plotly = FALSE) {
	# TODO: refactor this function out of the package. The layers should be an
	# optional argument to ozflux_plot(). If absent, they're determined using
	# the current algorithm (ie try total/mean). If present, there should be
	# a second optional argument which determines whether one plot is generated
	# per layer, or if all layers are to be plotted on the same plot.

	# Sanitise data sources.
	sources <- sanitise_sources(sources)

	# Sanitise variables to be plotted.
	var <- list(sanitise_variable(var))

	# Get the site to be plotted.
	sites <- sanitise_ozflux_sites(sites)

	# Read data for this gridcell.
	data <- read_data(var, sources, site = sites, layers = layers)

	panels <- list()
	nsite <- nrow(sites)
	for (i in seq_len(nsite)) {
		site <- sites[i, ]
		gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)

		plots <- list()
		if (separate_plots) {
			for (layer in layers) {
				layers_to_plot <- get_layer_names_for_sources(var, 1, layer
					, sources, nlayer = length(layers))
				plt <- plot_timeseries(gridcell, layers = layers_to_plot)
				plt <- trim_ggplot(plt, title = layer)
				plt <- convert_plot(plt, use_plotly)
				plots[[length(plots) + 1]] <- plt
			}
		} else {
			lyrs <- get_layer_names_for_sources(var, 1, layers, sources)
			plt <- plot_timeseries(gridcell, layers = lyrs)
			plt <- trim_ggplot(plt)
			plt <- convert_plot(plt, use_plotly)
			plots[[length(plots) + 1]] <- plt
		}
		xlab <- "Date"
		ylab <- get_y_label(var[[1]], site$Name)
		site_title <- paste(site$Name, title)
		args <- c(plots, use_plotly = use_plotly, xlab = xlab, ylab = ylab
			, title = site_title)
		panel <- do.call(create_square_panel, args)
		panels[[length(panels) + 1]] <- panel
	}
	if (length(panels) == 1) {
		return(panels[[1]])
	}
	return(panels)
}
