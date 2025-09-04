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
#' @param sources Sources to be plotted (see: [sanitise_source])
#' @param vars Variables to be plotted (see: [sanitise_variable])
#' @param sites Sites to be plotted (see: [sanitise_ozflux_sites])
#' @param separate iff TRUE, a list of the site-level plots will be returned.
#' If FALSE, all plots will be rendered to a single panel. This has no effect
#' if a single site is to be plotted.
#' @param use_plotly iff TRUE, graphs will be plotted using [plotly]
#' @param common_yaxis iff TRUE, all plots in the panel will have the same y-axis
#' range. No effect if only 1 site is being plotted.
#' @param show_all_observations If true, all observations will be returned (ie the predicted data may contain NA values). If false, only the observations which have a matching prediction will be returned.
#' @param show_all_predictions If true, all predictions will be returned (ie the observed data may contain NA values). If false, only the predictions which have a matching observation will be returned.
#'
#' @return Returns either a single [ggplot2::ggplot] object (if separate is FALSE),
#' or a list of [ggplot2::ggplot] objects (if separate is TRUE). If use_plotly
#' is TRUE, returns [plotly] objects instead.
#' @export
#'
ozflux_plot <- function(
        sources,
        vars,
        sites = NULL,
        separate = FALSE,
        use_plotly = FALSE,
        common_yaxis = FALSE,
        show_all_observations = TRUE,
        show_all_predictions = TRUE) {

    # Sanitise data sources.
    sources <- sanitise_sources(sources)

    # Sanitise the sites to be plotted.
    sites <- sanitise_ozflux_sites(sites, sources[[1]]@dir)

    # Sanitise variables to be plotted.
    vars <- sanitise_variables(vars)

    # Read data for this gridcell.
    data <- read_data(sources, vars, sites = sites,
                       show_all_observations = show_all_observations,
                      show_all_predictions = show_all_predictions)

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
        site <- list(Lon = row$Lon, Lat = row$Lat, Name = row$Name)
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
#' @param sources Source or list of data sources to be plotted (see: [sanitise_source])
#' @param var The variable to be plotted (see: [sanitise_variable])
#' @param sites Ozflux sites to be plotted (see: [sanitise_ozflux_sites])
#' @param use_plotly iff TRUE, generate [plotly] outputs, FALSE to use [ggplot2::ggplot]
#' @param common_yaxis iff TRUE, use the same y-axis scales for all sites. FALSE
#' otherwise
#' @param show_all_observations If true, all observations will be returned (ie the predicted data may contain NA values). If false, only the observations which have a matching prediction will be returned.
#' @param show_all_predictions If true, all predictions will be returned (ie the observed data may contain NA values). If false, only the predictions which have a matching observation will be returned.
#'
#' @return Returns a single [ggplot2::ggplot] object if plotting one site, otherwise returns
#' a list of [ggplot2::ggplot] objects of the same length as the number of sites to be
#' plotted. If use_plotly is TRUE, returns [plotly] objects instead.
#' @export
#'
ozflux_panel <- function(
        sources,
        var,
        sites = NULL,
        use_plotly = FALSE,
        common_yaxis = FALSE,
        show_all_observations = TRUE,
        show_all_predictions = TRUE) {

    # Sanitise data sources.
    sources <- sanitise_sources(sources)

    # Sanitise sites to be plotted.
    sites <- sanitise_ozflux_sites(sites, sources[[1]]@dir)

    # Sanitise variables to be plotted.
    var <- sanitise_variable(var)

    # Read data for this gridcell.
    data <- read_data(sources, list(var), sites = sites,
                      show_all_observations = show_all_observations,
                      show_all_predictions = show_all_predictions)

    # Get upper/lower limits of y-axis data.
    ylim <- get_ylim(data, common_yaxis)

    # Get a suitable y-axis label.
    ylab <- get_y_label(var)

    # Get panel title.
    title <- get_panel_title(list(var))

    nsite <- nrow(sites)
    plots <- list()
    for (i in seq_len(nsite)) {
        row <- sites[i, ]
        site <- list(Lon = row$Lon, Lat = row$Lat, Name = row$Name)

        # Get the panel title for this site.
        site_title <- paste(site$Name, title)

        gc <- get_gridcell(data, site$Lat, site$Lon, site$Name)
        p <- create_plots(gc, ylab, use_plotly = use_plotly, ylim = ylim)
        panel <- create_panel(p$timeseries, p$pvo, p$subannual, use_plotly = use_plotly
            , ylab = ylab, title = site_title, colspans = c(2, 1, 1))
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
#' @param sources Data sources to be plotted (see: [sanitise_source])
#' @param var The single variable to be plotted (see: [sanitise_variable])
#' @param sites Sites to be plotted (see: [sanitise_ozflux_sites])
#' @param layers The layers to be plotted. E.g. `paste0("sw_", 0:14)`
#' @param title The desired panel titles
#' @param separate_plots iff TRUE, draw each layer in a separate plot. FALSE to
#' draw all layers in a single plot
#' @param use_plotly iff TRUE, generate [plotly] outputs, FALSE to use [ggplot2::ggplot]
#' @param common_yaxis iff TRUE, use a common y-axis for all plots. False otherwise
#' @param show_all_observations If true, all observations will be returned (ie the predicted data may contain NA values). If false, only the observations which have a matching prediction will be returned.
#' @param show_all_predictions If true, all predictions will be returned (ie the observed data may contain NA values). If false, only the predictions which have a matching observation will be returned.
#' @param combine_sites iff TRUE, combine all sites into a single plot. False otherwise
#' @param allow_points iff TRUE, allow points to be plotted for sparse datasets. FALSE will ensure that all data is plotted using lines.
#'
#' @return Returns a list of [ggplot2::ggplot] objects. If use_plotly is TRUE,
#' returns [plotly] objects instead.
#' @export
#'
ozflux_plot_layerwise <- function(
    sources,
    var,
    sites = NULL,
    layers = NULL,
    title = NULL,
    separate_plots = TRUE,
    combine_sites = FALSE,
    use_plotly = FALSE,
    common_yaxis = FALSE,
    show_all_observations = TRUE,
    show_all_predictions = TRUE,
    allow_points = TRUE) {
    # TODO: refactor this function out of the package. The layers should be an
    # optional argument to ozflux_plot(). If absent, they're determined using
    # the current algorithm (ie try total/mean). If present, there should be
    # a second optional argument which determines whether one plot is generated
    # per layer, or if all layers are to be plotted on the same plot.

    # Sanitise data sources.
    sources <- sanitise_sources(sources)

    # Sanitise variables to be plotted.
    var <- sanitise_variable(var)

    # Get the site to be plotted.
    sites <- sanitise_ozflux_sites(sites, sources[[1]]@dir)

    # Read data for this gridcell.
    if (is.null(layers) && length(sources) > 0) {
        layers <- get_layers(sources[[1]], var, sites)
        log_diag("No layers were specified. Therefore the default layers for "
            , "this file will be used. These are: ["
            , paste(layers, collapse = ", "), "]")
    }
    data <- read_data(sources, list(var), sites = sites, layers = layers,
                      show_all_observations = show_all_observations,
                      show_all_predictions = show_all_predictions)

    panels <- list()
    nsite <- nrow(sites)
    for (i in seq_len(nsite)) {
        row <- sites[i, ]
        site <- list(Lon = row$Lon, Lat = row$Lat, Name = row$Name)
        gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)

        # Get upper/lower limits of y-axis data.
        ylim <- get_ylim(gridcell, common_yaxis)

        plots <- list()
        if (separate_plots) {
            for (layer in layers) {
                layers_to_plot <- get_layer_names_for_sources(var, 1, layer
                    , sources, nlayer = length(layers))
                multiplot <- length(layers) > 1 || nrow(sites) > 1
                xlab <- if (multiplot) "" else NULL
                ylab <- if (multiplot) "" else NULL
                plt <- plot_timeseries(gridcell, layers = layers_to_plot, ylim = ylim, xlab = xlab, ylab = ylab, allow_points = allow_points)
                plt <- convert_plot(plt, use_plotly)
                plt <- set_title(plt, layer, use_plotly)
                plots[[length(plots) + 1]] <- plt
            }
        } else {
            lyrs <- get_layer_names_for_sources(var, 1, layers, sources)
            plt <- plot_timeseries(gridcell, layers = lyrs, ylim = ylim, allow_points = allow_points)
            plt <- trim_ggplot(plt)
            plt <- convert_plot(plt, use_plotly)
            plots[[length(plots) + 1]] <- plt
        }
        xlab <- "Date"
        ylab <- get_y_label(var, site$Name)
        if (is.null(title)) {
            title <- trim_dave(var@name)
        }
        site_title <- paste(site$Name, title)
        if (length(plots) > 1) {
            args <- c(plots, use_plotly = use_plotly, xlab = xlab, ylab = ylab
                , title = site_title)
            panel <- do.call(create_square_panel, args)
            panels[[length(panels) + 1]] <- panel
        } else {
            plt <- set_title(plots[[1]], site_title, use_plotly)
            panels[[length(panels) + 1]] <- plt
        }
    }
    if (length(panels) == 1) {
        return(panels[[1]])
    }
    if (combine_sites && !separate_plots && length(sites) > 1) {
        if (is.null(title)) {
            title <- var@name
        }
        panel <- dave_panel(panels, xlab, ylab, title, use_plotly, sites)
        return(panel)
    }
    return(panels)
}
