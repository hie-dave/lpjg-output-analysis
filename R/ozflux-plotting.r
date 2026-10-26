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
        show_all_predictions = TRUE,
        show_obs = TRUE) {

    # Sanitise data sources.
    sources <- sanitise_sources(sources)

    # Sanitise the sites to be plotted.
    sites <- sanitise_ozflux_sites(sites, sources[[1]]@dir)

    # Sanitise variables to be plotted.
    vars <- sanitise_variables(vars)

    # Read data for this gridcell.
    data <- read_data(sources, vars, sites = sites,
                      show_all_observations = show_all_observations,
                      show_all_predictions = show_all_predictions,
                      read_obs = show_obs)
    data <- filter_default_plot_layers(data, sources, vars, sites)

    # Get upper/lower limits of y-axis data.
    if (common_yaxis) {
        ylim <- get_ylim(data)
    } else {
        ylim <- NULL
    }

    # Get plot and axis titles.
    xlab <- "Date"
    ylab <- if (length(vars) == 1) get_y_label(vars[[1]]) else "Value"
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
    data <- filter_default_plot_layers(data, sources, list(var), sites)

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
#' Create site-level plots for one or more output variables/layers. By default,
#' the requested output variables are drawn as separate facets, with one plot
#' returned for each site.
#'
#' The plotting controls are semantic rather than ggplot-specific. `facet_by`
#' chooses which data columns become small multiples. `site_layout` controls
#' whether sites are returned as separate plots, included as facets, or overlaid.
#' `entity_layout` controls stand/patch/individual output. When
#' `entity_level = "auto"`, individual-level data are grouped by
#' stand/patch/individual, patch-level data by stand/patch, stand-level data by
#' stand, and gridcell-level data as a single entity. Entities are separate time
#' series by default.
#'
#' @param sources Data sources to be plotted (see: [sanitise_source])
#' @param var The single variable to be plotted (see: [sanitise_variable])
#' @param sites Sites to be plotted (see: [sanitise_ozflux_sites])
#' @param layers The layers to be plotted. E.g. `paste0("sw_", 0:14)`
#' @param title The desired plot title. For separate site plots, the site name
#' is prepended.
#' @param facet_by Columns to draw as separate facets. Common values are
#' `"variable"`, `"quantity"`, `"source"`, `"Site"`, `"entity"`, `"pft"`, and
#' `"layer"`. Use `NULL` or `"none"` for no faceting. Facets with only one
#' value are omitted.
#' @param site_layout How multiple sites should be shown. `"separate"` returns
#' one plot per site, `"facet"` adds site to the facets, and `"overlay"` draws
#' all sites in the same facets.
#' @param entity_layout How stand/patch/individual output should be shown.
#' `"series"` draws each entity as a separate time series, `"facet"` facets by
#' entity, and `"aggregate"` plots the mean value across entities.
#' @param entity_level Entity detail to show. `"auto"` uses the finest level
#' present in the data: individual, then patch, then stand, then gridcell.
#' @param colour_by Column used for line/point colour. Use `NULL` or `"none"`
#' for a constant colour.
#' @param linetype_by Column used for line type. Use `NULL` or `"none"` for a
#' constant line type.
#' @param shape_by Column used for point shape. Use `NULL` or `"none"` for a
#' constant point shape.
#' @param geometry One of `"auto"`, `"line"`, `"point"`, or `"line_point"`.
#' `"auto"` draws sparse series as points and denser series as lines.
#' @param axis `"free"` allows each facet to choose its own y-axis; `"shared"`
#' uses one y-axis scale for all facets.
#' @param use_plotly iff TRUE, generate [plotly] outputs, FALSE to use [ggplot2::ggplot]
#' @param show_all_observations If true, all observations will be returned (ie the predicted data may contain NA values). If false, only the observations which have a matching prediction will be returned.
#' @param show_all_predictions If true, all predictions will be returned (ie the observed data may contain NA values). If false, only the predictions which have a matching observation will be returned.
#'
#' @return Returns a [ggplot2::ggplot] object for one site or combined site
#' layouts. With `site_layout = "separate"` and multiple sites, returns one plot
#' per site as a list. If `use_plotly` is TRUE, returns [plotly] objects instead.
#' @export
#'
ozflux_plot_layerwise <- function(
    sources,
    var,
    sites = NULL,
    layers = NULL,
    title = NULL,
    facet_by = "variable",
    site_layout = c("separate", "facet", "overlay"),
    entity_layout = c("series", "facet", "aggregate"),
    entity_level = c("auto", "stand", "patch", "indiv", "gridcell"),
    colour_by = "source",
    linetype_by = NULL,
    shape_by = NULL,
    geometry = c("auto", "line", "point", "line_point"),
    axis = c("free", "shared"),
    use_plotly = FALSE,
    show_all_observations = TRUE,
    show_all_predictions = TRUE,
    show_obs = TRUE) {
    site_layout <- match.arg(site_layout)
    entity_layout <- match.arg(entity_layout)
    entity_level <- match.arg(entity_level)
    geometry <- match.arg(geometry)
    axis <- match.arg(axis)

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
                      show_all_predictions = show_all_predictions,
                      read_obs = show_obs)
    data <- add_site_column(data, sites)

    if (is.null(title)) {
        title <- trim_dave(var@name)
    }
    ylab <- get_y_label(var)

    make_plot <- function(plot_data, plot_title) {
        plt <- plot_layerwise_table(
            plot_data,
            facet_by = facet_by,
            entity_layout = entity_layout,
            entity_level = entity_level,
            colour_by = colour_by,
            linetype_by = linetype_by,
            shape_by = shape_by,
            geometry = geometry,
            axis = axis,
            title = plot_title,
            ylab = ylab
        )
        convert_plot(plt, use_plotly)
    }

    if (site_layout == "separate") {
        plots <- list()
        for (i in seq_len(nrow(sites))) {
            row <- sites[i, ]
            site <- list(Lon = row$Lon, Lat = row$Lat, Name = row$Name)
            gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)
            plot_title <- paste(site$Name, title)
            plots[[site$Name]] <- make_plot(gridcell, plot_title)
        }
        if (length(plots) == 1) {
            return(plots[[1]])
        }
        return(plots)
    }

    if (site_layout == "facet") {
        facet_by <- unique(c("Site", facet_by))
    }
    make_plot(data, title)
}
