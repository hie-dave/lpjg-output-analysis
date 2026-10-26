
# Colour palette optimised for people with various kinds of colour-blindness.
# Wong, B. (2011) Color blindness, Nature Methods, Vol 8, No. 6.
set_global("cb_colours", c(
    "#e69f00",
    "#56b4e9",
    "#cc79a7",
    "#009e73",
    "#0072b2",
    "#d55e00",
    "#f0e442",
    "#000000"
))

get_colour_palette <- function(n, alpha = 1, begin = 0) {
    base_cols <- get_global("cb_colours")
    if (n <= length(base_cols)) {
        colours <- base_cols[1:n]
    } else {
        # Fall back to an HCL palette when more colours are needed.
        log_warning("Requested ", n, " colours; extending beyond the ",
                    length(base_cols), "-colour Wong palette")
        colours <- grDevices::hcl.colors(n, palette = "Dark 3")
    }
    log_debug("Successfully generated colour palette with ", n, " colours")
    return(colours)
}

to_plotly <- function(chart, x_title = NULL, y_title = NULL) {
    res <- plotly::ggplotly(chart)
    layout_args <- list(legend = list(bgcolor = "rgba(0,0,0,0)"))
    if (!is.null(x_title)) {
        layout_args$xaxis <- list(title = list(text = x_title))
    }
    if (!is.null(y_title)) {
        layout_args$yaxis <- list(title = list(text = y_title))
    }
    res <- do.call(plotly::layout, c(list(res), layout_args))
    res <- plotly::config(res, scrollZoom = TRUE)
    return(res)
}

create_panel <- function(
        ...,
        use_plotly = FALSE,
        ncol = 2,
        xlab = NULL,
        ylab = NULL,
        title = NULL,
        colspans = NULL) {

    plots <- list(...)
    log_diag("Creating panel with ", length(plots), " plots")
    nplot <- length(plots)

    # Set default colspans if not provided
    if (is.null(colspans)) {
        colspans <- rep(1, nplot)
    } else if (length(colspans) != nplot) {
        stop("Length of colspans (", length(colspans), ") must match the number of plots (", nplot, ")")
    }

    if (use_plotly) {
        fix_legend <- function(p) {
            return(plotly::layout(p, legendgroup = "Layout"))
        }
        for (plot in plots) {
            plot <- fix_legend(plot)
        }

        # Custom colspans not supported for plotly yet
        if (any(colspans != 1)) {
            warning("Custom colspans not supported for plotly. Using default layout.")
        }

        nrow <- as.integer(ceiling(nplot / ncol))
        args <- c(plots, nrows = nrow, shareY = TRUE)
        plt <- do.call(plotly::subplot, args)
        for (layer in seq_len(length(plt$x$data) * (nplot - 1L) / nplot)) {
            plt$x$data[[layer]]$showlegend <- FALSE
        }
    } else {
        for (i in seq_along(plots)) {
            plots[[i]] <- trim_ggplot(plots[[i]], xlab = TRUE)
        }

        gp <- grid::gpar(cex = 1.3)

        # Check if we need custom layout
        if (all(colspans == 1)) {
            # Use standard layout
            nrow <- as.integer(ceiling(nplot / ncol))
            args <- c(plots, ncol = ncol, nrow = nrow, common.legend = TRUE,
                legend = "bottom", align = "hv")
            plt <- do.call(ggpubr::ggarrange, args)
        } else {
            # Use custom layout with colspans
            # First, calculate how many rows we need and how many plots go in each row
            total_cols <- ncol
            row_plots <- list()
            plot_index <- 1
            row_index <- 1

            while (plot_index <= nplot) {
                row_plots[[row_index]] <- list()
                cols_used <- 0

                # Add plots to this row until we run out of columns or plots
                while (plot_index <= nplot && cols_used + colspans[plot_index] <= total_cols) {
                    row_plots[[row_index]] <- c(row_plots[[row_index]], list(plots[[plot_index]]))
                    cols_used <- cols_used + colspans[plot_index]
                    plot_index <- plot_index + 1
                }

                row_index <- row_index + 1
            }

            # Now create each row as a separate ggarrange
            row_panels <- list()
            plot_tracker <- 1

            for (i in seq_along(row_plots)) {
                row_size <- length(row_plots[[i]])
                if (row_size > 0) {
                    # Get the colspans for plots in this row
                    row_indices <- seq(plot_tracker, length.out = row_size)
                    row_colspans <- colspans[row_indices]
                    widths <- row_colspans / sum(row_colspans) * length(row_colspans)

                    # Create this row's panel
                    row_panels[[i]] <- ggpubr::ggarrange(
                        plotlist = row_plots[[i]],
                        ncol = length(row_plots[[i]]),
                        widths = widths,
                        common.legend = FALSE,
                        legend = "none",
                        align = "h"
                    )

                    # Update the plot tracker
                    plot_tracker <- plot_tracker + row_size
                }
            }

            # Combine all rows into a single panel
            plt <- ggpubr::ggarrange(
                plotlist = row_panels,
                ncol = 1,
                nrow = length(row_panels),
                common.legend = TRUE,
                legend = "bottom",
                legend.grob = ggpubr::get_legend(plots),
                align = "v"
            )
        }

        if (!is.null(xlab)) {
            plt <- ggpubr::annotate_figure(plt,
                bottom = grid::textGrob(xlab, gp = gp))
        }
        if (!is.null(ylab)) {
            plt <- ggpubr::annotate_figure(plt,
                left = grid::textGrob(ylab, rot = 90, vjust = 1, gp = gp))
        }
        if (!is.null(title)) {
            plt <- ggpubr::annotate_figure(plt,
                top  = grid::textGrob(title, vjust = 1, gp = gp))
        }
    }
    return(plt)
}

create_square_panel <- function(
        ...,
        use_plotly,
        xlab = NULL,
        ylab = NULL,
        title = NULL) {
    nplot <- length(list(...))
    ncol <- as.integer(ceiling(sqrt(nplot)))
    return(create_panel(..., use_plotly = use_plotly, ncol = ncol, xlab = xlab
        , ylab = ylab, title = title))
}

set_text_multiplier <- function(plt, text_multiplier = NULL) {
    if (!is.null(text_multiplier)) {
        size <- ggplot2::theme_get()$text$size * text_multiplier
        plt <- plt + ggplot2::theme(text = ggplot2::element_text(size = size))
    }
    return(plt)
}

#' Plot temporal data
#'
#' This is a modified copy of [DGVMTools::plotTemporal], with the ability to plot
#' some fields as points instead of lines.
#'
#' For more information, see: [DGVMTools::plotTemporal]
#'
#' @keywords internal
#' @return A ggplot
#'
plot_temporal <- function(gc,
                         layers = NULL,
                         gridcells = NULL,
                         title = character(0),
                         subtitle = character(0),
                         cols = NULL,
                         col.by = "Layer",
                         col.labels = waiver(),
                         linetypes = NULL,
                         linetype.by = NULL,
                         linetype.labels = waiver(),
                         linewidths = NULL,
                         linewidth.by = NULL,
                         linewidth.labels = waiver(),
                         sizes = NULL,
                         size.by = NULL,
                         size.labels = waiver(),
                         shapes = NULL,
                         shape.by = NULL,
                         shape.labels = waiver(),
                         alphas = NULL,
                         alpha.by = NULL,
                         alpha.labels = waiver(),
                         y.label = NULL,
                         y.lim = NULL,
                         x.label = NULL,
                         x.lim = NULL,
                         legend.position = "bottom",
                         text.multiplier = NULL,
                         dropEmpty = FALSE,
                         plotTrend = FALSE,
                         ...
) {
    data.toplot <- DGVMTools::plotTemporal(gc,
        layers = layers,
        gridcells = gridcells,
        title = title,
        subtitle = subtitle,
        cols = cols,
        col.by = col.by,
        col.labels = col.labels,
        linetypes = linetypes,
        linetype.by = linetype.by,
        linetype.labels = linetype.labels,
        linewidths = linewidths,
        linewidth.by = linewidth.by,
        linewidth.labels = linewidth.labels,
        sizes = sizes,
        size.by = size.by,
        size.labels = size.labels,
        shapes = shapes,
        shape.by = shape.by,
        shape.labels = shape.labels,
        alphas = alphas,
        alpha.by = alpha.by,
        alpha.labels = alpha.labels,
        y.label = y.label,
        y.lim = y.lim,
        x.label = x.label,
        x.lim = x.lim,
        legend.position = legend.position,
        text.multiplier = text.multiplier,
        dropEmpty = dropEmpty,
        plotTrend = plotTrend,
        plot = FALSE # Force plotTemporal to return a dataframe
    )

    fields <- DGVMTools:::santiseFieldsForPlotting(gc)
    layers <- DGVMTools:::santiseLayersForPlotting(fields, layers)

    # Threshold of NA values as a fraction of the "number of values" below which
    # we use points instead of lines. "Number of values" of a particular layer
    # here means the total number of rows (ie timesteps) between the first and
    # last non-NA value in that layer.
    POINTS_THRESHOLD <- 0.1

    # Threshold of number of data points below which we use points instead of
    # lines.
    NVALUE_THRESHOLD <- 30

    # Track which layers should use points vs lines
    sparse_layers <- c()
    for (lyr in layers) {
        # Get the index of the first and last rows with non-NA values in this
        # layer.
        lyr_rows <- which(!is.na(gc@data[[lyr]]))
        if (length(lyr_rows) < 2) {
            # No valid data - consider it sparse.
            sparse_layers <- c(sparse_layers, lyr)
            next
        }
        first <- lyr_rows[1]
        last <- lyr_rows[length(lyr_rows)]

        # Get all rows between the first and last non-NA rows.
        layer_data <- gc@data[first:last, ]

        # Calculate fraction of non-NA data points within this period
        nvalue <- length(which(!is.na(layer_data[[lyr]])))
        data_frac <- nvalue / nrow(layer_data)
        log_debug("Layer ", lyr, " has ", data_frac,
                 " fraction of non-NA data points (",
                 nvalue, "/", nrow(layer_data), ") (rows ", first, ":", last,
                 ")")
        if (data_frac < POINTS_THRESHOLD) {
            log_debug("Layer ", lyr, " is sparse (data fraction ", data_frac,
                     " < ", POINTS_THRESHOLD, "). Ergo, we use points.")
            sparse_layers <- c(sparse_layers, lyr)
        } else if (nvalue < NVALUE_THRESHOLD) {
            log_debug("Layer ", lyr, " has too few data points (", nvalue,
                     " < ", NVALUE_THRESHOLD, "). Ergo, we use points.")
            sparse_layers <- c(sparse_layers, lyr)
        }
    }

    ### PLOT! - now make the plot

    # first make the "symbols" for the ggplot2 call.  A bit of a pain -since they ggplot2 folks took away aes_string()- but what can you do...
    col.sym <- if(is.character(col.by)) ensym(col.by) else NULL
    alpha.sym <- if(is.character(alpha.by))  ensym(alpha.by) else NULL
    size.sym <- if(is.character(size.by)) ensym(size.by) else  NULL
    shape.sym <- if(is.character(shape.by)) ensym(shape.by) else NULL
    linewidth.sym <- if(is.character(linewidth.by)) ensym(linewidth.by) else NULL
    linetype.sym <- if(is.character(linetype.by)) ensym(linetype.by) else  NULL

    # Base plot with time on x-axis
    p <- ggplot(data.toplot, aes(x = Time,
                                 y = Value,
                                 col = !! col.sym,
                                 alpha = !! alpha.sym,
                                 size = !! size.sym,
                                 shape = !! shape.sym,
                                 linetype = !! linetype.sym,
                                 linewidth = !! linewidth.sym))

    # Add layers in original order, using points or lines as appropriate
    for (lyr in layers) {
        layer_data <- data.toplot[data.toplot$Layer == lyr, ]
        # Filter out NAs.
        layer_data <- layer_data[!is.na(layer_data$Value), ]
        is_sparse <- lyr %in% sparse_layers

        # build arguments for aesthetics to geom_line/geom_line and/or fixed arguments outside
        geom_args <- list()
        geom_args[["data"]] <- layer_data
        # col and alpha (for both geom_points and geom_line)
        if(!is.null(cols) && is.null(col.by)) geom_args[["col"]] <- cols
        if(!is.null(alphas) && is.null(alpha.by)) geom_args[["alpha"]] <- alphas
        # for points only.
        if (is_sparse) {
            if (!is.null(shapes) && is.null(shape.by)) geom_args[["shape"]] <- shapes
            if (!is.null(sizes) && is.null(size.by)) geom_args[["size"]] <- sizes
        }
        # for lines only
        else {
            if (!is.null(linetypes) && is.null(linetype.by)) geom_args[["linetype"]] <- linetypes
            if (!is.null(linewidths) && is.null(linewidth.by)) geom_args[["linewidth"]] <- linewidths
        }

        func <- NULL
        if (is_sparse) {
            func <- ggplot2::geom_point
            # p <- p + geom_point(
            #     data = layer_data,
            #     aes(
            #         col = !!col.sym,
            #         alpha = !!alpha.sym,
            #         size = !!size.sym,
            #         shape = !!shape.sym
            #     )
            # )
        } else {
            func <- ggplot2::geom_line
            # p <- p + geom_line(
            #     data = layer_data,
            #     aes(
            #         col = !!col.sym,
            #         alpha = !!alpha.sym,
            #         linewidth = !!linewidth.sym,
            #         linetype = !!linetype.sym
            #     )
            # )
        }
        p <- p + do.call(func, geom_args)
    }

    # apply labels
    if (!is.null(col.by) & !is.null(cols)) p <- p + scale_color_manual(values=cols, labels=col.labels)
    if (!is.null(alpha.by) & !is.null(alphas)) p <- p + scale_alpha_manual(values=alphas, labels=alpha.labels)
    if (length(sparse_layers) > 0) {
        if (!is.null(size.by) & !is.null(sizes)) p <- p + scale_size_manual(values=sizes, labels=size.labels)
        if (!is.null(shape.by) & !is.null(shapes)) p <- p + scale_shape_manual(values=shapes, labels=shape.labels)
    } else {
        if (!is.null(linewidth.by) & !is.null(linewidths)) p <- p + scale_linewidth_manual(values=linewidths, labels=linewidth.labels)
        if (!is.null(linetype.by) & !is.null(linetypes)) p <- p + scale_linetype_manual(values=linetypes, labels=linetype.labels)
    }

    # set the theme to theme_bw, simplest way to set the background to white
    p <- p + theme_bw()

    # labels and positioning
    p <- p + labs(title = title, subtitle = subtitle)

    p <- p + theme(legend.title=element_blank())
    p <- p + theme(legend.position = legend.position, legend.key.size = unit(2, 'lines'))
    p <- p + theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5))

    # overall text multiplier
    if (!is.null(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))

    # set limits
    if (!is.null(x.lim)) p <- p + xlim(x.lim)
    if (!is.null(y.lim)) p <- p + scale_y_continuous(limits = y.lim, name = y.label)
    else p <- p + labs(y = y.label)

    if (!is.null(x.label)) p <- p + labs(x = x.label)

    # all column names, used a lot below
    all.columns <- names(data.toplot)

    if (!missing(col.by) && !is.null(col.by) && !col.by %in% all.columns) stop(paste("Colouring by", col.by, "requested, but that is not available, so failing."))
    if (!missing(linetype.by) && !is.null(linetype.by) && !linetype.by %in% all.columns) stop(paste("Setting linetypes by", linetype.by, "requested, but that is not available, so failing."))
    if (!missing(linewidth.by) && !is.null(linewidth.by) && !linewidth.by %in% all.columns) stop(paste("Setting linewidth by", linewidth.by, "requested, but that is not available, so failing."))
    if (!missing(size.by) && !is.null(size.by) && !size.by %in% all.columns) stop(paste("Setting sizes by", size.by, "requested, but that is not available, so failing."))
    if (!missing(shape.by) && !is.null(shape.by) && !shape.by %in% all.columns) stop(paste("Setting shapes by", shape.by, "requested, but that is not available, so failing."))
    if (!missing(alpha.by) && !is.null(alpha.by) && !alpha.by %in% all.columns) stop(paste("Setting alphas by", alpha.by, "requested, but that is not available, so failing."))

    # ar first assume facetting by everything except for...
    dontFacet <- c("Value", "Time", "Year", "Month", "Season", "Day", "Lon", "Lat", col.by, linetype.by, linewidth.by,  size.by, shape.by, alpha.by)
    vars.facet <- all.columns[!all.columns %in% dontFacet]

    # then remove facets with only one unique value
    for (this.facet in vars.facet) {
        if (length(unique(data.toplot[[this.facet]])) == 1) {
            vars.facet <- vars.facet[!vars.facet == this.facet]
        }
    }

    # facetting
    if (length(vars.facet > 0)) {
        suppressWarnings(p <- p + facet_wrap(vars.facet, ...))
    }

    return(p)
}

plot_timeseries <- function(
    gc,
    ylim = NULL,
    text_multiplier = NULL,
    xlab = NULL,
    ylab = NULL,
    layers = NULL,
    subtitle = NULL,
    colours = NULL,
    allow_points = TRUE) {

    gc <- data.table::as.data.table(gc)
    if (!("value" %in% names(gc))) {
        log_error("Unable to plot: data is not in read_data() long format")
    }
    if (!is.null(layers)) {
        gc <- gc[variable %in% layers]
    }
    if (nrow(gc) == 0) {
        log_error("Unable to plot: no rows are available")
    }
    gc[, series := get_series_labels(.SD)]

    ncolour <- length(unique(gc$series))
    if (is.null(colours)) {
        colours <- get_colour_palette(ncolour)
        colours <- setNames(colours, unique(gc$series))
    }

    plt <- ggplot2::ggplot(gc[!is.na(value)],
                           ggplot2::aes(x = date, y = value, color = series,
                                        group = series)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = xlab, y = ylab, title = NULL, subtitle = subtitle) +
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())
    if (!is.null(colours)) {
        plt <- plt + ggplot2::scale_color_manual(values = colours)
    }
    if (!is.null(ylim)) {
        plt <- plt + ggplot2::scale_y_continuous(limits = ylim)
    }
    plt <- set_text_multiplier(plt, text_multiplier)
    return(plt)
}

get_series_labels <- function(data) {
    data <- data.table::as.data.table(data)
    components <- intersect(c("source", "quantity", "variable"), names(data))
    components <- components[vapply(components, function(column) {
        length(unique(data[[column]])) > 1
    }, logical(1))]
    if (length(components) == 0) {
        return(rep("value", nrow(data)))
    }
    labels <- do.call(paste, c(data[, components, with = FALSE], sep = ": "))
    as.character(labels)
}

filter_default_plot_layers <- function(data, sources, vars, sites) {
    data <- data.table::as.data.table(data)
    source_ids <- vapply(sources, function(source) source@id, character(1))
    keep <- !(data$source %in% source_ids)
    any_defaults <- FALSE
    for (source in sources) {
        for (var in vars) {
            default_layers <- get_default_layers(source, var, sites$Name)
            if (is.null(default_layers)) {
                keep <- keep | (data$source == source@id & data$quantity == var@id)
            } else {
                any_defaults <- TRUE
                keep <- keep | (data$source == source@id &
                                data$quantity == var@id &
                                data$variable %in% default_layers)
            }
        }
    }
    if (!any_defaults) {
        return(data)
    }
    data[keep]
}

get_entity_columns <- function(data, entity_level = "auto") {
    entity_level <- match.arg(entity_level,
                              c("auto", "stand", "patch", "indiv", "gridcell"))
    available <- intersect(c("stand", "patch", "indiv"), names(data))
    if (entity_level == "gridcell" || length(available) == 0) {
        return(character(0))
    }
    if (entity_level == "auto") {
        if ("indiv" %in% available) return(c("stand", "patch", "indiv"))
        if ("patch" %in% available) return(c("stand", "patch"))
        if ("stand" %in% available) return("stand")
        return(character(0))
    }
    levels <- c("stand", "patch", "indiv")
    selected <- levels[seq_len(match(entity_level, levels))]
    intersect(selected, available)
}

add_entity_column <- function(data, entity_level = "auto") {
    data <- data.table::as.data.table(data)
    entity_cols <- get_entity_columns(data, entity_level)
    if (length(entity_cols) == 0) {
        data[, entity := "gridcell"]
        return(data)
    }
    labels <- lapply(entity_cols, function(column) {
        paste(column, data[[column]])
    })
    data[, entity := do.call(paste, c(labels, sep = " / "))]
    data
}

add_site_column <- function(data, sites = NULL) {
    data <- data.table::as.data.table(data)
    if ("Site" %in% names(data) && any(!is.na(data$Site))) {
        data[is.na(Site), Site := "Unknown"]
        return(data)
    }
    if (is.null(sites) || nrow(sites) == 0 ||
        !all(c("Lon", "Lat", "Name") %in% names(sites))) {
        data[, Site := "Site"]
        return(data)
    }
    data[, Site := NA_character_]
    tol <- get_global("merge_tol")
    for (i in seq_len(nrow(sites))) {
        data[abs(Lon - sites$Lon[[i]]) < tol & abs(Lat - sites$Lat[[i]]) < tol,
             Site := sites$Name[[i]]]
    }
    data[is.na(Site), Site := "Unknown"]
    data
}

normalise_layerwise_mapping <- function(mapping, allowed, arg_name,
                                        allow_none = TRUE) {
    if (is.null(mapping)) {
        return(NULL)
    }
    if (allow_none) {
        mapping <- setdiff(mapping, "none")
    }
    if (length(mapping) == 0) {
        return(NULL)
    }
    missing <- setdiff(mapping, allowed)
    if (length(missing) > 0) {
        stop(arg_name, " contains unknown column(s): ",
             paste(missing, collapse = ", "))
    }
    mapping
}

plot_layerwise_table <- function(
    data,
    facet_by = "variable",
    entity_layout = c("series", "facet", "aggregate"),
    entity_level = c("auto", "stand", "patch", "indiv", "gridcell"),
    colour_by = "source",
    linetype_by = NULL,
    shape_by = NULL,
    geometry = c("auto", "line", "point", "line_point"),
    axis = c("free", "shared"),
    title = NULL,
    ylab = NULL) {

    data <- data.table::as.data.table(data)
    entity_layout <- match.arg(entity_layout)
    entity_level <- match.arg(entity_level)
    geometry <- match.arg(geometry)
    axis <- match.arg(axis)

    if (!("value" %in% names(data))) {
        log_error("Unable to plot: data is not in read_data() long format")
    }
    if (nrow(data) == 0) {
        log_error("Unable to plot: no rows are available")
    }

    data <- data.table::copy(data)
    data <- add_entity_column(data, entity_level)

    allowed_mappings <- intersect(c("source", "quantity", "variable", "layer",
                                    "Site", "entity", "pft"),
                                  names(data))
    facet_by <- normalise_layerwise_mapping(facet_by, allowed_mappings,
                                            "facet_by")
    colour_by <- normalise_layerwise_mapping(colour_by, allowed_mappings,
                                             "colour_by")
    linetype_by <- normalise_layerwise_mapping(linetype_by, allowed_mappings,
                                               "linetype_by")
    shape_by <- normalise_layerwise_mapping(shape_by, allowed_mappings,
                                            "shape_by")

    if (entity_layout == "facet") {
        facet_by <- unique(c(facet_by, "entity"))
    } else if (entity_layout == "aggregate") {
        grouping <- unique(c("date", "Year", "Day", "Lon", "Lat", "Site",
                             "source", "quantity", "variable", "layer"))
        grouping <- intersect(grouping, names(data))
        data <- data[!is.na(value),
                     .(value = mean(value, na.rm = TRUE)),
                     by = grouping]
        data[, entity := "aggregate"]
    }

    facet_by <- facet_by[vapply(facet_by, function(column) {
        length(unique(data[[column]])) > 1
    }, logical(1))]

    group_cols <- unique(c("source", "quantity", "variable", "layer", "Site",
                           "pft", if (entity_layout == "series") "entity",
                           colour_by, linetype_by, shape_by))
    group_cols <- intersect(group_cols, names(data))
    data[, group_id := do.call(paste, c(.SD, sep = " | ")), .SDcols = group_cols]

    mapping <- ggplot2::aes(x = date, y = value, group = group_id)
    if (!is.null(colour_by)) {
        mapping$colour <- as.name(colour_by)
    }
    if (!is.null(linetype_by)) {
        mapping$linetype <- as.name(linetype_by)
    }
    if (!is.null(shape_by)) {
        mapping$shape <- as.name(shape_by)
    }

    plot_data <- data[!is.na(value)]
    plt <- ggplot2::ggplot(plot_data, mapping) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Date", y = ylab, title = title) +
        ggplot2::theme(legend.position = "bottom")

    if (geometry == "line") {
        plt <- plt + ggplot2::geom_line()
    } else if (geometry == "point") {
        plt <- plt + ggplot2::geom_point()
    } else if (geometry == "line_point") {
        plt <- plt + ggplot2::geom_line() + ggplot2::geom_point()
    } else {
        group_counts <- plot_data[, .N, by = group_id]
        sparse_groups <- group_counts[N < 30]$group_id
        line_data <- plot_data[!(group_id %in% sparse_groups)]
        point_data <- plot_data[group_id %in% sparse_groups]
        if (nrow(line_data) > 0) {
            plt <- plt + ggplot2::geom_line(data = line_data)
        }
        if (nrow(point_data) > 0) {
            plt <- plt + ggplot2::geom_point(data = point_data)
        }
    }

    if (length(facet_by) > 0) {
        scales <- if (axis == "free") "free_y" else "fixed"
        formula <- stats::as.formula(paste("~", paste(facet_by, collapse = "+")))
        plt <- plt + ggplot2::facet_wrap(formula, scales = scales)
    }

    plt
}

plot_pvo <- function(
    gc,
    ylim = NULL,
    text_multiplier = NULL,
    marker_size = 3,
    colours = NULL,
    obs_name = NULL,
    units = NULL) {

    gc <- data.table::as.data.table(gc)
    if (is.null(obs_name)) {
        obs_name <- get_global("obs_lyr")
    }

    obs <- gc[source == obs_name]
    pred <- gc[source != obs_name]
    if (nrow(obs) == 0 || nrow(pred) == 0) {
        log_error("Unable to plot: observed and predicted rows are required")
    }

    key_cols <- intersect(c("Lon", "Lat", "Year", "Day", "Site", "stand",
                            "patch", "indiv", "quantity"),
                          names(gc))
    obs <- obs[, c(key_cols, "value"), with = FALSE]
    data.table::setnames(obs, "value", "observed")
    pred <- pred[, c(key_cols, "source", "variable", "value"), with = FALSE]
    data <- merge(pred, obs, by = key_cols, allow.cartesian = TRUE)
    if (nrow(data) == 0) {
        log_error("Unable to plot: predictions do not overlap observations")
    }

    # Initialise the plot object.
    data[, series := get_series_labels(.SD)]
    plt <- ggplot2::ggplot(data, ggplot2::aes(x = observed, y = value,
                                              color = series))
    plt <- plt + ggplot2::geom_point(size = marker_size) + ggplot2::theme_bw()

    # Set correct colours (use named mapping when available).
    if (!is.null(colours)) {
        pred_cols <- colours[intersect(names(colours), unique(data$series))]
        if (length(pred_cols) > 0) {
            plt <- plt + ggplot2::scale_color_manual(values = pred_cols)
        }
    }

    # Set text size.
    plt <- set_text_multiplier(plt, text_multiplier)

    if (is.null(units) || !nzchar(units)) {
        metadata <- resolve_quantity_metadata(gc$quantity[[1]])
        if (!is.null(metadata) && !is.null(metadata$units) &&
            nzchar(metadata$units)) {
            units <- metadata$units
        } else {
            units <- ""
        }
    }
    units <- DGVMTools:::standardiseUnitString(units)
    x_label <- obs_name
    y_label <- "Predicted"
    if (units != "1" && units != "") {
        x_label <- paste0(x_label, " (", units, ")")
        y_label <- paste0(y_label, " (", units, ")")
    }

    plt <- plt + ggplot2::labs(y = DGVMTools:::stringToExpression(y_label),
                               x = DGVMTools:::stringToExpression(x_label))

    # Plot 1:1 line.
    plt <- plt + geom_abline(slope = 1, intercept = 0)

    # plt <- plt + geom_point(, alpha = alpha)

    # scatter.plot <- scatter.plot + theme(legend.position = legend.position, legend.key.size = unit(2, 'lines'))

  # Apply x/y axis limits.
    if(!is.null(ylim)) {
        plt <- plt + xlim(ylim)
        plt <- plt + scale_y_continuous(limits = ylim)
    }

    return(plt)
}

plot_subannual <- function(
    gc,
    ylim = NULL,
    text_multiplier = NULL,
    colours = NULL) {

    gc <- data.table::as.data.table(gc)
    if (!("Day" %in% names(gc))) {
        log_error("Unable to plot subannual cycle: data has no Day column")
    }
    data <- gc[!is.na(value),
               .(value = mean(value, na.rm = TRUE)),
               by = .(Day, source, quantity, variable)]
    data[, series := get_series_labels(.SD)]

    if (is.null(colours)) {
        colours <- get_colour_palette(length(unique(data$series)))
        colours <- setNames(colours, unique(data$series))
    }

    plt <- ggplot2::ggplot(data, ggplot2::aes(x = Day, y = value,
                                              color = series,
                                              group = series)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Day") +
        ggplot2::scale_color_manual(values = colours)

    if (!is.null(ylim)) {
        plt <- plt + ggplot2::scale_y_continuous(limits = ylim)
    }

    plt <- set_text_multiplier(plt, text_multiplier)

    blank <- ggplot2::element_blank()
    plt <- plt + theme(legend.position = "bottom", legend.title = blank)

    return(plt)
}

get_stats_lyr_name <- function(var_name, src_name) {
    return(paste0(gsub("dave_", "", var_name), "_", tolower(src_name)))
}

get_stats_lyr <- function(var, src) {
    return(get_stats_lyr_name(var@id, src@id))
}

create_plots <- function(gc, ylab, ncol = 2, use_plotly = TRUE
        , text_multiplier = NULL, do_timeseries = TRUE, do_pvo = TRUE
        , do_subannual = TRUE, marker_size = 3, ylim = NULL
        , obs_lyr = NULL) {

    gc <- data.table::as.data.table(gc)
    quantity_id <- unique(gc$quantity)
    quantity_id <- quantity_id[!is.na(quantity_id)][[1]]
    if (is.null(ylab)) {
        ylab <- quantity_id
    }

    quantity_units <- ""
    metadata <- resolve_quantity_metadata(quantity_id)
    if (!is.null(metadata) && !is.null(metadata$units) &&
        nzchar(metadata$units)) {
        quantity_units <- metadata$units
    }

    # Compute (and store) statistics).
    if (is.null(obs_lyr)) {
        log_debug("No observed layer was specified, so will attempt to find one")
        readers <- find_readers_for_var(trim_dave(quantity_id))
        log_debug("Found ", length(readers), " readers")
        if (length(readers) > 0) {
            reader_names <- names(readers)
            src_ids <- unlist(lapply(readers, function(r) r@src@id))
            reader_names <- unique(c(reader_names, src_ids))
            log_debug("Reader names: ", paste(reader_names, collapse = ", "))
            avail_readers <- reader_names[reader_names %in% unique(gc$source)]
            log_debug("Available readers: ", paste(avail_readers, collapse = ", "))
            if (length(avail_readers) > 0) {
                # Choose the reader with the most data points.
                nvalues <- sapply(avail_readers, function(r)
                    nrow(gc[source == r & !is.na(value)]))
                obs_lyr <- avail_readers[which.max(nvalues)]
                log_debug("Using reader ", obs_lyr,
                          " as default, as it has the most data points")
            }
        }
        if (is.null(obs_lyr)) {
            log_debug("No available readers found, so will use global default")
            obs_lyr <- get_global("obs_lyr")
        }
    }
    if (!(obs_lyr %in% unique(gc$source))) {
        log_warning("Observed layer '", obs_lyr,
                    "' is not present in data; disabling observed-dependent plots")
        obs_lyr <- NULL
    }
    log_debug("obs_lyr=", obs_lyr)

    pred_sources <- setdiff(unique(gc$source), obs_lyr)
    if (length(pred_sources) == 0) {
        log_error("Unable to plot: data contains no predicted sources")
    }

    r2 <- list()
    rmse <- list()
    nse <- list()
    rsr <- list()
    bias <- list()
    # Compute stats for each source.
    if (!is.null(obs_lyr)) {
        key_cols <- intersect(c("Lon", "Lat", "Year", "Day", "Site", "stand",
                                "patch", "indiv", "quantity"),
                              names(gc))
        obs_data <- gc[source == obs_lyr, c(key_cols, "value"), with = FALSE]
        data.table::setnames(obs_data, "value", "observed")
        for (name in pred_sources) {
            log_debug("Computing stats with observed source='", obs_lyr,
                      "', predicted source='", name, "'")
            pred_data <- gc[source == name, c(key_cols, "value"), with = FALSE]
            df <- merge(pred_data, obs_data, by = key_cols)
            df <- df[!is.na(observed) & !is.na(value)]
            obs <- df$observed
            pred <- df$value
            lyr_name <- get_stats_lyr_name(quantity_id, name)

            r2[[lyr_name]] <- if (length(obs) > 0) compute_r2(obs, pred) else NA
            rmse[[lyr_name]] <- if (length(obs) > 0) compute_rmse(obs, pred) else NA
            nse[[lyr_name]] <- if (length(obs) > 0) compute_nse(obs, pred) else NA
            rsr[[lyr_name]] <- if (length(obs) > 0) compute_rsr(obs, pred) else NA
            bias[[lyr_name]] <- if (length(obs) > 0) compute_bias(obs, pred) else NA
        }
    } else {
        log_debug("Skipping stats: no observed layer available")
    }

    # Apparently a variable name in the j value will be interpreted literally
    # rather than dereferencing the value stored in the variable. What an
    # amazing language this is.
    if (is.null(ylim)) {
        ylim <- get_ylim(gc)
    }

    colours <- c()
    labelled_gc <- data.table::copy(gc)
    labelled_gc[, series := get_series_labels(labelled_gc)]
    if (!is.null(obs_lyr)) {
        obs_series <- unique(labelled_gc[source == obs_lyr]$series)
        colours[obs_series] <- "#000000"
    }
    pred_series <- unique(labelled_gc[source %in% pred_sources]$series)
    if (length(pred_series) > 0) {
        pred_cols <- get_colour_palette(length(pred_series))
        pred_cols <- setNames(pred_cols, pred_series)
        colours <- c(colours, pred_cols)
    }

    # Create plots.
    result <- list()

    if (do_timeseries) {
        log_diag("Creating timeseries plot...")
        timeseries <- plot_timeseries(gc, ylim, text_multiplier,
                                      colours = colours)
        timeseries <- trim_ggplot(timeseries, xlab = TRUE)
        if (use_plotly) {
            timeseries <- to_plotly(timeseries, x_title = "Date",
                                    y_title = ylab)
        }
        result$timeseries <- timeseries
    }
    if (do_pvo && !is.null(obs_lyr)) {
        log_diag("Creating predicted vs. observed scatter plot...")
        pvo <- plot_pvo(gc, ylim, text_multiplier, marker_size = marker_size,
                        colours = colours, obs_name = obs_lyr,
                        units = quantity_units)
        # On the subannual plots, we want the x-axis label (observed) as well
        # as the y-axis label (predicted), because these are not shared with any
        # other plots in the panel.
        pvo <- trim_ggplot(pvo, xlab = TRUE, ylab = TRUE)
        if (use_plotly) {
            pvo <- to_plotly(pvo)
        }
        result$pvo <- pvo
    } else if (do_pvo) {
        log_warning("Skipping predicted-vs-observed plot: no observed layer was found")
    }
    if (do_subannual) {
        log_diag("Creating subannual plot...")
        subannual <- plot_subannual(gc, ylim, text_multiplier,
                                    colours = colours)
        subannual <- trim_ggplot(subannual, xlab = TRUE)
        if (use_plotly) {
            subannual <- to_plotly(subannual)
        }
        result$subannual <- subannual
    }

    # Save plots (we're looping over variables first, because it's faster to
    # read/process the data this way, but we want to group plots by site, rather
    # than by variable).
    # combined <- create_panel(timeseries, pvo, subannual, use_plotly, ncol)

    # class_name <- if (use_plotly) "PlotlyPanel" else "ggplotPanel"

    result$r2 <- r2
    result$rmse <- rmse
    result$nse <- nse
    result$rsr <- rsr
    result$bias <- bias

    return(result)
}

set_title <- function(plt, title, use_plotly = FALSE) {
    if (use_plotly) {
        return(plotly::layout(plt, title = title))
    } else {
        return(plt + ggplot2::labs(title = title))
    }
}

trim_ggplot <- function(
    plt,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = FALSE,
    ylab = FALSE) {

    result <- plt

    if (!is.null(title)) {
        log_debug("Adding title to plot: '", title, "'...")
        result <- result + ggplot2::labs(title = title)
    }
    if (!is.null(subtitle)) {
        log_debug("Adding subtitle to plot: '", subtitle, "'...")
        result <- result + ggplot2::labs(subtitle = subtitle)
    }
    if (!is.null(caption)) {
        log_debug("Adding caption to plot: '", caption, "'...")
        result <- result + ggplot2::labs(caption = caption)
    }

    # log_debug("Annotating ggplot; title = '", title, "'; subtitle = '", subtitle, "'; caption = '", caption, "'.")
    # result <- result + ggplot2::labs(title = title, subtitle = subtitle,
    #                                  caption = caption)

    if (!xlab) {
        log_debug("Removing x-axis label from plot...")
        result <- result + ggpubr::rremove("xlab")
    }
    if (!ylab) {
        log_debug("Removing y-axis label from plot")
        result <- result + ggpubr::rremove("ylab")
    }
    return(result)
}

format_plotly <- function(
    plt,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = FALSE,
    ylab = FALSE
) {
    plt <- plotly::layout(plt,
        legend = list(orientation = "h"),
        title = title
    )
    if (!xlab) {
        plt <- plotly::layout(plt, xaxis = list(title = ""))
    }
    if (!ylab) {
        plt <- plotly::layout(plt, yaxis = list(title = ""))
    }
    return(plt)
}

format_plot <- function(
    plt,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = FALSE,
    ylab = FALSE,
    use_plotly = FALSE
) {
    if (use_plotly) {
        return(format_plotly(plt, title, subtitle, caption, xlab, ylab))
    } else {
        return(trim_ggplot(plt, title, subtitle, caption, xlab, ylab))
    }
}

get_y_label <- function(var, site = NULL) {
    var <- sanitise_variable(var)
    metadata <- resolve_quantity_metadata(var@id)

    name <- var@name
    units <- var@units
    if (!is.null(metadata)) {
        if (!is.null(metadata$name) && nzchar(metadata$name)) {
            name <- metadata$name
        }
        if (!is.null(metadata$units) && nzchar(metadata$units)) {
            units <- metadata$units
        }
    }

    ylab <- trim_dave(name)
    if (!is.null(site)) {
      ylab <- paste(site, ylab)
    }
    if (!is.null(units) && units != "") {
        ylab <- paste0(ylab, " (", units, ")")
    }
    return(ylab)
}

get_y_label_from_vars <- function(vars) {
    if (length(vars) == 1) {
        return(get_y_label(vars[[1]]))
    }
    return(NULL)
}

dave_panel <- function(plots, xlab, ylab, title, use_plotly, sites) {
    if (use_plotly) {
        plotlies <- list()
        nplot <- length(plots)
        nrow <- as.integer(ceiling(sqrt(nplot)))
        for (plot in plots) {
            site_name <- sites[length(plotlies) + 1, "Name"]
            p <- plotly::ggplotly(plot)
            p <- plotly::layout(p, legendgroup = "Layout"
                , yaxis = list(title = ""))
            p <- plotly::layout(p,
                annotations = list(
                    x = 0.5, y = 1.05, text = site_name, showarrow = FALSE,
                    xref = "paper", yref = "paper"
                )
            )
            plotlies[[length(plotlies) + 1]] <- p
        }
        plt <- plotly::subplot(plotlies, nrows = nrow, shareY = TRUE
            , titleY = FALSE)
        plt <- plotly::layout(plt,
            yaxis = list(title = ylab),
            legend = list(orientation = "h"),
            title = title
        )
        # plt <- plt %>% plotly::layout(
        #     margin = list(l = 60),
        #     annotations = list(
        #         x = -0.1, y = 0.5, text = ylab, textangle = -90,
        #         showarrow = FALSE, xref = "paper", yref = "paper")
        # )
        for (layer in seq_len(length(plt$x$data) * (nplot - 1L) / nplot)) {
            plt$x$data[[layer]]$showlegend <- FALSE
        }
        return(plt)
    } else {
        gp <- grid::gpar(cex = 1.3)
        panel <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE
            , legend = "bottom")
        panel <- ggpubr::annotate_figure(panel,
            top = grid::textGrob(title, gp = gp),
            left = grid::textGrob(ylab, rot = 90, vjust = 1, gp = gp),
            bottom = grid::textGrob(xlab, gp = gp))
        return(panel)
    }
}

convert_plot <- function(plt, to_plotly) {
    if (to_plotly) {
        plt <- plotly::ggplotly(plt)
        plt <- plotly::layout(plt, legend = list(orientation = "h"))
        return(plt)
    } else {
        return(plt)
    }
}

get_title <- function(title) {
    if (title == "live_biomass") {
        return("Live Biomass")
    }
    if (title == "height") {
        return("Mean Vegetation Height")
    }
    if (title == "diameter") {
        return("Mean Stem Diameter")
    }
    return(title)
}

ozflux_plot_site <- function(
        data,
        ylim,
        site,
        separate,
        use_plotly,
        nsite,
        vars,
        xlab = NULL,
        ylab = NULL) {
    log_diag("Plotting ", site$Name, "...")

    # Extract data for the required grid cell.
    gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)
    if (nrow(gridcell) == 0) {
        log_error("No data found for site ", site$Name, " (", site$Lon, ", "
            , site$Lat, ")")
    }

    # Plot the gridcell.
    plt <- plot_timeseries(gridcell, ylim, xlab = xlab, ylab = ylab)

    # The plot title should not include the variable name if plotting a panel
    # of sites. In that case it's better to have the variable name as the main
    # title above the panel, and use the site name for the subplot titles.
    title <- site$Name
    if (length(vars) == 1 && nsite == 1) {
        name <- trim_dave(vars[[1]]@name)
        name <- get_title(name)
        title <- paste(title, name)
    }

    # Keep the axis titles iff plotting 1 site. In a multi-site (ie panel)
    # scenario, we want a single axis label shared by all the sites.
    keep_xaxis <- nsite == 1
    keep_yaxis <- nsite == 1

    # Remove redundant plot elements.
    plt <- trim_ggplot(plt, title, xlab = keep_xaxis, ylab = keep_yaxis)

    # Convert to plotly (if necessary).
    plt <- convert_plot(plt, use_plotly)

    return(plt)
}

get_panel_title <- function(vars) {
    if (length(vars) == 1) {
        return(trim_dave(vars[[1]]@name))
    }
    return(NULL)
}

#'
#' Calculate a date from a year, day and optional timestep.
#' @param data A data frame containing `Year`, optionally `Day` (zero-based)
#' and `timestep` (zero-based within each day).
#' @return A `Date` vector for annual and daily records, or a UTC `POSIXct`
#' vector for subdaily records.
#'
#' @keywords internal
#'
calc_date <- function(data) {
    if (!("Year" %in% names(data))) {
        stop("Data must contain a 'Year' column.")
    }
    if (!("Day" %in% names(data))) {
        return(as.Date(paste0(data$Year, "-12-31")))
    }

    date <- as.Date(paste0(data$Year, "-01-01")) + data$Day
    if (!("timestep" %in% names(data))) {
        return(date)
    }

    # A zero-based timestep index gives the number of steps in a full day.
    steps_per_day <- if (all(is.na(data$timestep))) NA_real_ else
        max(data$timestep, na.rm = TRUE) + 1
    as.POSIXct(date, tz = "UTC") + data$timestep * (86400 / steps_per_day)
}
