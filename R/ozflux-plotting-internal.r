
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
    if (n > length(get_global("cb_colours"))) {
        log_warning("Unable to get Wong et al colour palette with ", n
            , " colours. Default colours will be used instead.")
        return(NULL)
    }
    colours <- get_global("cb_colours")[1:n]
    log_debug("Successfully generated colour palette with ", n, " colours")
    return(colours)
}

to_plotly <- function(chart, lyr_name) {
    res <- plotly::ggplotly(chart)
    res <- plotly::layout(res,
        xaxis = list(title = list(text = "Date")),
        yaxis = list(title = list(text = lyr_name)),
        legend = list(bgcolor = "rgba(0,0,0,0)"))
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

    ncolour <- ifelse(is.null(layers), length(names(gc)), length(layers))
    if (is.null(colours)) {
        colours <- get_colour_palette(ncolour)
    }

    non_data_layers <- c("Site")
    if (is.null(layers)) {
        lyrs <- setdiff(names(gc), non_data_layers)
    } else {
        lyrs <- layers
    }

    chr_layers <- c()
    for (lyr in lyrs) {
        if (class(gc@data[[lyr]]) == "character") {
            chr_layers <- c(chr_layers, lyr)
        }
    }
    lyrs <- setdiff(lyrs, chr_layers)

    return(plot_temporal(gc, layers = lyrs, cols = colours
        , text.multiplier = text_multiplier, text.expression = FALSE
        , y.lim = ylim, x.label = xlab, y.label = ylab, subtitle = NULL
        , title = NULL))
}

plot_pvo <- function(
    gc,
    ylim = NULL,
    text_multiplier = NULL,
    marker_size = 3,
    colours = NULL,
    obs_name = NULL) {

    if (is.null(colours)) {
        colours <- get_colour_palette(length(names(gc)))
    }

    # predicted_name <- names(gc)[[length(names(gc))]]
    log_debug("[plot_pvo] names(gc) = ", paste(names(gc), collapse = ", "))
    # plt <- DGVMTools::plotScatter(gc, layer.x = get_global("obs_lyr")
    #     , layer.y = predicted_name, text.multiplier = text_multiplier)
    if (is.null(obs_name)) {
        obs_name <- get_global("obs_lyr")
    }

    readers <- find_readers_for_var(trim_dave(gc@quant@id))
    if (!obs_name %in% names(gc@data) && length(readers) > 0) {
        obs_name <- names(readers)[1]
        log_debug("Using reader ", obs_name, " as default for site ", row$Name)
    }

    ignored_names <- c("Site", obs_name)
    ynames <- setdiff(names(gc), ignored_names)
    log_debug("[plot_pvo] ynames = ", paste(ynames, collapse = ", "))
    # Filter out non-numeric layers.
    ynames <- ynames[unlist(lapply(ynames, function(n) is.numeric(gc@data[[n]])))]
    log_debug("[plot_pvo] ynames (numeric) = ", paste(ynames, collapse = ", "))
    if (length(ynames) == 0) {
        log_error("Unable to plot: data contains no numeric non-observed layers")
    }

    # alpha <- 1
    data <- gc@data
    data <- data %>% tidyr::pivot_longer(cols = ynames, names_to = "layer", values_to = "value")

    # Initialise the plot object.
    plt <- ggplot2::ggplot(data, aes(x = .data[[obs_name]], y = value, color = layer))
    plt <- plt + ggplot2::geom_point(size = marker_size) + ggplot2::theme_bw()

    # Set correct colours.
    scale <- ggplot2::scale_color_manual(values = colours[2:length(colours)]
                                         , labels = ynames)
    plt <- plt + scale

    # Set text size.
    plt <- set_text_multiplier(plt, text_multiplier)

    units <- DGVMTools:::standardiseUnitString(gc@quant@units)
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

    if (is.null(colours)) {
        colours <- get_colour_palette(length(names(gc)))
        colours <- setNames(colours, names(gc))
    }

    # Create the base plot
    plt <- ggplot2::ggplot() +
           ggplot2::theme_bw() +
           ggplot2::labs(x = "Day") +
           ggplot2::scale_color_manual(values = colours)

    # Filter data down to only those dates for which all layers are available.
    # Otherwise, we will be plotting over different time periods for each layer.
    # filtered <- gc@data[complete.cases(gc@data), ]
    # if (nrow(filtered) == 0) {
    #     log_warning("[plot_subannual] When plotting ", gc@quant@name, ", the available layers (", paste(names(gc), collapse = ", "), ") do not cover the same time period. In other words, there is no date on which all layers have a value. The resultant subannual plot will therefore not be filtered by date, so you must be careful when interpreting it, because the subannual pattern that is rendered will have a different 'meaning' for each layer, because the timeseries cover different time periods.")
    # } else {
    #     gc@data <- filtered
    # }

    ignored_names <- c("Site")
    ynames <- setdiff(names(gc), ignored_names)
    log_debug("[plot_subannual] ynames = ", paste(ynames, collapse = ", "))

    # Filter out non-numeric layers.
    ynames <- ynames[unlist(lapply(ynames,
                                   function(n) is.numeric(gc@data[[n]])))]
    log_debug("[plot_subannual] ynames (numeric) = ",
              paste(ynames, collapse = ", "))
    if (length(ynames) == 0) {
        log_error("Unable to plot: data contains no numeric layers")
    }

    # Process and plot each layer separately
    for (layer_name in ynames) {
        log_diag("Plotting layer ", layer_name)
        # Create a temporary gc with just this layer
        temp_gc <- DGVMTools::selectLayers(gc, layer_name)
        log_diag("Found ", nrow(temp_gc@data), " rows")

        # Remove rows with NA values for this layer
        temp_gc@data <- temp_gc@data[!is.na(temp_gc@data[[layer_name]]), ]
        log_diag("After removing NAs, ", nrow(temp_gc@data), " rows remain")

        # Only proceed if we have data
        if (nrow(temp_gc@data) > 0) {
            # Aggregate this layer
            agg_result <- DGVMTools::aggregateYears(temp_gc)

            # Convert to long format for this layer only
            df_layer <- tidyr::pivot_longer(
                agg_result@data,
                cols = layer_name,
                names_to = "variable",
                values_to = "value"
            )

            # Add this layer to the plot
            plt <- plt + ggplot2::geom_line(
                data = df_layer,
                ggplot2::aes(x = Day, y = value, color = variable)
            )
        } else {
            log_warning("[plot_subannual] No data found for layer ", layer_name)
        }
    }

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
    return(get_stats_lyr_name(var@id, src@name))
}

create_plots <- function(gc, ylab, ncol = 2, use_plotly = TRUE
        , text_multiplier = NULL, do_timeseries = TRUE, do_pvo = TRUE
        , do_subannual = TRUE, marker_size = 3, ylim = NULL
        , obs_lyr = NULL) {

    # Compute (and store) statistics).
    if (is.null(obs_lyr)) {
        log_debug("No observed layer was specified, so will attempt to find one")
        readers <- find_readers_for_var(trim_dave(gc@quant@id))
        log_debug("Found ", length(readers), " readers")
        if (length(readers) > 0) {
            reader_names <- unlist(lapply(readers, function(r) r@src@id))
            log_debug("Reader names: ", paste(reader_names, collapse = ", "))
            avail_readers <- reader_names[reader_names %in% names(gc)]
            log_debug("Available readers: ", paste(avail_readers, collapse = ", "))
            if (length(avail_readers) > 0) {
                # Choose the reader with the most data points.
                nvalues <- sapply(avail_readers, function(r)
                    length(which(!is.na(gc@data[[r]]))))
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
    log_debug("obs_lyr=", obs_lyr)

    ignored_names <- c("Site", obs_lyr)
    names <- setdiff(names(gc), ignored_names)
    # Ignore any non-numeric layers that may be present.
    log_debug("All names: ", paste(names, collapse = ", "))
    names <- names[unlist(lapply(names, function(n) is.numeric(gc@data[[n]])))]
    log_debug("Numeric names: ", paste(names, collapse = ", "))
    if (length(names) == 0) {
        log_error("Unable to plot: data contains no numeric layers")
    }

    r2 <- list()
    rmse <- list()
    nse <- list()
    rsr <- list()
    bias <- list()
    # Compute stats for each source.
    for (name in names) {
        log_debug("Computing stats with xlayer='", obs_lyr, "', ylayer='", name,
                  "'")
        df <- gc@data
        df <- df[!is.na(df[[obs_lyr]]) & !is.na(df[[name]]), ]
        obs <- df[[obs_lyr]]
        pred <- df[[name]]
        lyr_name <- get_stats_lyr_name(gc@quant@id, name)

        r2[[lyr_name]] <- if (length(obs) > 0) compute_r2(obs, pred) else NA
        rmse[[lyr_name]] <- if (length(obs) > 0) compute_rmse(obs, pred) else NA
        nse[[lyr_name]] <- if (length(obs) > 0) compute_nse(obs, pred) else NA
        rsr[[lyr_name]] <- if (length(obs) > 0) compute_rsr(obs, pred) else NA
        bias[[lyr_name]] <- if (length(obs) > 0) compute_bias(obs, pred) else NA
    }

    # Apparently a variable name in the j value will be interpreted literally
    # rather than dereferencing the value stored in the variable. What an
    # amazing language this is.
    if (is.null(ylim)) {
        ymin <- min(gc@data[which(!is.na(gc@data[[obs_lyr]])), ][[obs_lyr]])
        ymax <- max(gc@data[which(!is.na(gc@data[[obs_lyr]])), ][[obs_lyr]])
        for (lyr_name in names) {
            v <- gc@data[which(!is.na(gc@data[[lyr_name]])), ][[lyr_name]]
            ymin <- min(ymin, min(v))
            ymax <- max(ymax, max(v))
        }
        ylim <- c(ymin, ymax)
    }

    colours <- get_colour_palette(length(names(gc)))
    colours <- setNames(colours, names(gc))

    # Create plots.
    result <- list()

    if (do_timeseries) {
        log_diag("Creating timeseries plot...")
        timeseries <- plot_timeseries(gc, ylim, text_multiplier,
                                      colours = colours)
        timeseries <- trim_ggplot(timeseries, xlab = TRUE)
        if (use_plotly) {
            timeseries <- to_plotly(timeseries, ylab)
        }
        result$timeseries <- timeseries
    }
    if (do_pvo) {
        log_diag("Creating predicted vs. observed scatter plot...")
        pvo <- plot_pvo(gc, ylim, text_multiplier, marker_size = marker_size,
                        colours = colours, obs_name = obs_lyr)
        # On the subannual plots, we want the x-axis label (observed) as well
        # as the y-axis label (predicted), because these are not shared with any
        # other plots in the panel.
        pvo <- trim_ggplot(pvo, xlab = TRUE, ylab = TRUE)
        if (use_plotly) {
            pvo <- to_plotly(pvo, ylab)
        }
        result$pvo <- pvo
    }
    if (do_subannual) {
        log_diag("Creating subannual plot...")
        subannual <- plot_subannual(gc, ylim, text_multiplier,
                                    colours = colours)
        subannual <- trim_ggplot(subannual, xlab = TRUE)
        if (use_plotly) {
            subannual <- to_plotly(subannual, ylab)
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
    ylab <- trim_dave(var@name)
    if (!is.null(site)) {
      ylab <- paste(site, ylab)
    }
    if (!is.null(var@units) && var@units != "") {
        ylab <- paste0(ylab, " (", var@units, ")")
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
    if (nrow(gridcell@data) == 0) {
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
#' Calculate a date from a year and day.
#' @param data A data frame containing a 'Year' and/or 'Day' column.
#' @return A Date vector.
#'
#' @keywords internal
#'
calc_date <- function(data) {
    if ("Year" %in% names(data) && "Day" %in% names(data)) {
        return(as.Date(paste0(data$Year, "-", data$Day), format = "%Y-%j"))
    } else if ("Year" %in% names(data)) {
        return(as.Date(paste0(data$Year, "-12-31"), format = "%Y-%m-%d"))
    } else {
        stop("Data must contain 'Year' and 'Day' columns, or 'Year' column.")
    }
}
