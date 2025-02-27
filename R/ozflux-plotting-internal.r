
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
        title = NULL) {

    plots <- list(...)
    log_diag("Creating panel with ", length(plots), " plots")
    nplot <- length(plots)
    nrow <- as.integer(ceiling(nplot / ncol))
    if (use_plotly) {
        fix_legend <- function(p) {
            return(plotly::layout(p, legendgroup = "Layout"))
        }
        for (plot in plots) {
            plot <- fix_legend(plot)
        }
        args <- c(plots, nrows = nrow, shareY = TRUE)
        plt <- do.call(plotly::subplot, args)
        for (layer in seq_len(length(plt$x$data) * (nplot - 1L) / nplot)) {
            plt$x$data[[layer]]$showlegend <- FALSE
        }
    } else {
        for (plot in plots) {
            plot <- trim_ggplot(plot, xlab = TRUE)
        }

        gp <- grid::gpar(cex = 1.3)

        args <- c(plots, ncol = ncol, nrow = nrow, common.legend = TRUE
            , legend = "bottom", align = "hv")
        plt <- do.call(ggpubr::ggarrange, args)
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

plot_timeseries <- function(
    gc,
    ylim = NULL,
    text_multiplier = NULL,
    xlab = NULL,
    ylab = NULL,
    layers = NULL,
    subtitle = NULL) {

    ncolour <- ifelse(is.null(layers), length(names(gc)), length(layers))
    colours <- get_colour_palette(ncolour)
    points <- FALSE
    lyrs <- ifelse(is.null(layers), names(gc), layers)
    POINTS_THRESHOLD <- 30
    for (lyr in lyrs) {
        data <- gc@data[[lyr]]
        data <- data[which(!is.na(data))]
        if (length(data) < POINTS_THRESHOLD) {
            points <- TRUE
        }
    }
    return(DGVMTools::plotTemporal(gc, layers = layers, cols = colours
        , text.multiplier = text_multiplier, text.expression = FALSE
        , y.lim = ylim, x.label = xlab, y.label = ylab, subtitle = NULL
        , title = NULL, points = points))
}

plot_pvo <- function(gc, ylim = NULL, text_multiplier = NULL, marker_size = 3) {
    colours <- get_colour_palette(length(names(gc)))
    # predicted_name <- names(gc)[[length(names(gc))]]
    log_debug("[plot_pvo] names(gc) = ", paste(names(gc), collapse = ", "))
    # plt <- DGVMTools::plotScatter(gc, layer.x = get_global("obs_lyr")
    #     , layer.y = predicted_name, text.multiplier = text_multiplier)
    obs_name <- get_global("obs_lyr")
    ynames <- setdiff(names(gc), obs_name)

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
    x_label <- "Observed"
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

plot_subannual <- function(gc, ylim = NULL, text_multiplier = NULL) {
    colours <- get_colour_palette(length(names(gc)))
    colours <- setNames(colours, names(gc))
    agg <- DGVMTools::aggregateYears(gc)
    # plt <- DGVMTools::plotSubannual(gc, col.by = "Layer"
    #     , summary.function = mean
    #     , title = NULL, subtitle = NULL, point.size = 0, cols = colours
    #     , text.multiplier = text_multiplier, summary.only = TRUE
    #     , summary.as.points = FALSE)
    df_long <- tidyr::pivot_longer(agg@data, cols = names(gc)
                                   , names_to = "variable"
                                   , values_to = "value")
    plt <- ggplot2::ggplot(df_long, aes(x = Day, y = value, color = variable)) +
                    ggplot2::geom_line() +
                    ggplot2::scale_color_manual(values = colours) +
                    ggplot2::labs(x = "Day") +
                    ggplot2::theme_bw()

    # plt <- DGVMTools::plotTemporal(agg, title = NULL, subtitle = NULL
    #                                , point.size = 0,cols = colours
    #                                , text.multiplier = text_multiplier)

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
        , do_subannual = TRUE, marker_size = 3, ylim = NULL) {

    # Compute (and store) statistics).
    obs_lyr <- get_global("obs_lyr")
    obs <- gc@data[[obs_lyr]]
    names <- names(gc)
    if (length(names) == 0) {
        log_error("Unable to plot: data contains no layers")
    }

    r2 <- list()
    rmse <- list()
    nse <- list()
    rsr <- list()
    bias <- list()
    # Compute stats for each source.
    for (name in names(gc)) {
        if (name != obs_lyr) {
            pred <- gc@data[[name]]
            lyr_name <- get_stats_lyr_name(gc@quant@id, name)

            r2[[lyr_name]] <- compute_r2(obs, pred)
            rmse[[lyr_name]] <- compute_rmse(obs, pred)
            nse[[lyr_name]] <- compute_nse(obs, pred)
            rsr[[lyr_name]] <- compute_rsr(obs, pred)
            bias[[lyr_name]] <- compute_bias(obs, pred)
        }
    }

    # Apparently a variable name in the j value will be interpreted literally
    # rather than dereferencing the value stored in the variable. What an
    # amazing language this is.
    if (is.null(ylim)) {
        ymin <- min(gc@data[which(!is.na(gc@data[[obs_lyr]])), ][[obs_lyr]])
        ymax <- max(gc@data[which(!is.na(gc@data[[obs_lyr]])), ][[obs_lyr]])
        for (lyr_name in names(gc)) {
            v <- gc@data[which(!is.na(gc@data[[lyr_name]])), ][[lyr_name]]
            ymin <- min(ymin, min(v))
            ymax <- max(ymax, max(v))
        }
        ylim <- c(ymin, ymax)
    }

    # Create plots.
    result <- list()

    if (do_timeseries) {
        log_diag("Creating timeseries plot...")
        timeseries <- plot_timeseries(gc, ylim, text_multiplier)
        timeseries <- trim_ggplot(timeseries, xlab = TRUE)
        if (use_plotly) {
            timeseries <- to_plotly(timeseries, ylab)
        }
        result$timeseries <- timeseries
    }
    if (do_pvo) {
        log_diag("Creating predicted vs. observed scatter plot...")
        pvo <- plot_pvo(gc, ylim, text_multiplier, marker_size = marker_size)
        pvo <- trim_ggplot(pvo, xlab = TRUE)
        if (use_plotly) {
            pvo <- to_plotly(pvo, ylab)
        }
        result$pvo <- pvo
    }
    if (do_subannual) {
        log_diag("Creating subannual plot...")
        subannual <- plot_subannual(gc, ylim, text_multiplier)
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
