
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
        stop(paste0("Unable to get colour palette with ", n, " colours"))
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
    timeseries,
    pvo,
    subannual,
    use_plotly,
    ncol = 2,
    ylab = NULL) {

    nplot <- 3
    nrow <- as.integer(ceiling(nplot / ncol))
    if (use_plotly) {
      fix_legend <- function(p) {
        return(plotly::layout(p, legendgroup = "Layout"))
      }
      timeseries <- fix_legend(timeseries)
      pvo <- fix_legend(pvo)
      subannual <- fix_legend(subannual)
      plt <- plotly::subplot(timeseries, pvo, subannual, nrows = nrow
        , shareY = TRUE)
      for (layer in seq_len(length(plt$x$data) * (nplot - 1L) / nplot)) {
        plt$x$data[[layer]]$showlegend <- FALSE
      }
    } else {
      trim_plot <- function(plt) {
        return(plt +
        ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL))
      }
      timeseries <- trim_plot(timeseries) + ggpubr::rremove("xylab")
      pvo <- trim_plot(pvo)
      subannual <- trim_plot(subannual) + ggpubr::remove("ylab")

      gp <- grid::gpar(cex = 1.3)

      plt <- ggpubr::ggarrange(timeseries, pvo, subannual, ncol = ncol
        , nrow = nrow, common.legend = TRUE, legend = "bottom", align = "hv")
      if (!is.null(ylab)) {
        plt <- ggpubr::annotate_figure(plt,
          left = grid::textGrob(ylab, rot = 90, vjust = 1, gp = gp))
      }
    }
    return(plt)
}

plot_timeseries <- function(
    gc,
    ylim = NULL,
    text_multiplier = NULL,
    ylab = NULL,
    layers = NULL) {

    ncolour <- ifelse(is.null(layers), length(names(gc)), length(colours))
    colours <- get_colour_palette(ncolour)
    return(DGVMTools::plotTemporal(gc, layers = layers, cols = colours
        , text.multiplier = text_multiplier, text.expression = FALSE
        , y.lim = ylim, y.label = ylab))
}

plot_pvo <- function(gc, ylim = NULL, text_multiplier = NULL, marker_size = 3) {
    colours <- get_colour_palette(length(names(gc)))
    return(DGVMTools::plotScatter(gc, layer.x = get_global("obs_lyr")
        , title = NULL
        , subtitle = NULL, cols = colours[2:length(colours)]
        , text.multiplier = text_multiplier, x.label = "Observed"
        , y.label = "Predicted", one_to_one_line = TRUE, y.lim = ylim
        , x.lim = ylim, size = marker_size))
}

plot_subannual <- function(gc, ylim = NULL, text_multiplier = NULL) {
    colours <- get_colour_palette(length(names(gc)))
    return(DGVMTools::plotSubannual(gc, col.by = "Layer"
        , summary.function = mean
        , title = NULL, subtitle = NULL, point.size = 0, cols = colours
        , text.multiplier = text_multiplier, summary.only = TRUE
        , summary.as.points = FALSE, y.lim = ylim))
}

create_plots <- function(gc, ylab, ncol = 2, use_plotly = TRUE
        , text_multiplier = NULL, do_timeseries = TRUE, do_pvo = TRUE
        , do_subannual = TRUE, marker_size = 3) {

    # Compute (and store) statistics).
    obs_lyr <- get_global("obs_lyr")
    obs <- gc@data[, obs_lyr]
    names <- names(gc)
    if (length(names) == 0) {
        log_error("Unable to plot: data contains no layers")
    }
    if (length(names) == 1) {
        predicted_name <- names[1]
    } else {
        if (names[1] == obs_lyr) {
            predicted_name <- names[2]
        } else {
            predicted_name <- names[1]
        }
    }
    predicted_name <- names(gc)[[length(names(gc))]]
    pred <- gc@data[, predicted_name]

    r2 <- compute_r2(obs, pred)
    rmse <- compute_rmse(obs, pred)
    nse <- compute_nse(obs, pred)
    rsr <- compute_rsr(obs, pred)
    bias <- compute_bias(obs, pred)

    # Apparently a variable name in the j value will be interpreted literally
    # rather than dereferencing the value stored in the variable. What an
    # amazing language this is.
    ymin <- min(gc@data[which(!is.na(gc@data[[obs_lyr]])), ][[obs_lyr]])
    ymax <- max(gc@data[which(!is.na(gc@data[[obs_lyr]])), ][[obs_lyr]])
    for (lyr_name in names(gc)) {
        v <- gc@data[which(!is.na(gc@data[[lyr_name]])), ][[lyr_name]]
        ymin <- min(ymin, min(v))
        ymax <- max(ymax, max(v))
    }
    ylim <- c(ymin, ymax)

    # Create plots.
    result <- list()

    if (do_timeseries) {
        timeseries <- plot_timeseries(gc, ylim, text_multiplier)
        if (use_plotly) {
            timeseries <- to_plotly(timeseries, ylab)
        }
        result$timeseries <- timeseries
    }
    if (do_pvo) {
        pvo <- plot_pvo(gc, ylim, text_multiplier, marker_size = marker_size)
        if (use_plotly) {
            pvo <- to_plotly(pvo, ylab)
        }
        result$pvo <- pvo
    }
    if (do_subannual) {
        subannual <- plot_subannual(gc, ylim, text_multiplier)
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

trim_ggplot <- function(
    plt,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    xlab = FALSE,
    ylab = FALSE) {

    result <- plt
    result <- result + ggplot2::labs(
        title = title,
        subtitle = subtitle,
        caption = caption)
    if (!xlab) {
        result <- result + ggpubr::rremove("xlab")
    }
    if (!ylab) {
        result <- result + ggpubr::rremove("ylab")
    }
    return(result)
}

get_y_label <- function(var, site = NULL) {
    var <- sanitise_variable(var)
    ylab <- trim_dave(var@name)
    if (!is.null(site)) {
      ylab <- paste(site, ylab)
    }
    if (!is.null(var@units)) {
        ylab <- paste0(ylab, " (", var@units, ")")
    }
    return(ylab)
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
        panel <- ggpubr::ggarrange(plotlist = plots, common.legend = TRUE)
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

ozflux_plot_site <- function(
        data,
        ylim,
        site,
        separate,
        use_plotly,
        nsite,
        vars) {
    log_diag("Plotting ", site$Name, "...")

    # Extract data for the required grid cell.
    gridcell <- get_gridcell(data, site$Lat, site$Lon, site$Name)
    if (nrow(gridcell@data) == 0) {
        log_error("No data found for site ", site$Name, " (", site$Lon, ", "
            , site$Lat, ")")
    }

    # Plot the gridcell.
    plt <- plot_timeseries(gridcell, ylim)

    # The plot title should not include the variable name if plotting a panel
    # of sites. In that case it's better to have the variable name as the main
    # title above the panel, and use the site name for the subplot titles.
    title <- site$Name
    if (length(vars) == 1 && nsite == 1) {
        name <- trim_dave(vars[[1]]@name)
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
        return(vars[[1]]@name)
    }
}
