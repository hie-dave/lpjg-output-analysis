
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
  res <- ggplotly(chart)
  res <- res %>% layout(
    xaxis = list(title = list(text = "Date")),
    yaxis = list(title = list(text = lyr_name)),
    legend = list(bgcolor = "rgba(0,0,0,0)"))
  res <- res %>% config(scrollZoom = TRUE)
  return(res)
}

create_panel <- function(timeseries, pvo, subannual, use_plotly, ncol = 2) {
  nplot <- 3
  nrow <- as.integer(ceiling(nplot / ncol))
  if (use_plotly) {
    fix_legend <- function(p) {
      return(plotly::layout(p, legendgroup = "Layout"))
    }
    timeseries <- fix_legend(timeseries)
    pvo <- fix_legend(pvo)
    subannual <- fix_legend(subannual)
    plt <- subplot(timeseries, pvo, subannual, nrows = nrow, shareY = TRUE)
    for (layer in seq_len(length(plt$x$data) * (nplot - 1L) / nplot)) {
      plt$x$data[[layer]]$showlegend <- FALSE
    }
  } else {
    plt <- ggarrange(timeseries, pvo, subannual, ncol = ncol, nrow = nrow
      , common.legend = TRUE, legend = "bottom", align = "hv")
  }
  return(plt)
}

plot_timeseries <- function(gc, ylim = NULL, text_multiplier = NULL, ylab = NULL) {
	colours <- get_colour_palette(length(names(gc)))
	return(plotTemporal(gc, cols = colours
      , text.multiplier = text_multiplier, text.expression = FALSE
      , y.lim = ylim, y.label = ylab))
}

plot_pvo <- function(gc, ylim = NULL, text_multiplier = NULL, marker_size = 3) {
	colours <- get_colour_palette(length(names(gc)))
	return(plotScatter(gc, layer.x = get_global("obs_lyr"), title = NULL
      , subtitle = NULL, cols = colours[2:length(colours)]
      , text.multiplier = text_multiplier, x.label = "Observed"
      , y.label = "Predicted", one_to_one_line = TRUE, y.lim = ylim
      , x.lim = ylim, size = marker_size))
}

plot_subannual <- function(gc, ylim = NULL, text_multiplier = NULL) {
	colours <- get_colour_palette(length(names(gc)))
	return(plotSubannual(gc, col.by = "Layer", summary.function = mean
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