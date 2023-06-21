
#' Create a variable 'object' given its lpj-guess name, units and display name.
#' @param name: Name of the variable in LPJ-Guess. This is also the name of the
#'              column in the observed data.
#' @param units: Units of the variable.
#' @param display_name: Friendly name of the variable to go in plots/legends.
#' @param obs_name: Name of the observed variable.
#' @param file_name: Name of the output file without file extension.
#' @param pred_name: Name of the predicted data column.
#' @param on_right: A variable to be plotted on the right-axis in timeseries
#'                  plots.
#' @param right_scale: Scaling factor to be applied to on_right.
define_variable <- function(name, units, display_name, obs_name = name
  , file_name = paste0("dave_", name, ".out"), obs_source = "flux data"
  , pred_name = "total", on_right = NULL, right_scale = 1) {
  variable <- c()
  variable$name <- name
  variable$units <- units
  variable$title <- display_name
  variable$obs_name <- obs_name
  variable$obs_source <- obs_source
  variable$file_name <- file_name
  variable$pred_name <- pred_name
  variable$on_right <- on_right
  variable$right_scale <- right_scale
  return(variable)
}

#' Create an 'object' which represents an LPJ-Guess version.
#' @param name: Name of the version which will appear in graph legends.
#' @param path: Path to the repository for this version of the model.
define_version <- function(name, path) {
  version <- c()
  version$name <- name
  version$path <- path
  return(version)
}

create_stats_table <- function(stat_name) {
  table <- data.frame(row.names = sites)
  columns <- c()
  for (var in vars) {
    columns <- c(columns, var$name)
    table[var$name] <- rep(NA, length(sites))
  }
  colnames(table) <- columns
  return(table)
}

################################################################################
# User Inputs - modify as required
################################################################################

# Path to directory containing observed data.
obs_dir <- "obs"

# Desired output directory.
out_dir <- "graphs"

# Names of sites to be plotted.
sites <- c(
  "AdelaideRiver"
, "AliceSpringsMulga"
, "Boyagin"
, "Calperum"
, "CapeTribulation"
, "Collie"
, "CowBay"
, "CumberlandPlain"
, "DalyPasture"
, "DalyUncleared"
, "DryRiver"
, "Emerald"
, "FoggDam"
, "Gingin"
, "GreatWesternWoodlands"
, "HowardSprings"
, "Litchfield"
, "Longreach"
, "Otway"
, "RedDirtMelonFarm"
, "Ridgefield"
, "RiggsCreek"
, "RobsonCreek"
, "Samford"
, "SilverPlains"
, "SturtPlains"
, "TiTreeEast"
, "Tumbarumba"
, "WallabyCreek"
# , "Warra"
# , "Whroo"
, "WombatStateForest"
, "Yanco"
)

versions <- list(
    define_version("dave", "../dave-daily-grass-photosynthesis")
  , define_version("daily-grass", "../dave-daily-grass")
  , define_version("trunk", "../dave-baseline")
)

# Variables to be plotted
vars <- list(
    define_variable("gpp", "gC * m ^ -2 * day ^ -1", "GPP", "gpp")
  , define_variable("resp", "gC * m ^ -2 * day ^ -1", "Respiration")
  , define_variable("nee", "gC * m ^ -2 * day ^ -1", "NEE")
  , define_variable("et", "mm.day ^ -1", "ET")
  , define_variable("lai", "m ^ 2 * m ^ -2", "LAI", obs_source = "remotely sensed")
  # , define_variable("cmass", "kgC/m2", "AboveGround Biomass", "live_biomass")
)

# TRUE to plot data during spinup period, FALSE otherwise.
show_spinup <- FALSE

# TRUE to show individual pfts' data. FALSE otherwise (ie just show totals).
show_pfts <- FALSE

# TRUE to generate plotly plots in addition to other plots.
use_plotly <- TRUE

# Plot scaling. Increase this to make everything bigger.
scale <- 1.5

# Width and height (in px) of the generated graphs.
width <- 1080
height <- as.integer(width / sqrt(2))

# Number of sig figs used when writing stats like RMSE/NSE.
num_figs <- 2

# TRUE to create an extra directory containing plots from all sites in a single
# directory. (Uses links, copies.)
create_allsite_dir <- TRUE

# Compute and draw on the plot stats (rmse/nse/etc) for baseline vs obs data.
write_baseline_stats <- FALSE

stats_file_name <- "stats.tex"

r2_table <- create_stats_table("r2")
rmse_table <- create_stats_table("rmse")
rsr_table <- create_stats_table("rsr")
nse_table <- create_stats_table("nse")
bias_table <- create_stats_table("bias")

figures_tex_name <- "figures.tex"

################################################################################
# Global Variables. Probably shouldn't be modified by users.
################################################################################

if (use_plotly) {
  library(plotly)
}

# Additional multiplier for legends text.
legend_scale <- 2

# Additional multiplier for title text.
title_scale <- 3

# Colour palette optimised for people with various kinds of colour-blindness.
# Wong, B. (2011) Color blindness, Nature Methods, Vol 8, No. 6.
cb_colours <- c(
  "#e69f00",
  "#56b4e9",
  "#cc79a7",
  "#009e73",
  "#0072b2",
  "#d55e00",
  "#f0e442",
  "#000000"
)

# Name of the observed column in the processed data.
colname_observed <- "Observed"

# Name of the year column in the predicted data.
colname_year <- "Year"

# Name of the 'day of year' column in the predicted data. Note that this column
# contains a day index (ie 0-364).
colname_day <- "Day"

# Name of the date column in the processed data.
colname_date <- "Date"

# Name of the site-level output directory.
out_dir_name <- "out"

# Name of the output directory which will contain site-level graphs in separate
# directories.
by_site_dir <- "by-site"

# Name of the output directory which will contain site-level graphs in a single
# directory.
all_site_dir <- paste0(out_dir, "/", "all-sites")

# Name of the total column in the input data.
in_colname_total <- "total"

# If less than this proportion of the observed data is NA, it will be
# interpolated and drawn as lines. If more than this proportion of the observed
# data is NA, it will be drawn as a line.
interp_threshold <- 0.9

# Name of the 90cm soil moisture column which will be calculated from the
# predictions.
name_sw90 <- "sw_90"

# Soil moisture at 90cm variable definition.
sw90 <- define_variable("sw", "", "90 * cm ~ Soil ~ Moisture ~ Fraction"
  , obs_name = "swindex_90", obs_source = "SMIPS")

# Relative path to ozflux directory within the repository root.
ozflux_relpath <- "/benchmarks/ozflux"

# Number of days in an LPJ-Guess year.
year_len <- 365

# Height of lpj-guess soil layers (cm).
lpj_layer_height <- 10

# PCH value which yields a filled circle.
# col sets the border colour and bg sets the background colour.
pch_filled_circle <- 21

# Scaling factor for symbols used in Predicted vs Observed plots.
pvo_point_size <- 0.5

# Alpha channel applied to symbols in predicted vs. observed plots. This should
# be in range [0, 1]. Lower values mean more transparency (ie less opacity).
symbol_alpha <- 1

# Name of precipitation column in processed data.
colname_precip <- "Precipitation"

# Name of the precipitation output file.
filename_precip <- "dave_met_precip.out"

# Precip data scaling factor.
scale_precip <- 0.25

# Top row margins (in # lines).
mar_top_row <- c(3, 3, 2, 1) + 0.1

# Bottom row margins (in # lines).
mar_bottom_row <- c(3, 3, 0, 1) + 0.1

# The bottom side in APIs which take a side index.
side_bottom <- 1

# The left side in APIs which take a side index.
side_left <- 2

# The top side in APIs which take a side index.
side_top <- 3

# The right side in APIs which take a side index.
side_right <- 4

# The line on which x-axis labels are drawn.
xaxis_label_line <- 2

# The line on which y-axis labels are drawn.
yaxis_label_line <- 1.8

################################################################################
# Functions.
################################################################################

#' Read an LPJ-Guess output file and return file data as a dataframe.
#' @param filename: Path to the output file, may be relative or absolute.
#' @param nrow: Maximum number of rows to read.
read_outfile <- function(filename, nrow = -1) {
  return(read.table(filename, header = TRUE, nrows = nrow))
}

#' Read data for the specified output variable for a site.
#' @param site: The site.
#' @param var: The output variable.
read_site <- function(guess, site, var) {
  ozflux <- paste0(guess, ozflux_relpath)
  filename <- paste0(ozflux, "/", site, "/", out_dir_name, "/", var)
  return(read_outfile(filename))
}

get_series_name <- function(name, units) {
  if (units == "") {
    return(name)
  }
  return(paste0(name, " ~ (", units, ")"))
}

#' Plot one column (xcol) of data against all of the column names specified in
#' ycols, on the same graph, using the same y-axis.
#'
#' @param data: Dataframe of data.
#' @param xcol: Name of the x-data column.
#' @param ycols: Vector of names of y-data columns to be plotted.
#' @param var: Metadata of the variable being plotted.
#' @param interp: Can the observations be interpolated?
#' @param write_legend: True to write the legend. False otherwise.
#' @param legend_inside_graph: True to write the legend inside the plot area. False otherwise.
plot_timeseries_plotly <- function(data, xcol, ycols, var, interp = TRUE, ...) {
  library(plotly)

  xlab <- parse(text = xcol)
  ylab <- parse(text = get_series_name(var$title, var$units))

  # Get colours used for plotting the data.
  ncol <- length(ycols)
  if (!is.null(var$on_right)) {
    ncol <- length(ycols) + 1
  }
  colours <- get_colour_palette(ncol)
  plt <- plot_ly(colors = colours)
  for (i in seq_len(length(ycols))) {
    type <- "l"

    # if y data is missing some points (common for observations), use points,
    # rather than lines.
    xdata <- data[, xcol]
    ydata <- data[, ycols[i]]
    if (ycols[i] == colname_observed && NA %in% ydata) {
      na_indices <- which(is.na(ydata))
      na_prop <- length(na_indices) / length(ydata)
      if (interp && na_prop < interp_threshold) {
        not_na <- which(!is.na(ydata))
        first <- not_na[1]
        last <- not_na[length(not_na)]
        ydata <- ydata[first:last]
        xdata <- xdata[first:last]
        na_indices <- which(is.na(ydata))
        not_na <- which(!is.na(ydata))
        for (na_idx in na_indices) {
          prv <- na_idx - 1
          nxt <- not_na[not_na > na_idx][1]
          dlt_x <- nxt - prv
          dlt_y <- ydata[nxt] - ydata[prv]
          ydata[na_idx] <- ydata[prv] + dlt_y / dlt_x
        }
      } else {
        type <- "p"
      }
    }
    if (type == "l") {
      plt <- plt %>% add_lines(x = xdata, y = ydata, name = ycols[i])
    } else {
      plt <- plt %>% add_markers(x = xdata, y = ydata, name = ycols[i])
    }
    plt <- plt %>% layout(xaxis = xlab, yaxis = ylab, title = title)
  }

  # TBI: variable on right axis
  # if (!is.null(var$on_right)) {
  #   xdata <- data[, xcol]
  #   ydata <- data[, var$on_right$name]
  #   ymax <- max(ydata) / var$right_scale
  #   ymin <- min(ydata)
  #   ylim <- c(ymin, ymax)

  #   par(new = TRUE)
  #   plot(xdata, ydata, type = type, col = colours[ncol], xlim = xlim
  #     , ylim = ylim, mgp = mgp, xlab = "", ylab = "", xaxt = "n", yaxt = "n"
  #     , ...)
  # }

  # return(subplot(figures, nrows = 2, shareY = TRUE))
  return(plt)
}

#' Plot one column (xcol) of data against all of the column names specified in
#' ycols, on the same graph, using the same y-axis.
#'
#' @param data: Dataframe of data.
#' @param xcol: Name of the x-data column.
#' @param ycols: Vector of names of y-data columns to be plotted.
#' @param var: Metadata of the variable being plotted.
#' @param interp: Can the observations be interpolated?
#' @param write_legend: True to write the legend. False otherwise.
#' @param legend_inside_graph: True to write the legend inside the plot area. False otherwise.
plot_timeseries <- function(data, xcol, ycols, var, interp = TRUE, ...) {
  # We want to plot all series using the same y-axis. Ergo, we need to know the
  # largest and smallest y-values in advance.
  ymax <- -1e300
  ymin <- 1e300
  for (i in seq_len(length(ycols))) {
    col <- data[!is.na(data[, ycols[i]]), ycols[i]]
    ymin <- min(ymin, min(col))
    ymax <- max(ymax, max(col))
  }

  xmin <- min(data[, xcol])
  xmax <- max(data[, xcol])
  xlim <- c(xmin, xmax)

  xlab <- parse(text = xcol)
  ylab <- parse(text = get_series_name(var$title, var$units))

  # Get colours used for plotting the data.
  ncol <- length(ycols)
  if (!is.null(var$on_right)) {
    ncol <- length(ycols) + 1
  }
  colours <- get_colour_palette(ncol)

  mgp <- c(2.5, 1, 0)

  for (i in seq_len(length(ycols))) {
    if (i > 1) {
      # new = TRUE means, rather couter-intuitively, that the next call to
      # plot() will go onto the previous plot, rather than creating a new plot.
      par(new = TRUE)
    }
    type <- "l"

    # if y data is missing some points (common for observations), use points,
    # rather than lines.
    xdata <- data[, xcol]
    ydata <- data[, ycols[i]]
    if (ycols[i] == colname_observed && NA %in% ydata) {
      na_indices <- which(is.na(ydata))
      na_prop <- length(na_indices) / length(ydata)
      if (interp && na_prop < interp_threshold) {
        not_na <- which(!is.na(ydata))
        first <- not_na[1]
        last <- not_na[length(not_na)]
        ydata <- ydata[first:last]
        xdata <- xdata[first:last]
        na_indices <- which(is.na(ydata))
        not_na <- which(!is.na(ydata))
        for (na_idx in na_indices) {
          prv <- na_idx - 1
          nxt <- not_na[not_na > na_idx][1]
          dlt_x <- nxt - prv
          dlt_y <- ydata[nxt] - ydata[prv]
          ydata[na_idx] <- ydata[prv] + dlt_y / dlt_x
        }
      } else {
        type <- "p"
      }
    }
    plot(xdata, ydata, type = type, col = colours[i]
      , xlab = "", ylab = "", xlim = xlim, ylim = c(ymin, ymax)
      , mgp = mgp, ...)
    mtext(xlab, side = side_bottom, line = xaxis_label_line, cex = scale)
    mtext(ylab, side = side_left, line = yaxis_label_line, cex = scale)
  }

  if (!is.null(var$on_right)) {
    xdata <- data[, xcol]
    ydata <- data[, var$on_right$name]
    ymax <- max(ydata) / var$right_scale
    ymin <- min(ydata)
    ylim <- c(ymin, ymax)

    par(new = TRUE)
    plot(xdata, ydata, type = type, col = colours[ncol], xlim = xlim
      , ylim = ylim, mgp = mgp, xlab = "", ylab = "", xaxt = "n", yaxt = "n"
      , ...)
    # side 1 = bottom, 2 = left, 3 = top, 4 = right
    # line controls distance from plot area
    # title <- var$on_right$title
    # units <- var$on_right$units
    # yname <- parse(text = get_series_name(title, units))
    # side_top <- 4
    #axis(side = side_top)
    #mtext(yname, side = side_top, line = 0.5, cex = scale)
  }
}

write_legend <- function(var, names, colours = get_colour_palette(length(names))
  , inside_graph) {
  if (!is.null(var$obs_source)) {
    names[which(names == colname_observed)] <- paste0(
      colname_observed, " (", var$obs_source, ")")
  }
  if (inside_graph) {
    pos <- "topleft"
    inset <- c(0, 0)
  } else {
    pos <- "topright"
    inset <- c(-0.475, 0)
  }
  legend(pos, legend = names, text.col = colours
    , lwd = par("lwd"), col = colours, inset = inset)
}

#' Aggregate data to means for each day of year.
#' @param data: A data frame.
#' @param ycols: Columns to be aggregated.
aggregate_seasonal <- function(data, ycols) {
    aggregated <- data[1:year_len, ]
    for (i in 1:year_len) {
      indices <- which(as.integer(format(data$Date, format = "%j")) == i)
      for (ycol in ycols) {
        not_na <- indices[which(!is.na(data[indices, ycol]))]
        aggregated[i, ycol] <- mean(data[not_na, ycol])
      }
    }
    return(aggregated)
}

#' Plot seasonal trends.
#'
#' @param data: Input data.
#' @param colname_date: Name of the date column.
#' @param timeseries_names: Column names, for which a seasonal timeseries will
#' be plotted.
#' @param var: Output variable metadata.
#' @param interp: True to interpolate sparse observations, false otherwise.
plot_seasonal_plotly <- function(data, xcol, ycols, var, interp = TRUE, ...) {
    aggregated <- aggregate_seasonal(data, ycols)
    return(plot_timeseries_plotly(aggregated, xcol, ycols, var, interp = TRUE
      , ...))
}

#' Plot seasonal trends.
#'
#' @param data: Input data.
#' @param colname_date: Name of the date column.
#' @param timeseries_names: Column names, for which a seasonal timeseries will
#' be plotted.
#' @param var: Output variable metadata.
#' @param interp: True to interpolate sparse observations, false otherwise.
plot_seasonal <- function(data, xcol, ycols, var, interp = TRUE, ...) {
    aggregated <- aggregate_seasonal(data, ycols)
    plot_timeseries(aggregated, xcol, ycols, var, interp = TRUE, ...)
}

#' Compute r^2 value
#'
#' @param x: Observations
#' @param y: Predictions
compute_r2 <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute r2: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute r2: input data contains no elements")
  }
  r2 <- cor(x, y)^2
  if (is.na(r2)) {
    return(0)
  }
  return(r2)
}

#' Compute root mean square error.
#'
#' This is in the units of the variable.
#'
#' @param x: Observations
#' @param y: Predictions
compute_rmse <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute rmse: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute rmse: input data contains no elements")
  }
  return(sqrt(mean((y - x)^2)))
}

#' Compute nash-sutcliffe efficiency (-Inf - 1)
#'
#' This is one minus the ratio of the error variance in the predictions to the
#' variance in the observations.
#'
#' An efficiency of 1 means the predictions perfectly match the observations.
#' An efficiency of 0 means that the predictions are equally accurate predictors
#' as the mean of the observed data. An efficiency of < 0 means that the mean of
#' the observations is a better predictor than the model.
#'
#' @param x: Observations
#' @param y: Predictions
compute_nse <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute nse: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute nse: input data contains no elements")
  }
  return(1 - (sum((y - x) ^ 2)) / sum((x - mean(x)) ^ 2))
}

#' Compute ratio of RMSE to standard deviation of observations (0 - Inf).
#'
#' Value of 0 means 0 RMSE (ie perfect fit).
#'
#' @param x: Observations
#' @param y: Predictions
compute_rsr <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Cannot compute rsr: x/y lengths differ")
  }
  if (length(x) == 0) {
    stop("Cannot compute rsr: input data contains no elements")
  }
  return(compute_rmse(x, y) / sd(x))
}

compute_bias <- function(observations, predictions) {
  return(mean(predictions - observations))
}

escape_underscores <- function(text) {
  return(gsub("_", "\\_", text, fixed = TRUE))
}

write_stats <- function(observations, predictions, units, num_figs, colour
  , var_name, site, inset = c(0, 0), write_tex) {
  # Filter out NA values.
  filter <- !is.na(observations)
  observations <- observations[filter]
  predictions <- predictions[filter]
  filter <- !is.na(predictions)
  observations <- observations[filter]
  predictions <- predictions[filter]

  r2 <- compute_r2(observations, predictions)
  rmse <- compute_rmse(observations, predictions)
  nse <- compute_nse(observations, predictions)
  rsr <- compute_rsr(observations, predictions)
  bias <- compute_bias(observations, predictions)

  r2_table[site, colnames(r2_table) == var_name] <<- r2
  rmse_table[site, colnames(rmse_table) == var_name] <<- rmse
  nse_table[site, colnames(nse_table) == var_name] <<- nse
  rsr_table[site, colnames(rsr_table) == var_name] <<- rsr
  bias_table[site, colnames(bias_table) == var_name] <<- bias

  # if (write_tex) {
  #   fmt <- function(x, ndigits = 3) {
  #     return(round(x, ndigits))
  #   }
  #   site_ref <- paste0("\\hyperref[fig:", tolower(site), "_", tolower(var$name)
  #     , "]{", site, "}")
  #   csv_line <- paste(
  #       site_ref,
  #       escape_underscores(var_name),
  #       fmt(r2),
  #       fmt(rmse),
  #       fmt(nse),
  #       fmt(rsr),
  #       fmt(bias),
  #       sep = " & ")
  #   w <- function(text) {
  #     cat(text, file = stats_file_name, append = TRUE)
  #   }
  #   w(csv_line)
  #   w(" \\\\ \n")
  # }

  mkstats <- function(...) {
    legend("topright", legend = c(
        paste0("r2 = ", signif(r2, num_figs))
      , paste0("rmse = ", signif(rmse, num_figs))
      , paste0("nse = ", signif(nse, num_figs))
      , paste0("rsr = ", signif(rsr, num_figs))
      , paste0("bias = ", signif(bias, num_figs))
    ), col = colour, text.col = colour, inset = inset, ...)
  }
  size <- mkstats(plot = FALSE)
  mkstats()
  return(size)
}

#' Return a vector of colours of the specified length, to be used for plotting.
get_colour_palette <- function(n) {
  if (n <= length(cb_colours)) {
    return(cb_colours[1:n])
  }
  return(hcl.colors(n))
}

#' Plot predicted data on the y-axis against observed data on the x-axis.
#' Data will be displayed as a series of points (not lines).
#' This function uses plotly (not base plotting functions).
#' @param data: Dataframe of data.
#' @param colname_obs: Name of the observed data column.
#' @param names: Name of predicted series to be plotted.
#' @param units: Units of the data (used for display purposes).
#' @param var_name: Name of the variable being plotted.
#' @param write_legend: True to write a legend, false otherwise.
plot_pvo_plotly <- function(data, colname_x, y_names, units, var_name) {

  xdata <- data[, colname_x]

  ymin <- min(xdata)
  ymax <- max(xdata)
  for (name in y_names) {
    col <- data[!is.na(data[, name]), name]
    ymin <- min(ymin, min(col))
    ymax <- max(ymax, max(col))
  }

  # The data may include the full spinup period, with NA values for the
  # observations in during this period. We need to remove these NA rows when
  # drawing the P vs O plot. (Using spinup data only makes sense in a timeseries
  # plot.)
  data <- data[!is.na(data[[colname_x]]), ]
  xdata <- data[, colname_x]

  xlab <- parse(text = get_series_name(colname_x, units))
  ylab <- parse(text = get_series_name(var_name, units))

  # Get a colour scheme to be used for plotting.
  colours <- c(get_colour_palette(length(y_names)), "black")

  # Plot each series.
  fig <- plot_ly(colors = colours)
  for (i in seq_len(length(y_names))) {
    # colour <- adjustcolor(colours[i], alpha.f = symbol_alpha)
    ydata <- data[, y_names[i]]
    fig <- fig %>% add_markers(x = xdata, y = ydata, name = y_names[i])
      # , marker = list(color = colour))
    fig <- fig %>% layout(xaxis = xlab, yaxis = ylab, title = title)
  }

  # Plot a 1:1 line
  ylim <- c(ymin, ymax)
  fig <- fig %>% add_lines(x = ylim, y = ylim, name = "1:1 line")
  #, line = c(color = "black"))
  return(fig)
}

#' Plot predicted data on the y-axis against observed data on the x-axis.
#' Data will be displayed as a series of points (not lines).
#' @param data: Dataframe of data.
#' @param colname_obs: Name of the observed data column.
#' @param names: Name of predicted series to be plotted.
#' @param units: Units of the data (used for display purposes).
#' @param var_name: Name of the variable being plotted.
#' @param write_legend: True to write a legend, false otherwise.
plot_pvo <- function(data, colname_x, y_names, units, var_name, write_legend) {

  # The data may include the full spinup period, with NA values for the
  # observations in during this period. We need to remove these NA rows when
  # drawing the P vs O plot. (Using spinup data only makes sense in a timeseries
  # plot.)
  data <- data[!is.na(data[[colname_x]]), ]

  # We want to use the same axes for all series. Ergo we need to know the
  # largest and smallest values in advance.
  x_data <- data[, colname_x]
  ymin <- min(x_data)
  ymax <- max(x_data)
  for (name in y_names) {
    col <- data[!is.na(data[, name]), name]
    ymin <- min(ymin, min(col))
    ymax <- max(ymax, max(col))
  }
  limits <- c(ymin, ymax)

  xlab <- parse(text = get_series_name(colname_x, units))
  ylab <- parse(text = get_series_name(var_name, units))

  # Get a colour scheme to be used for plotting.
  colours <- get_colour_palette(length(y_names))

  # Symbol type.
  pch <- pch_filled_circle

  # Plot each series.
  for (i in seq_len(length(y_names))) {
    colour <- adjustcolor(colours[i], alpha.f = symbol_alpha)
    if (i > 1) {
      # new = TRUE means, rather couter-intuitively, that the next call to
      # plot() will go onto the previous plot, rather than creating a new plot.
      par(new = TRUE)
    }
    plot(data[, colname_x], data[, y_names[i]], type = "p", xlab = ""
      , ylab = "", xlim = limits, ylim = limits, col = "transparent"
      , bg = colour, pch = pch, cex = pvo_point_size)
  }
  mtext(xlab, side_bottom, line = xaxis_label_line, cex = scale)
  mtext(ylab, side_left, line = yaxis_label_line, cex = scale)

  # Plot a 1:1 line
  oto_col <- "black"
  oto <- c(ymin, ymax)
  lines(oto, oto, type = "l", col = oto_col)

  if (write_legend) {
    oto_name <- "1:1 line"
    legend("topleft"
      , legend = c(y_names, oto_name)
      , lwd = c(rep(NA, length(y_names)), par("lwd"))
      , pch = c(rep(pch, length(y_names)), NA)
      , col = c(colours, oto_col))
  }
}

#' Aggregate data values in the specified table over all patches.
#'
#' E.g.:
#'
#' | day | patch | value |
#' |-----|-------|-------|
#' | 0   | 0     | 1     |
#' | 0   | 1     | 2     |
#' | 1   | 0     | 1.5   |
#' | 1   | 1     | 2.5   |
#'
#' Becomes
#'
#' | day | value |
#' |-----|-------|
#' | 0   | 1.5   |
#' | 1   | 2     |
#'
aggregate_patches <- function(data) {
  colname_patch <- "patch"
  if (! colname_patch %in% colnames(data)) {
    return(data)
  }

  col_index <- which(colnames(data) == colname_patch)
  npatch <- length(unique(data[[colname_patch]]))
  if (npatch > 1) {
    tmp <- data[data[[colname_patch]] == 1, ]
    tmp$Total <- .colMeans(data$Total, npatch, nrow(data) / npatch)
    data <- tmp
  }
  # Remove the patch column.
  data <- data[, -col_index]

  return(data)
}

#' Get the name of the observed data file for a given site.
#' @param site: Name of the site.
get_obs_file_name <- function(site) {
  return(paste0(obs_dir, "/", site, ".csv"))
}

read_version <- function(version, var, site, keep_pfts) {
  data <- read_site(version$path, site, var$file_name)
  colnames(data)[which(colnames(data) == var$pred_name)] <- version$name
  if (!keep_pfts) {
    data <- data[, c(colname_year, colname_day, version$name)]
  }
  return(data)
}

#' Read data for the specified variable and site from all versions of the model.
read_data <- function(versions, obs_dir, var, site, keep_spinup, keep_pfts) {

  # Column names for date variables in guess output files. NOTE: this would need
  # to be modified for annual outputs (ie just year in that case).
  date_cols <- c(colname_year, colname_day)

  # Read data from all sites into a single data table. This table will contain
  # year, date, and one column named after each version.
  data <- NULL
  for (version in versions) {
    version_data <- read_version(version, var, site, keep_pfts)
    if (is.null(data)) {
      data <- version_data
    } else {
      data <- merge(data, version_data, by = date_cols, sort = FALSE
        , all.x = TRUE, all.y = TRUE)
    }
  }

  # Aggregate over all patches.
  data <- aggregate_patches(data)

  obs_file <- get_obs_file_name(site)
  if (!is.null(obs_dir)) {
    if (file.exists(obs_file)) {
      # Column names different in observed data.
      observed <- read.csv(obs_file)
      if (var$obs_name %in% colnames(observed)) {
        observed <- observed[, c("year", "doy", var$obs_name)]
        colnames(observed)[3] <- colname_observed
        # We want to plot all predicted data, and all observed data for which we
        # have a corresponding prediction. Therefore all.x = TRUE, all.y = FALSE
        data <- merge(data, observed, by.x = date_cols, by.y = c("year", "doy")
          , sort = FALSE, all.x = TRUE, all.y = FALSE)
      } else {
        warning(paste0("Variable '", var$obs_name, "' does not exist in observed file '", obs_file, "'"))
      }
    } else {
      warning(paste0("Observed data file not found: '", obs_file, "'"))
    }
  }

  # Convert Year/Day columns into a single date column.
  data$Date <- as.POSIXct(paste(data$Year, data$Day + 1, sep = "-")
    , format = "%Y-%j")

  if (keep_pfts) {
    # Keep PFTs, remove year, day, lon, lat
    cols_to_remove <- c(colname_year, colname_day, "Lon", "Lat")
    data <- data[, !(colnames(data) %in% cols_to_remove)]
  } else {
    # Remove all columns except date, predicted, observed, and baseline.
    cols_to_keep <- c(colname_date)
    for (version in versions) {
      cols_to_keep <- c(cols_to_keep, version$name)
    }
    if (colname_observed %in% colnames(data)) {
      cols_to_keep <- c(cols_to_keep, colname_observed)
    }
    data <- data[, cols_to_keep]
  }

  return(data)
}

#' Get the names of all PFTs defined in the data table.
#' @param data: A data table.
get_pft_names <- function(data, versions) {
    not_pfts <- c(colname_date)
    for (version in versions) {
      not_pfts <- c(not_pfts, version$name)
    }
    not_pfts <- c(not_pfts, colname_observed)
    return(names(data)[!names(data) %in% not_pfts])
}

#' Print a message to the user.
#' @param message: The message to be displayed.
print <- function(message) {
  cat(paste0(message, "\n"))
}

#' Write the .tex data for a single figure.
#' @param graph_file_name: Name of the graph file.
#' @param site: Site name.
#' @param var_name: Name of the variable being plotted.
write_figure_tex <- function(graph_file_name, site, var_name) {
  w <- function(...) {
    text <- paste0(...)
    cat(text, file = figures_tex_name, append = TRUE)
  }
  w("\\begin{figure}\n")
  w("  \\label{fig:", tolower(site), "_", tolower(var_name), "}\n")
  w("  \\includegraphics{", graph_file_name, "}\n")
  w("  \\caption{", site, " ", escape_underscores(var_name), "}\n")
  w("\\end{figure}\n")
  w("\\clearpage\n")
}

#' Create graphs for a single variable for a single site from the given data,
#' and write them to a .png file in the specified output directory.
#'
#' @param data: The data to be graphed.
#' @param versions: The versions of the model to plot.
#' @param out_dir: Output directory into which graphs will be saved.
#' @param site: Name of the site being plotted.
#' @param var: Metadata of the variable being plotted.
#' @param scale: Graph size scaling factor. Increase for bigger text/lines/etc.
#' @param nsigfig: Number of significant figures used when writing r2/nse/etc.
#' @param show_pfts: TRUE to show individual PFTs' data.
#' @param combined: TRUE to write all graphs to a single file.
#' @param interp: TRUE to interpolate missing observations and draw them as a
#'                line. False otherwise.
#' @param width: Width of the graphs.
#' @param height: Height of the graphs.
#' @param write_tex: If TRUE, stats will be written to the output .tex file.
plot_site <- function(data, versions, out_dir, site, var, scale, nsigfig
  , show_pfts, combined, interp = TRUE, width = 640, height = 480) {
  # Get the names of y-axis variables (but NOT the observed column name).
  # Technically this is probably not necessary, because the only columns in the
  # data right now are date + the y variables. However it doesn't have a
  # performance impact, and I may change the read function to invalidate that
  # assumption later.
  y_names <- c()
  for (version in versions) {
    if (version$name %in% colnames(data)) {
      y_names <- c(y_names, version$name)
    }
  }
  if (show_pfts) {
    y_names <- c(y_names, get_pft_names(data, versions))
  }

  timeseries_names <- y_names
  if (colname_observed %in% colnames(data)) {
    timeseries_names <- c(timeseries_names, colname_observed)
  }

  legend_names <- timeseries_names
  if (!is.null(var$on_right)) {
    legend_names <- c(legend_names, var$on_right$title)
  }

  # Choose a title.
  site_title <- paste0(var$title, " ~ (", site, ")")

  # Determine output filename. E.g. out_dir/site_var.png
  # Note: this variable is just a prefix. There may be more text appended later,
  # depending on other settings.
  pfx <- paste0(site, "_", var$name)

  # Create output directory if it doesn't already exist.
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  plotlies <- list()

  #' Initialise the graphics device ready for plotting.
  init_graph <- function(out_file) {
    file_name <<- out_file

    # Open graphics device writing to png file.
    png(out_file, width = width, height = height)

    # Initial graphing parameters.
    par(mfrow = mfrow, cex = scale, lwd = scale, xpd = NA)
  }

  #' Finish the graph by adding a title and closing the graphics device.
  finish_graph <- function() {
    # Add a legend.
    write_legend(var, legend_names, inside_graph = !combined)

    # Add a title.
    mtext(parse(text = site_title), line = -2, outer = TRUE, cex = scale + 1)

    # Close the png file connection.
    dev.off()

    html_out <- gsub(".png", ".html", file_name)
    # xlab <- parse(text = xcol)
    # ylab <- parse(text = get_series_name(var$title, var$units))
    plt_out <- subplot(plotlies, nrows = mfrow[1])
    title <- paste0(var$title, " (", site, ")")
    plt_out <- plt_out %>% layout(title = title)
    plt_out <- plt_out %>% config(scrollZoom = TRUE)
    htmlwidgets::saveWidget(plt_out, html_out, selfcontained = FALSE
      , libdir = "lib")
    plotlies <<- list()
  }

  # Determine graph layout.
  mfrow <- if (combined) c(2, 2) else c(1, 1)

  # Create a new png file at this location.
  sfx <- if (!combined) "_timeseries" else ""
  file_name <- paste0(out_dir, "/", pfx, sfx, ".png")
  init_graph(file_name)

  if (combined) {
    par(mar = mar_top_row)
  }

  # Create the timeseries plot.
  plot_timeseries(data, colname_date, timeseries_names, var, interp = interp)
  if (use_plotly) {
    plt <- plot_timeseries_plotly(data, colname_date, timeseries_names, var
      , interp = interp)
    plotlies <- append(plotlies, list(plt))
  }

  # If plotting in individual mode, finish off this plot then get ready for the
  # next one.
  if (!combined) {
    if (colname_observed %in% colnames(data)) {
      finish_graph()
      init_graph(paste0(out_dir, "/", pfx, "_pvo.png"))
    }
  }

  if (combined) {
    par(mar = mar_top_row)
  }

  on_right <- var$on_right
  var$on_right <- NULL
  if (colname_observed %in% colnames(data)) {
    plot_pvo(data, colname_observed, y_names, var$units, var$title, !combined)

    if (use_plotly) {
      plt <- plot_pvo_plotly(data, colname_observed, y_names, var$units
        , var$title)
      plotlies <- append(plotlies, list(plt))
    }

    colours <- get_colour_palette(length(timeseries_names))
    nstat <- if (write_baseline_stats) length(versions) else 1
    inset <- c(0, 0)
    for (i in 1:nstat) {
      version <- versions[[i]]
      idx <- which(timeseries_names == version$name)[1]
      colour <- colours[idx]
      obs <- data[[colname_observed]]
      pred <- data[[version$name]]

      size <- write_stats(obs, pred, var$units, nsigfig, colour
      , var$name, site, inset = inset, write_tex = combined)
      inset[1] <- inset[1] + size$rect$w
    }
    if (combined) {
      write_figure_tex(file_name, site, var$name)
    }
  }

  if (combined) {
    par(mar = mar_bottom_row)
  }

  if (!combined) {
    if (colname_observed %in% colnames(data)) {
      finish_graph()
      init_graph(paste0(out_dir, "/", pfx, "_seasonal.png"))
    }
  }

  plot_seasonal(data, colname_date, timeseries_names, var, interp = interp)
  if (use_plotly) {
    plt <- plot_seasonal_plotly(data, colname_date, timeseries_names, var
      , interp = interp)
    plotlies <- append(plotlies, list(plt))
  }
  var$on_right <- on_right
  if (!combined) {
    site_title <- paste0("Mean ~ Annual ~ ", site_title)
  }
  finish_graph()
}

#' Link all files under the input directory (recursively) which match the file
#' name pattern provided to a file with the same name in the output directory.
#' @param in_dir: Input directory (ie the search path).
#' @param out_dir: Output directory. Will be created if required.
#' @param pattern: Only file names matching this pattern will be linked.
#' @param copy: Iff true, files will be copied, not linked.
#' @param hard_link: Ignored if copy = TRUE. True for a hard link, false for a
#' symlink.
link_all <- function(in_dir, out_dir, pattern = "*.png", copy = TRUE,
  hard_link = FALSE) {
  # Create output directory if it doesn't already exist.
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Enumerate files matching the pattern under the input directory.
  bysite_files <- list.files(path = in_dir, pattern = pattern
    , recursive = TRUE, full.names = TRUE)

  # Function pointer to the correct link function.
  if (copy) {
    link_func <- file.copy
  } else {
    link_func <- if (hard_link) file.link else file.symlink
  }

  # Symlink each file.
  for (file in bysite_files) {
    out_name <- paste0(out_dir, "/", basename(file))
    if (file.exists(out_name)) {
      file.remove(out_name)
    }
    link_func(file, out_name)
  }
}

#' Create all plots for the specified site using the given data.
#'
#' @param versions: Versions of the model for which data will be read/plotted.
#' @param data: Dataframe containing predictd and observed timeseries.
#' @param site: Name of the site (used in plot labels, dir names, etc.).
#' @param var: Variable name created by a call to define_variable().
#' @param interp: True to enable interpolation of observations. False otherwise.
make_plots_site <- function(versions, data, site, var, interp = TRUE) {

  # Generate both combined and separate graphs.
  comb_bysite <- out_dir
  sepa_bysite <- out_dir

  if (create_allsite_dir) {
    comb_bysite <- paste0(comb_bysite, "/", by_site_dir)
    sepa_bysite <- paste0(sepa_bysite, "/", by_site_dir)
  }

  comb_bysite <- paste0(comb_bysite, "/combined/", site)
  sepa_bysite <- paste0(sepa_bysite, "/separate/", site)

  if (colname_observed %in% colnames(data)) {
    # Only generate a combined plot if we actually have observed data for this
    # variable.
    plot_site(data, versions, comb_bysite, site, var, scale, num_figs
      , show_pfts, TRUE, interp = interp, width = width, height = height)
  }
  plot_site(data, versions, sepa_bysite, site, var, scale, num_figs
    , show_pfts, FALSE, interp = interp, width = width, height = height)

  if (create_allsite_dir) {
    comb_allsite <- paste0(all_site_dir, "/combined")
    sepa_allsite <- paste0(all_site_dir, "/separate")

    link_all(comb_bysite, comb_allsite)
    link_all(sepa_bysite, sepa_allsite)
  }
}

#' Create all plots for a particular variable/site combination, using the global
#' user input variables.
#'
#' @param versions: Versions of the model for which data will be read/plotted.
#' @param site: Name of the site.
#' @param var: Variable to be plotted.
#' @param min_date: If provided, only data after this data will be plotted.
plotting_site <- function(versions, site, var, min_date = NULL) {
  print(paste0("Generating ", var$name, " plots for site ", site, "..."))

  # Read data for this site.
  data <- read_data(versions, obs_dir, var, site, show_spinup, show_pfts)

  if (is.null(min_date) && colname_observed %in% colnames(data)) {
    min_date <- data[1, colname_date]
  }
  if (!is.null(min_date) && !colname_observed %in% colnames(data)) {
    data <- data[data$Date >= min_date, ]
  }

  make_plots_site(versions, data, site, var)

  return(min_date)
}

get_soil_moisture_colname <- function(layer) {
  if (layer < 0 || layer > 14) {
    stop("Only 15 soil layers exist (0-14)")
  }
  return(paste0(sw90$name, "_", layer))
}

#' Calc cumulative 90cm soil moisture content.
#'
#' @param data: Dataframe containing layered soil moisture data.
calc_sw90 <- function(data) {
  result <- data[[get_soil_moisture_colname(0)]]
  nlayer <- 9
  for (i in 1:(nlayer - 1)) {
    result <- result + data[[get_soil_moisture_colname(i)]]
  }
  result <- result / nlayer
  return(result)
}

#' Get a dataframe containing predicted, observed, and baseline timeseries of
#' cumulative soil moisture at 90cm depth.
#'
#' @param site name.
get_sw90_data <- function(site, versions) {

  data <- NULL
  for (version in versions) {
    predictions <- read_data(list(version), obs_dir, sw90, site, FALSE, TRUE)
    sw90_data <- calc_sw90(predictions)
    predictions[which(colnames(predictions) == version$name)] <- sw90_data
    if (is.null(data)) {
      data <- predictions
    } else {
      data <- merge(data, predictions[, c(colname_date, version$name)]
        , by = colname_date, all = TRUE, sort = FALSE)
    }
  }
  data <- data[order(data[[colname_date]]), ]
  return(data)
}

#' Get the index of the LPJ-Guess soil layer which contains the specified depth.
#' @param depth: The depth in cm.
get_closest_layer <- function(depth_cm) {
  return(as.integer(floor(depth_cm / lpj_layer_height)))
}

#' @param versions: Versions of the model for which data will be read/plotted.
make_sw_plots_site <- function(site, versions) {
  # Comparison to smips data. Need to sum swmm over top 90cm.
  cat(paste0("Generating sw90 plots for site ", site, "...\n"))
  sw90_data <- get_sw90_data(site, versions)
  make_plots_site(versions, sw90_data, site, sw90)

  precip <- define_variable(colname_precip, "mm", "Precipitation"
    , file_name = filename_precip)
  precip_data <- read_data(list(versions[[1]]), NULL, precip, site, FALSE, FALSE)
  precip_data <- precip_data[, c(colname_date, versions[[1]]$name)]
  colnames(precip_data)[2] <- colname_precip

  obs <- read.csv(get_obs_file_name(site), header = TRUE, nrows = 1)
  pfx <- "swvol_"
  swcols <- colnames(obs)[which(startsWith(colnames(obs), pfx))]
  for (observed_layer in swcols) {
    depth_cm <- sub(pfx, "", observed_layer)
    lpj_layer <- get_closest_layer(as.integer(depth_cm))
    lpj_name <- paste0("swvol_", lpj_layer)
    title <- paste0(depth_cm, " * cm ~ Soil ~ Moisture")
    file_name <- "dave_swvol.out"
    var <- define_variable(lpj_name, "mm * mm ^ -1", title
      , obs_name = observed_layer
      , file_name = file_name, pred_name = lpj_name
      , on_right = precip, right_scale = scale_precip)

    cat(paste0("Generating ", lpj_name, " plots for site ", site, "\n"))
    data <- read_data(versions, obs_dir, var, site, FALSE, TRUE)
    data <- merge(data, precip_data, by = colname_date, all = FALSE)
    make_plots_site(versions, data, site, var)
  }
}

write_stats_table <- function(table, name) {
  fmt <- function(x, ndigits = 3) {
    return(round(x, ndigits))
  }
  w <- function(text) {
    cat(text, file = stats_file_name, append = TRUE)
  }

  start_stats_table(name)

  for (i in seq_len(nrow(table))) {
    row <- table[i, ]
    site <- rownames(table)[i]

    line <- site
    # line <- paste0("\\hyperref[fig:", tolower(site), "_", tolower(var$name)
    #   , "]{", site, "}")

    for (j in seq_along(vars)) {
      var <- vars[[j]]
      href <- paste0("\\hyperref[fig:", tolower(site), "_", tolower(var$name)
      , "]{", fmt(row[j]), "}")
      line <- paste(line, href, sep = " & ")
    }
    w(line)
    w(" \\\\ \n")
  }

  finish_stats_table(name)
}

start_stats_table <- function(name) {
  w <- function(text) {
    cat(text, file = stats_file_name, append = TRUE)
  }

  w("\\begin{longtable}{c c c c c c c}\n")
  w(paste0("\\caption[]{", name, "}\\\\\n"))
  w("\\hline\\hline\n")
  w("Site")
  for (var in vars) {
    w(paste0(" & ", var$name))
  }
  w(" \\\\ [0.5ex]\n")
  w("\\hline\n")
  w("\\endhead\n")
}

finish_stats_table <- function(name) {
  w <- function(text) {
    cat(text, file = stats_file_name, append = TRUE)
  }

  w("\\hline\n")
  w("\\end{longtable}\n")
  w(paste0("\\label{table:", name, "}\n"))
}

write_tex_preamble <- function() {
  w <- function(text, append = TRUE) {
    cat(text, file = stats_file_name, append = append)
  }

  w("\\documentclass{article}\n", append = FALSE)
  w("\\usepackage{geometry}\n")
  w("\\usepackage{graphicx}\n")
  w("\\usepackage{hyperref}\n")
  w("\\usepackage{longtable}\n")
  w("\\usepackage[table]{xcolor}\n")
  w("\n")
  w("% Alternating row colours.\n")
  w("\\definecolor{lightgray}{gray}{0.9}\n")
  w("\\let\\oldlongtable\\longtable\n")
  w("\\let\\endoldlongtable\\endlongtable\n")
  w("\\renewenvironment{longtable}{\\rowcolors{2}{lightgray}{white}\n")
  w("\\oldlongtable}{\\endoldlongtable}\n")
  w("\n")
  w("\\begin{document}\n")
  # start_stats_table()
}

read_file <- function(file_name) {
  return(paste(readLines(file_name), collapse = "\n"))
}

write_stats_suffix <- function() {
  w <- function(...) {
    text <- paste0(...)
    cat(text, file = stats_file_name, append = TRUE)
  }
  # finish_stats_table()
  w("\\newgeometry{margin=0bp}\n")
  w("\\eject \\pdfpagewidth=", width, "bp \\pdfpageheight=", height, "bp\n")

  figures_tex <- read_file(figures_tex_name)
  w(figures_tex)
  # file.remove(figures_tex_name)

  w("\\end{document}\n")
}

################################################################################
# Main entrypoint.
################################################################################

if (dir.exists(all_site_dir)) {
  unlink(all_site_dir, recursive = TRUE)
}

if (file.exists(figures_tex_name)) {
  file.remove(figures_tex_name)
}
# stats_csv_header <- paste("site", "variable", "r2", "rmse", "nse", "rsr", "bias"
#   , sep = ",")
# cat(stats_csv_header, file = stats_file_name, append = FALSE)
# cat("\n", file = stats_file_name, append = TRUE)

for (site in sites) {
  min_date <- NULL
  for (var in vars) {
    min_date <- plotting_site(versions, site, var, min_date)
  }

  make_sw_plots_site(site, versions)
}

# Generate .tex file.
write_tex_preamble()

write_stats_table(r2_table, "r2")
write_stats_table(rmse_table, "rmse")
write_stats_table(nse_table, "nse")
write_stats_table(rsr_table, "rsr")
write_stats_table(bias_table, "bias")

write_stats_suffix()

print(paste0("Charts successfully generazted in ", out_dir))
