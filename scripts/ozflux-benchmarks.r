################################################################################
# Global Variables. Should not be modified by users. See bottom for user inputs.
################################################################################

# Additional multiplier for legends text.
legend_scale <- 2

# Additional multiplier for title text.
title_scale <- 3

# Colour palette optimised for people with various kinds of colour-blindness.
# Wong, B. (2011) Color blindness, Nature Methods, Vol 8, No. 6.
cb_colours <- c(
  "#e69f00",
  "#56b4e9",
  "#009e73",
  "#0072b2",
  "#d55e00",
  "#cc79a7",
  "#f0e442",
  "#000000"
)

# Name of the observed column in the processed data.
colname_observed <- "Observed"

# Name of the total column in the processed data.
colname_total <- "dave"

# Name of the date column in the processed data.
colname_date <- "Date"

# Name of the baseline total column in the processed data.
colname_baseline_total <- "daily-grass"

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
  ozflux <- paste0(guess, "/benchmarks/ozflux")
  filename <- paste0(ozflux, "/", site, "/out/", var, ".out")
  return(read_outfile(filename))
}

#' Plot one column (xcol) of data against all of the column names specified in
#' ycols, on the same graph, using the same y-axis.
#'
#' @param data: Dataframe of data.
#' @param xcol: Name of the x-data column.
#' @param ycols: Vector of names of y-data columns to be plotted.
#' @param units: Units of the variable being displayed.
#' @param var_name: Friendly/display name of the variable being displayed.
plot_timeseries <- function(data, xcol, ycols, units, var_name, ...) {
  # We want to plot all series using the same y-axis. Ergo, we need to know the
  # largest and smallest y-values in advance.
  ymax <- -1e300
  ymin <- 1e300
  for (i in seq_len(length(ycols))) {
    col <- data[!is.na(data[, ycols[i]]), ycols[i]]
    ymin <- min(ymin, min(col))
    ymax <- max(ymax, max(col))
  }

  xlab <- xcol
  ylab <- paste0(var_name, " (", units, ")")

  # Get colours used for plotting the data.
  colours <- get_colour_palette(length(ycols))

  for (i in seq_len(length(ycols))) {
    if (i > 1) {
      # new = TRUE means, rather couter-intuitively, that the next call to
      # plot() will go onto the previous plot, rather than creating a new plot.
      par(new = TRUE)
    }
    plot(data[, xcol], data[, ycols[i]], type = "l", col = colours[i]
      , xlab = xlab, ylab = ylab, xlim = NULL, ylim = c(ymin, ymax), ...)
  }

  legend("topleft", legend = ycols, text.col = colours, lwd = par("lwd")
    , col = colours)
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
  return(cor(x, y)^2)
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
#' An efficiency of 1 means the predictions perfectly match the observations.
#' An efficiency of 0 means that the predictions are equally accurate predictors
#' as the mean of the observed data. An efficiency of < 0 means that the mean of
#' the observations is a better predictor than the model predictions.
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

#' Compute RMSE to standard deviation of observations (0 - Inf).
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

write_stats <- function(observations, predictions, units, num_figs) {
  r2 <- compute_r2(observations, predictions)
  rmse <- compute_rmse(observations, predictions)
  nse <- compute_nse(observations, predictions)
  rsr <- compute_rsr(observations, predictions)

  legend("topright", legend = c(
    paste0("r2 = ", signif(r2, num_figs)),
    paste0("rmse = ", signif(rmse, num_figs)),
    paste0("nse = ", signif(nse, num_figs)),
    paste0("rsr = ", signif(rsr, num_figs))
  ))
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
#' @param data: Dataframe of data.
#' @param colname_obs: Name of the observed data column.
#' @param names: Name of predicted series to be plotted.
#' @param units: Units of the data (used for display purposes).
#' @param var_name: Name of the variable being plotted.
plot_pvo <- function(data, colname_x, y_names, units, var_name) {

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

  xlab <- paste0(colname_x, " (", units, ")")
  ylab <- paste0(var_name, " (", units, ")")
  pch <- 21 # Filled circle

  # Get a colour scheme to be used for plotting.
  colours <- get_colour_palette(length(y_names))

  # Plot each series.
  for (i in seq_len(length(y_names))) {
    if (i > 1) {
      # new = TRUE means, rather couter-intuitively, that the next call to
      # plot() will go onto the previous plot, rather than creating a new plot.
      par(new = TRUE)
    }
    plot(data[, colname_x], data[, y_names[i]], type = "p", xlab = xlab
      , ylab = ylab, xlim = limits, ylim = limits, col = colours[i], pch = pch)
  }

  # Plot a 1:1 line
  oto_name <- "1:1 line"
  oto_col <- "black"
  oto <- c(ymin, ymax)
  lines(oto, oto, type = "l", col = oto_col)

  legend("topleft"
    , legend = c(y_names, oto_name)
    , lwd = c(rep(NA, length(y_names)), par("lwd"))
    , pch = c(rep(pch, length(y_names)), NA)
    , col = c(colours, oto_col))
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

read_data <- function(guess_dir, obs_dir, baseline_dir = NULL, var_name, site
  , keep_spinup, keep_pfts) {
  colname_year <- "Year"
  colname_day <- "Day"

  data <- read_site(guess_dir, site, var_name)

  data <- aggregate_patches(data)

  # Column names for date variables in guess output files. NOTE: this would need
  # to be modified for annual outputs (ie just year in that case).
  date_cols <- c(colname_year, colname_day)

  obs_file <- paste0(obs_dir, "/", site, ".csv")
  if (file.exists(obs_file)) {
    # Column names different in observed data.
    observed <- read.csv(obs_file)
    if (var_name %in% colnames(observed)) {
      observed <- observed[, c("year", "doy", var_name)]
      colnames(observed)[3] <- colname_observed
      data <- merge(data, observed, by.x = date_cols, by.y = c("year", "doy")
        , sort = FALSE, all.x = keep_spinup)
    } else {
      warning(paste0("Variable '", var_name, "' does not exist in observed file '", obs_file, "'"))
    }
  } else {
    warning(paste0("Observed data file not found: '", obs_file, "'"))
  }

  has_baseline_total <- FALSE
  if (!is.null(baseline_dir)) {
    baseline_file <- paste0(baseline_dir, "/", site, "/output/", var$name
      , ".out")
    if (file.exists(baseline_file)) {
      has_baseline_total <- TRUE
      baseline_data <- read_outfile(baseline_file)
      baseline_total_index <- which(colnames(baseline_data) == "Total")
      colnames(baseline_data)[baseline_total_index] <- colname_baseline_total
      baseline_data <- baseline_data[, c(date_cols, colname_baseline_total)]
      data <- merge(data, baseline_data, by = date_cols, sort = FALSE
        , all.x = keep_spinup)
    } else {
      warning(paste0("WARNING: baseline directory provided, but baseline output file '", baseline_file, "' does not exist."))
    }
  }

  data$Date <- as.POSIXct(paste(data$Year, data$Day + 1, sep = "-")
    , format = "%Y-%j")

  # Rename column 'Total' to 'Predicted'.
  in_colname_total <- "Total"
  total_idx <- which(colnames(data) == in_colname_total)
  colnames(data)[total_idx] <- colname_total

  if (keep_pfts) {
    # Keep PFTs, remove year, day, lon, lat
    cols_to_remove <- c(colname_year, colname_day, "Lon", "Lat")
    data <- data[, !(colnames(data) %in% cols_to_remove)]
  } else {
    # Remove all columns except date, predicted, observed, and baseline.
    cols_to_keep <- c(colname_date, colname_total)
    if (colname_observed %in% colnames(data)) {
      cols_to_keep <- c(cols_to_keep, colname_observed)
    }
    if (has_baseline_total) {
      cols_to_keep <- c(cols_to_keep, colname_baseline_total)
    }
    data <- data[, cols_to_keep]
  }

  return(data)
}

#' Get the names of all PFTs defined in the data table.
#' @param data: A data table.
get_pft_names <- function(data) {
    not_pfts <- c(
        colname_date
      , colname_total
      , colname_observed
      , colname_baseline_total)
    return(names(data)[!names(data) %in% not_pfts])
}

#' Create a variable 'object' given its lpj-guess name, units and display name.
#' @param name: Name of the variable in LPJ-Guess. This is also the name of the
#'              column in the observed data.
#' @param units: Units of the variable.
#' @param display_name: Friendly name of the variable to go in plots/legends.
define_variable <- function(name, units, display_name) {
  variable <- c()
  variable$name <- name
  variable$units <- units
  variable$title <- display_name
  return(variable)
}

#' Print a message to the user.
#' @param message: The message to be displayed.
print <- function(message) {
  cat(paste0(message, "\n"))
}

#' Create graphs for a single variable for a single site from the given data,
#' and write them to a .png file in the specified output directory.
#'
#' @param data: The data to be graphed.
#' @param out_dir: Output directory into which graphs will be saved.
#' @param site: Name of the site being plotted.
#' @param units: Units of the variable being plotted.
#' @param title: Friendly/display name of the variable being plotted.
#' @param scale: Graph size scaling factor. Increase for bigger text/lines/etc.
#' @param nsigfig: Number of significant figures used when writing r2/nse/etc.
#' @param show_pfts: TRUE to show individual PFTs' data.
#' @param combined: TRUE to write all graphs to a single file.
#' @param width: Width of the graphs.
#' @param height: Height of the graphs.
plot_site <- function(data, out_dir, site, units, title, scale, nsigfig
  , show_pfts, combined, width = 640, height = 480) {
  # Get the names of y-axis variables (but NOT the observed column name).
  # Technically this is probably not necessary, because the only columns in the
  # data right now are date + the y variables. However it doesn't have a
  # performance impact, and I may change the read function to invalidate that
  # assumption later.
  y_names <- c(colname_total)
  if (show_pfts) {
    y_names <- c(y_names, get_pft_names(data))
  }

  # Remember, we may not necessarily have baseline data.
  if (colname_baseline_total %in% colnames(data)) {
    y_names <- c(y_names, colname_baseline_total)
  }

  timeseries_names <- y_names
  if (colname_observed %in% colnames(data)) {
    timeseries_names <- c(timeseries_names, colname_observed)
  }

  # Choose a title.
  site_title <- paste0(title, " (", site, ")")

  # Determine output filename. E.g. out_dir/site_var.png
  # Note: this variable is just a prefix. There may be more text appended later,
  # depending on other settings.
  pfx <- paste0(site, "_", var$name)

  # Create output directory if it doesn't already exist.
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  #' Initialise the graphics device ready for plotting.
  init_graph <- function() {
    # Open graphics device writing to png file.
    png(out_file, width = width, height = height)

    # Initial graphing parameters.
    par(mfcol = mfcol, cex = scale, lwd = scale)
  }

  #' Finish the graph by adding a title and closing the graphics device.
  finish_graph <- function() {
    # Add a title.
    mtext(site_title, line = -2, outer = TRUE, cex = scale + 1)

    # Close the png file connection.
    dev.off()
  }

  # Determine graph layout.
  mfcol <- if (combined) c(1, 2) else c(1, 1)

  # Create a new png file at this location.
  sfx <- if (!combined) "_timeseries" else ""
  out_file <- paste0(out_dir, "/", pfx, sfx, ".png")
  init_graph()

  # Create the timeseries plot.
  plot_timeseries(data, colname_date, timeseries_names, units, title)

  # If plotting in individual mode, finish off this plot then get ready for the
  # next one.
  if (!combined) {
    if (colname_observed %in% colnames(data)) {
      finish_graph()
      out_file <- paste0(out_dir, "/", pfx, "_pvo.png")
      init_graph()
    }
  }

  if (colname_observed %in% colnames(data)) {
    plot_pvo(data, colname_observed, y_names, units, title)
    write_stats(data[[colname_observed]], data[[colname_total]], units, nsigfig)
  }
  finish_graph()
}

################################################################################
# User Inputs
################################################################################

# Path to LPJ-Guess repository.
guess <- "../dave-daily-grass-photosynthesis"

# Path to directory containing observed data.
obs_dir <- "obs/fluxes"

# Optional path to directory containing baseline data. Set to NULL if not using
# baseline data.
baseline_dir <- "baseline"

# Desired output directory.
out_dir <- "ozflux-graphs"

# Names of sites to be plotted.
sites <- c(
  "SturtPlains_L6_20080828_20220218",
  "Yanco_L6_20130101_20220218"
)

# Variables to be plotted
vars <- list(
  define_variable("gpp", "kgC/m2/day", "GPP"),
  define_variable("resp", "kgC/m2/day", "Respiration"),
  define_variable("lai", "m2/m2", "LAI")
)

# TRUE to plot data during spinup period, FALSE otherwise.
show_spinup <- FALSE

# TRUE to show individual pfts' data. FALSE otherwise (ie just show totals).
show_pfts <- FALSE

# Plot scaling. Increase this to make everything bigger.
scale <- 2

# Width and height (in px) of the generated graphs.
width <- 1920
height <- 1080

# Number of sig figs used when writing stats like RMSE/NSE.
num_figs <- 2

################################################################################
# End of user inputs
################################################################################

for (site in sites) {
  min_date <- NULL
  for (var in vars) {
    print(paste0("Generating ", var$name, " plots for site ", site, "..."))

    # Read data for this site.
    data <- read_data(guess, obs_dir, baseline_dir, var$name, site, show_spinup
      , show_pfts)

    if (is.null(min_date) && colname_observed %in% colnames(data)) {
      min_date <- data[1, colname_date]
    }
    if (!is.null(min_date) && !colname_observed %in% colnames(data)) {
      data <- data[data$Date >= min_date, ]
    }

    # Generate both combined and separate graphs.
    combined_path <- paste0(out_dir, "/combined")
    separate_path <- paste0(out_dir, "/separate")

    if (colname_observed %in% colnames(data)) {
      # Only generate a combined plot if we actually have observed data for this
      # variable.
      plot_site(data, combined_path, site, var$units, var$title, scale, num_figs
        , show_pfts, TRUE, width = width, height = height)
    }
    plot_site(data, separate_path, site, var$units, var$title, scale, num_figs
      , show_pfts, FALSE, width = width, height = height)
  }
}

print(paste0("Charts successfully generated in ", out_dir))
