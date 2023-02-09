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
  "#000000",
  "#e69f00",
  "#56b4e9",
  "#009e73",
  "#0072b2",
  "#d55e00",
  "#cc79a7",
  "#f0e442"
)

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

plot_timeseries <- function(data, xcol, ylab, ...) {
  xcol_index <- which(colnames(data) == xcol)

  n_data_cols <- ncol(data) - 1
  if (length(cb_colours) >= n_data_cols) {
    colours <- cb_colours[1:n_data_cols]
  } else {
    # More columns of data to plot then supported by the above colour palette.
    # Fallback to hcl.colors(). This shouldn"t happen unless plotting lots of
    # PFTs together.
    colours <- hcl.colors(n_data_cols)
  }

  ymax <- -1e300
  ymin <- 1e300

  for (i in seq_len(ncol(data))) {
    if (i != xcol_index) {
      ymin <- min(ymin, min(data[!is.na(data[,i]),i]))
      ymax <- max(ymax, max(data[!is.na(data[,i]),i]))
    }
  }

  columns <- c()

  first_series = TRUE
  series_index <- 0
  for (i in seq_len(ncol(data))) {
    if (i != xcol_index) {
      if (first_series) {
        first_series <- FALSE
        axes <- TRUE
      } else {
        par(new = TRUE)
        axes <- FALSE
      }
      series_index <- series_index + 1
      columns <- c(columns, colnames(data)[i])
      colour <- colours[series_index]
      plot(data[[xcol]], data[, i], type = "l", col = colour, ylab = ylab
        , xlab = xcol, axes = axes, xlim = NULL, ylim = c(ymin, ymax), ...)
    }
  }
  legend("topleft", legend = columns, text.col = colours, lwd = par()$lwd
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

plot_pvo <- function(observations, predictions, units, series_name, scale
  , col_pred, col_baseline = NA, baseline_data = NULL) {
  min_obs <- min(observations[!is.na(observations)])
  min_pred <- min(predictions[!is.na(predictions)])
  ymin <- min(min_obs, min_pred)

  max_obs <- max(observations[!is.na(observations)])
  max_pred <- max(predictions[!is.na(predictions)])
  ymax <- max(max_obs, max_pred)

  names <- c(series_name)
  if (is.null(baseline_data)) {
    colours <- c(col_pred)
  } else {
    colours <- c(col_pred, col_baseline)
    ymin <- min(ymin, min(baseline_data))
    ymax <- max(ymax, max(baseline_data))
    names <- c(names, "Baseline")
  }
  names <- c(names, "1:1 line")
  limits <- c(ymin, ymax)

  xlab <- paste0("Observed (", units, ")")
  ylab <- paste0("Predicted (", units, ")")
  pch <- 21 # Filled circle

  plot(observations, predictions, type = "p", xlab = xlab, ylab = ylab
    , xlim = limits, ylim = limits, col = colours[1]
    , pch = pch)

  if (!is.null(baseline_data)) {
    par(new = TRUE)
    plot(observations, baseline_data, type = "p", xlab = xlab, ylab = ylab
    , xlim = limits, ylim = limits, col = colours[2]
    , pch = pch)
  }

  # Plot a 1:1 line
  one_to_one_col <- "black"
  one_to_one <- c(ymin, ymax)
  lines(one_to_one, one_to_one, type = "l", col = one_to_one_col)

  ns <- length(names) - 1
  legend("topleft", legend = names, lwd = c(rep(NA, ns), par("lwd"))
    , pch = c(rep(pch, ns), NA), col = c(colours, one_to_one_col))
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
    tmp <- data[data[[colname_patch]] == 1,]
    tmp$Total <- .colMeans(data$Total, npatch, nrow(data) / npatch)
    data <- tmp
  }
  # Remove the patch column.
  data <- data[,-col_index]

  return(data)
}

plot_site <- function(guess, obs_dir, var_name, site, units, title, scale
  , baseline_dir, show_spinup = FALSE, show_pfts = FALSE, width = 640
  , height = 480) {
  data <- read_site(guess, site, var_name)

  data <- aggregate_patches(data)

  colname_observed <- "Observed"
  colname_total <- "Total"
  colname_date <- "Date"
  colname_year <- "Year"
  colname_day <- "Day"

  obs_file <- paste0(obs_dir, "/", site, ".csv")
  if (!file.exists(obs_file)) {
    stop(paste0("Observed data file not found: '", obs_file, "'"))
  }

  # Column names for date variables in guess output files. NOTE: this would need
  # to be modified for annual outputs (ie just year in that case).
  date_cols <- c(colname_year, colname_day)

  # Column names different in observed data.
  observed <- read.csv(obs_file)[,c("year", "doy", var_name)]
  colnames(observed)[3] <- colname_observed
  data <- merge(data, observed, by.x = date_cols, by.y = c("year", "doy")
    , sort = FALSE, all.x = show_spinup)

  has_baseline_total <- FALSE
  if (!is.null(baseline_dir)) {
    baseline_file <- paste0(baseline_dir, "/", site, "/output/", var$name
      , ".out")
    if (file.exists(baseline_file)) {
      has_baseline_total <- TRUE
      baseline_data <- read_outfile(baseline_file)
      baseline_total_index <- which(colnames(baseline_data) == "Total")
      colname_baseline_total <- "Baseline"
      colnames(baseline_data)[baseline_total_index] <- colname_baseline_total
      baseline_data <- baseline_data[, c(date_cols, colname_baseline_total)]
      data <- merge(data, baseline_data, by = date_cols, sort = FALSE
        , all.x = show_spinup)
    } else {
      warning(paste0("WARNING: baseline directory provided, but baseline output file '", baseline_file, "' does not exist."))
    }
  }

  data$Date <- as.POSIXct(paste(data$Year, data$Day + 1, sep = "-")
    , format = "%Y-%j")

  # Drop first N cols:
  # Year,Day,Lon,Lat
  ncol_drop <- 4
  data <- data[,-seq(1, ncol_drop)]

  total_idx <- which(colnames(data) == colname_total)
  if (!show_pfts) {
    cols_to_keep <- c(colname_date, colname_total, colname_observed)
    if (has_baseline_total) {
      cols_to_keep <- c(cols_to_keep, colname_baseline_total)
    }
    data <- data[,cols_to_keep]
    total_idx <- which(colnames(data) == colname_total)
    colname_total <- "Predicted"
    colnames(data)[total_idx] <- colname_total

    if (var_name == "resp") {
      data[,total_idx] <- -data[,total_idx]
    }
  }

  ylab <- paste0(title, " (", units, ")")
  site_title <- paste0(title, " (", site, ")")

  # E.g. out_dir/site_var.png
  out_file <- paste0(out_dir, "/", site, "_", var, ".png")
  png(out_file, width = width, height = height)

  # Number of sig figs used when writing stats like RMSE/NSE
  num_figs <- 2

  # If showing spinup data in first plot, the data will contain NA values for
  # the observations during the spinup period. We need to remove these NA rows
  # before we show the predicted vs observed plot.
  if (show_spinup) {
    data <- data[!is.na(data[[colname_observed]]),]
  }

  nc <- ncol(data) - 1
  if (nc <= length(cb_colours)) {
    predicted_colour <-  cb_colours[1]
    baseline_colour <- cb_colours[3]
  } else {
    predicted_colour <- hcl.colors(nc)[min(nc, total_idx)]
    baseline_colour <- hcl.colors(nc)[min(nc, baseline_total_index)]
  }

  par(mfcol = c(1, 2), cex = scale, lwd = scale)
  plot_timeseries(data, colname_date, ylab)
  plot_pvo(data[[colname_observed]], data[[colname_total]], units, title, scale
    , predicted_colour, baseline_colour
    , baseline_data = if(has_baseline_total) data[[colname_baseline_total]])
  write_stats(data[[colname_observed]], data[[colname_total]], units, num_figs)
  mtext(site_title, line = -2, outer = TRUE, cex = scale + 1)

  dev.off()

  return(data)
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
  define_variable("resp", "kgC/m2/day", "Respiration")
)

# TRUE to plot data during spinup period, false otherwise.
show_spinup <- TRUE

# Plot scaling. Increase this to make everything bigger.
scale <- 2

# Width and height (in px) of the generated graphs.
width <- 1920
height <- 1080

################################################################################
# End of user inputs
################################################################################

# Create output directory if it doesn't already exist.
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

for (site in sites) {
  for (var in vars) {
    print(paste0("Generating ", var$name, " plots for site ", site, "..."))
    plot_site(guess, obs_dir, var$name, site, var$units, var$title, scale
      , baseline_dir = baseline_dir, width = width, height = height
      , show_spinup = show_spinup)
  }
}

print(paste0("Charts successfully generated in ", out_dir))
