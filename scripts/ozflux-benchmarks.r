
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
  ozflux <- paste0(guess, '/benchmarks/ozflux')
  filename <- paste0(ozflux, '/', site, '/out/', var, '.out')
  cat(paste0("Reading '", filename, "'\n"))
  return(read_outfile(filename))
}

plot_timeseries <- function(data, xcol, ylab, ...) {
  xcol_index <- which(colnames(data) == xcol)

  colours <- hcl.colors(ncol(data) - 1)

  ymax <- -1e300
  ymin <- 1e300

  for (i in 1:ncol(data)) {
    if (i != xcol_index) {
      ymin <- min(ymin, min(data[!is.na(data[,i]),i]))
      ymax <- max(ymax, max(data[!is.na(data[,i]),i]))
    }
  }
  
  columns <- c()

  first_series = TRUE
  series_index <- 0
  for (i in 1:ncol(data)) {
    if (i != xcol_index) {
      if (first_series) {
        first_series = FALSE
        axes = TRUE
      } else {
        par(new = TRUE)
        axes = FALSE
      }
      series_index <- series_index + 1
      columns <- c(columns, colnames(data)[i])
      colour <- colours[series_index]
      plot(data[[xcol]], data[,i], type = "l", col = colour, ylab = ylab, xlab = xcol, axes = axes, xlim = NULL, ylim <- c(ymin, ymax), ...)
    }
  }
  legend("topleft", legend = columns, text.col = colours, lwd = par('lwd'), col = colours)
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

  #stats <- paste0("r2 = ", signif(r2, num_figs), ", rmse = ", signif(rmse, num_figs), " ", units, ", nse = ", signif(nse, num_figs), ", rsr = ", signif(rsr, num_figs))
  #mtext(stats, side = 1, line = -1, outer = TRUE, cex = par()$cex)
  legend("topright", legend = c(
    paste0("r2 = ", signif(r2, num_figs)),
    paste0("rmse = ", signif(rmse, num_figs)),
    paste0("nse = ", signif(nse, num_figs)),
    paste0("rsr = ", signif(rsr, num_figs))
  ))
}

plot_pvo <- function(observations, predictions, units, series_name) {
  ymin <- min(min(observations[!is.na(observations)]), min(predictions[!is.na(predictions)]))
  ymax <- max(max(observations[!is.na(observations)]), max(predictions[!is.na(predictions)]))
  limits <- c(ymin, ymax)

  xlab <- paste0("Observed (", units, ")")
  ylab <- paste0("Predicted (", units, ")")
  plot(observations, predictions, type = "p", xlab = xlab, ylab = ylab, xlim = limits, ylim = limits)

  # Plot a 1:1 line
  one_to_one <- c(ymin, ymax)
  lines(one_to_one, one_to_one, type = "l")
  legend("topleft", legend = c(series_name, "1:1 line"), lwd = c(NA, par('lwd')), pch = c(1, NA))
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

plot_site <- function(guess, obs_dir, var_name, site, units, title, show_spinup = FALSE, show_pfts = FALSE, width = 640, height = 480) {
  data <- read_site(guess, site, var_name)

  data <- aggregate_patches(data)
  
  colname_observed <- "Observed"
  colname_total <- "Total"
  colname_date <- "Date"
  
  obs_file <- paste0(obs_dir, '/', site, '.csv')
  if (!file.exists(obs_file)) {
    stop(paste0("Observed data file not found: '", obs_file, "'"))
  }

  observed <- read.csv(obs_file)[,c("year", "doy", var_name)]
  colnames(observed)[3] <- colname_observed
  data <- merge(data, observed, by.x = c("Year", "Day"), by.y = c("year", "doy"), sort = FALSE, all.x = show_spinup)
  col_year <- 1

  data$Date <- as.POSIXct(paste(data$Year, data$Day + 1, sep = "-"), format = "%Y-%j")
  
  # Drop first N cols:
  # Year,Day,Lon,Lat,patch
  ncol_drop <- 5
  data <- data[,-seq(1, ncol_drop)]

  if (!show_pfts) {
    cols_to_keep <- c(colname_date, colname_total, colname_observed)
    data <- data[,cols_to_keep]
    total_idx <- which(colnames(data) == colname_total)
    colname_total <- "Predicted"
    colnames(data)[total_idx] <- colname_total
  
    if (var_name == "resp") {
      data[,total_idx] <- -data[,total_idx]
    }
  }

  ylab <- paste0(title, " (", units, ")")
  site_title <- paste0(title, ' (', site, ')')

  # out_dir/site_var.png
  out_file <- paste0(out_dir, '/', site, "_", var, '.png')
  png(out_file, width = width, height = height)

  # Number of sig figs used when writing stats like RMSE/NSE
  num_figs <- 2

  # If showing spinup data in first plot, the data will contain NA values for
  # the observations during the spinup period. We need to remove these NA rows
  # before we show the predicted vs observed plot.
  if (show_spinup) {
    data <- data[!is.na(data[[colname_observed]]),]
  }
  
  par(mfcol = c(1, 2))
  plot_timeseries(data, colname_date, ylab)
  plot_pvo(data[[colname_observed]], data[[colname_total]], units, title)
  write_stats(data[[colname_observed]], data[[colname_total]], units, num_figs)
  mtext(site_title, line = -2, outer = TRUE, cex = 2 * par()$cex)
  
  dev.off()
  
  return(data)
}

# Path to LPJ-Guess repository.
guess <- '/path/to/dave-daily-grass-photosynthesis'

# Path to directory containing observed data.
obs_dir <- '/path/to/observed/data'

# Site names
sturt <- "SturtPlains_L6_20080828_20220218"
yanco <- "Yanco_L6_20130101_20220218"

# Desired output directory.
out_dir <- paste0(guess, '/benchmarks/ozflux/graphs')

# Variables to be plotted
gpp <- c()
gpp$name <- "gpp"
gpp$units <- "kgC/m2/day"
gpp$title <- "GPP"

resp <- c()
resp$name <- "resp"
resp$units <- "kgC/m2/day"
resp$title <- "Respiration"

sites <- c(sturt, yanco)
vars <- list()
vars[[1]] <- gpp
vars[[2]] <- resp

# Plot scaling. Increase to make everything bigger.
scale <- 2
par(cex = scale, lwd = scale)

width <- 1920
height <- 1080

for (site in sites) {
  for (var in vars) {
    plot_site(guess, obs_dir, var$name, site, var$units, var$title, width = width, height = height, show_spinup = T)
  }
}
