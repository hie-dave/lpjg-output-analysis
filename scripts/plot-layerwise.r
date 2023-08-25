library("tools")

################################################################################
# User inputs - modify as required.
################################################################################

# Path to lpj-guess repository.
repo <- "../dave-daily-grass-photosynthesis"

# Output directory.
out_dir <- "profile-graphs"

# Graph width (px).
width <- 1920

# Graph height (px).
height <- 1080

# Plot scaling. Increase this to make everything bigger.
scale <- 2

################################################################################
# Global variables - think twice before modifying.
################################################################################

# Number of LPJ-Guess soil layers.
nlayer <- 15

# Name of the date column added to the data frame.
colname_date <- "Date"

get_out_dir <- function(site) {
  return(paste0(repo, "/benchmarks/ozflux/", site, "/out"))
}

parse_date <- function(year, day) {
  return(as.POSIXct(paste(year, day + 1, sep = "-"), format = "%Y-%j"))
}

read_file <- function(site, filename) {
  data <- read.table(paste0(get_out_dir(site), "/", filename), header = TRUE)
  data[[colname_date]] <- parse_date(data$Year, data$Day)
  return(data)
}

get_layer_name <- function(var, i) {
  if (i >= nlayer || i < 0) {
    stop(paste0("layer must be in range [0, ", nlayer, "]"))
  }
  return(paste(var, i, sep = "_"))
}

get_output_layer_name <- function(var, i) {
  return(paste0(var$display_name, " (", i, ") (", var$units, ")"))
}

get_ymin <- function(data, var) {
  ymin <- 1e300
  for (i in 0:(nlayer - 1)) {
    col <- get_layer_name(var, i)
    ymin <- min(ymin, data[, col])
  }
  return(ymin)
}

get_ymax <- function(data, var) {
  ymax <- -1e300
  for (i in 0:(nlayer - 1)) {
    col <- get_layer_name(var, i)
    ymax <- max(ymax, data[, col])
  }
  return(ymax)
}

define_input_file <- function(filename, name, display_name, units) {
  file <- c()
  file$filename <- filename
  file$name <- name
  file$display_name <- display_name
  file$units <- units
  return(file)
}

plot_layerwise <- function(site, input_file, start_year = NULL) {
  data <- read_file(site, input_file$filename)

  # Filter to specified date range.
  if (!is.null(start_year)) {
    data <- data[data$Year >= start_year, ]
  }

  ymin <- get_ymin(data, input_file$name)
  ymax <- get_ymax(data, input_file$name)

  # Create output directory if it doesn't already exist.
  site_dir <- paste0(out_dir, "/", site)
  if (!dir.exists(site_dir)) {
    dir.create(site_dir, recursive = TRUE)
  }

  out_name <- file_path_sans_ext(input_file$filename)
  out_file <- paste0(site_dir, "/", out_name, ".png")
  # png(out_file, width = width, height = height)
  par(cex = scale, lwd = scale)

  par(mfrow = c(3, 5))
  for (i in 0:(nlayer - 1)) {
    x <- data[[colname_date]]
    name <- get_output_layer_name(input_file, i)
    y <- data[, get_layer_name(input_file$name, i)]
    plot(x, y, type = "l", ylab = name, xlab = colname_date, main = name
      , ylim = c(ymin, ymax))
  }
  # dev.off()
}

files <- list(
  define_input_file("dave_sw.out", "sw", "FAW", "0-1"),
  define_input_file("dave_swmm.out", "swmm", "Soil Water Content", "mm"),
  define_input_file("dave_swvol.out", "swvol", "Volumetric Soil Water Content"
    , "mm/mm")
)

sites <- c("Otway", "Samford", "Yanco", "SturtPlains")

for (site in sites) {
  for (file in files) {
    plot_layerwise(site, file)
  }
}

cat("Done\n")
