library('animation')
library('raster')

data_directory = '.data'

#' Read an LPJ-Guess output file and return file data as a dataframe.
#' @param filename: Path to the output file, may be relative or absolute.
read_lpj_guess_output_file <- function(filename) {
  return(read.table(filename, header = T))
}

output_data_to_raster <- function(data, year, column) {
  # Filter data to the specified year and PFT.
  filtered <- data[data$Year == year,c('Lon', 'Lat', column)]

  # Generate a raster for this data.
  return(rasterFromXYZ(filtered))
}

output_file_to_raster <- function(filename, year, column) {
  # Read data from disk.
  data <- read_lpj_guess_output_file(filename)
  return(output_data_to_raster(data))
}

geoplot_out <- function(filename, year, column) {
  rs <- output_file_to_raster(filename, year, column)
  plot(rs)
}

get_full_filename <- function(filename) {
  return(paste0(data_directory, '/', filename))
}

#' @param rescale: Set to true to make y axis symmetrical around 0.
create_animation <- function(data, out_file, column, title, ymin = -1,
  ymax = -1, width = 480, height = 480, colours = NULL, rescale = F) {
  # Setting up a few plot-related settings.
  if (ymax == -1) {
    # Limit y axis (z-axis, really) to whichever is smaller:
    # - maximum y value
    # - mean + 3 * sd of y data
    # This should hopefully prevent a single very high data point
    # from causing unfortunate axis limits which in turn limits the
    # rasters to the "low" range of colours for 99% of the data.
    ymax <- min(max(data[[column]]), 
                mean(data[[column]]) + 3 * sd(data[[column]]))
  }
  if (ymin == -1) {
    # See above comment RE ymax for details.
    ymin <- max(min(data[[column]]),
                mean(data[[column]]) - 3 * sd(data[[column]]))
  }

  if (rescale && ymin < 0 && ymax > 0) {
    ymax <- max(abs(ymin), ymax)
    ymin <- -ymax
  }
  if (is.null(colours)) {
    colours = rev(terrain.colors(255))
  }

  ylim <- c(ymin, ymax)
  title <- paste0(title, ':')

  # Generate the animation.
  saveGIF({
    for (year in unique(data$Year)) {
      # Create raster for this year's data.
      rs <- output_data_to_raster(data, year, column)

      # Calling plot() will add the frame to the animation.
      plot(rs, main = paste(title, year), zlim = ylim, interpolate = F, col = colours)
    }
  # These options could be user-configurable too.
  }, interval = 0.2, movie.name = out_file, ani.width = width,
  ani.height = height)
}

create_animation_from_file <- function(in_file, out_file, column, title) {
  data <- read_lpj_guess_output_file(in_file)
  create_animation(data, out_file, column, title)
}

# Animation settings for the delta graphs.
anim <- function(data, column, filename, title, width, height) {
  create_animation(data, filename, column, title,
                   colours = hcl.colors(255, 'red-green'), rescale = T,
                   width = width, height = height)
}

animate_delta <- function(data0, data1, filename, title, width, height, column) {
  data_columns <- c("BNE", "BINE", "BNS", "TeNE", "TeBS", "IBS", "TeBE", "TrBE",
                    "TrIBE", "TrBR", "C3G", "C4G", "Total")
  deltas <- data.frame(data0)
  for (col in data_columns) {
    deltas[[column]] <- data0[[column]] - data1[[column]]
  }
  anim(deltas, column, filename, title, width, height)
}

#' Create animations for deltas between the different australian runs.
#' This will take a few minutes. Reducing width and height will make it faster.
#' @param width: Output animation width (px)
#' @param height: Output animation height(px)
create_delta_animations <- function(width = 1280, height = 1280) {
  
  base_file <- get_full_filename('australia.out')
  hico2_file <- get_full_filename('australia_elevated_co2.out')
  hico2_hitemp_file <- get_full_filename('australia_hot.out')

  base_data <- read_lpj_guess_output_file(base_file)
  hico2_data <- read_lpj_guess_output_file(hico2_file)
  hico2_hitemp_data <- read_lpj_guess_output_file(hico2_hitemp_file)

  # Generate animations of raw data.
  create_animation(base_data, 'au-baseline.gif', 'Total', 'LAI', width = width,
                   height = height)
  create_animation(hico2_data, 'au-highco2.gif', 'Total', 'LAI at +150ppm CO2',
                   width = width, height = height)
  create_animation(hico2_hitemp_data, 'au-highco2-hightemp', 'Total',
                   'LAI at +2°C, +150ppm CO2', width = width, height = height)

  # Generate animations of LAI deltas.
  animate_delta(hico2_data, base_data, 'highco2_vs_baseline_delta.gif',
       'LAI 𝚫 at +150ppm CO2', width, height)

  animate_delta(hico2_hitemp_data, base_data, 'highco2_hightemp_vs_baseline_delta.gif',
       'LAI 𝚫 at +2°C, +150ppm CO2', width, height)

  animate_delta(hico2_hitemp_data, hico2_data, 'highco2_hightemp_vs_highco2_delta.gif',
       'LAI 𝚫 at +2°C, +150ppm CO2 vs +150ppm CO2', width, height)
}

au_file <- get_full_filename('australia.out')
au_elev_co2 <- get_full_filename('australia_elevated_co2.out')
au_elev_temp_co2 <- get_full_filename('australia_hot.out')
historical_files <- c(
  get_full_filename('historical_CSIRO-BOM-ACCESS1-0_UNSW-WRF360J.out'),
  get_full_filename('historical_CSIRO-BOM-ACCESS1-0_UNSW-WRF360K.out'),
  get_full_filename('historical_CSIRO-BOM-ACCESS1-3_UNSW-WRF360J.out'),
  get_full_filename('historical_CSIRO-BOM-ACCESS1-3_UNSW-WRF360K.out')
)

data_columns <- c("BNE", "BINE", "BNS", "TeNE", "TeBS", "IBS", "TeBE", "TrBE",
                  "TrIBE", "TrBR", "C3G", "C4G", "Total")

