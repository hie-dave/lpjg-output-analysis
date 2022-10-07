library('animation')
library('raster')

data_directory = '.data'

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

create_animation <- function(data, out_file, column, title, ymax = -1,
  width = 480, height = 480, frame) {
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
  ylim <- c(min(data[[column]]), ymax)
  title <- paste0(title, ':')

  # Generate the animation.
  saveGIF({
    for (year in unique(data$Year)) {
      # Create raster for this year's data.
      rs <- output_data_to_raster(data, year, column)

      # Calling plot() will add the frame to the animation.
      plot(rs, main = paste(title, year), zlim = ylim, interpolate = T)
    }
  # These options could be user-configurable too.
  }, interval = 0.2, movie.name = out_file, ani.width = width,
  ani.height = height)
}

create_animation_from_file <- function(in_file, out_file, column, title) {
  data <- read_lpj_guess_output_file(in_file)
  create_animation(data, out_file, column, title)
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

