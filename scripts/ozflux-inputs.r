
################################################################################
# User Inputs - modify as required
################################################################################

# Path to directory containing observed data.
obs_dir <- "obs"

# Desired output directory.
out_dir <- "graphs"

define_version <- function(name, path) {
	return(defineSource(id = name, format = GUESS, dir = path))
}

# LPJ-Guess versions for which outputs will be plotted.
versions <- list(
    define_version("dave", "out/dave-daily-grass-photosynthesis")
  , define_version("dailygrass", "out/dave-daily-grass")
  , define_version("trunk", "out/dave-baseline")
)

# Variables to be plotted
vars <- list(
    defineQuantity("dave_gpp", "GPP", "gC m^-2 day^-1")
  , defineQuantity("dave_resp", "Respiration", "gC m^-2 day^-1")
  , defineQuantity("dave_nee", "NEE", "gC m^-2 day^-1")
  , defineQuantity("dave_et", "ET", "mm day^-1")
  , defineQuantity("dave_lai", "LAI", "m^2 m^-2")
  # , defineQuantity("cmass", "AboveGround Biomass", "kgC/m2")
)

# Name of the observed data netcdf file.
obs_file <- "ozflux-obs.nc"

observations <- defineSource("obs", "Ozflux", format = NetCDF, dir = obs_dir)

# The index of the version for which stats should be computed/displayed. Note
# that array indices in R start at 1. 🤦
stats_version_index <- 1

# Ozflux gridcells. This is a data frame with 3 columns (Lon, Lat, Name).
gridcells <- read.csv("ozflux.grid", sep = " ", header = FALSE)
names(gridcells) <- c("Lon", "Lat", "Name")

# Marker size. Decrease for smaller markers in the predicted vs observed plots.
marker_size <- 1
