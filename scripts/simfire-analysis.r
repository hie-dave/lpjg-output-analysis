source("scripts/stacked_area.r")
define_outdir <- function(dirname, description) {
  out <- c()
  out$name <- dirname
  out$desc <- description
  return(out)
}

################################################################################
# User Inputs
################################################################################

# Sites for which plots will be generated.
sites <- c("CumberlandPlain", "Litchfield")

# Guess repository path.
guess_dir <- "~/code/lpj-guess/dave-baseline"

# Output directories in which model outputs are stored, along with an optional
# description which will be rendered as a plot subtitle.
model_outputs <- list(
  define_outdir("out-baseline", "standard parameters"),
  define_outdir("out-high-k", "temperate k_tun_litter = 0.75"),
  define_outdir("out-no-popdens", "no population density effect"),
  define_outdir("out-no-popdens-high-k", "temperate k_tun_litter = 0.75 and no population density effect")
)

# Directory into which graphs will be saved.
out_dir <- "fire-plots"

# Width of generated graphs in px.
width <- 1920

# Height of generated graphs in px.
height <- 1080

# Plot scaling. Increase this to make everything bigger.
scale <- 2

################################################################################
# Global Variables
################################################################################

min_fuel <- 200

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

################################################################################
# Code
################################################################################

read_guess <- function(filename) {
  table <- read.table(filename, header = TRUE)
  if ("Day" %in% colnames(table)) {
    table$Date <- as.POSIXct(paste(table$Year, table$Day + 1, sep = "-")
      , format = "%Y-%j")
  } else {
    table$Date <- as.POSIXct(paste(table$Year, 365, sep = "-")
      , format = "%Y-%j")
  }
  return(table)
}

read_site <- function(site, out_dir, filename) {
  path <- paste0(guess_dir, "/benchmarks/ozflux/", site, "/", out_dir, "/"
    , filename)
  return(read_guess(path))
}

plot_fire <- function(simf, subtitle = NULL) {
  mar <- par()$mar

  pfire <- simf$PBiome * simf$PMxNest * simf$PFpar * simf$PPopDens
  par(mar = (c(5, 4, 4, 8) + 0.1))
  ylim <- c(0, 1)
  n <- 7 # Number of lines on graph
  plot(simf$Date, simf$PBiome, type = "l", col = cb_colours[1], ylim = ylim
    , ylab = NA, xlab = "Date")
  par(new = TRUE)
  plot(simf$Date, simf$PFpar, type = "l", col = cb_colours[2], ylim = ylim
    , ylab = NA, xlab = NA)
  par(new = TRUE)
  plot(simf$Date, simf$PMxNest, type = "l", col = cb_colours[3], ylim = ylim
    , ylab = NA, xlab = NA)
  par(new = TRUE)
  plot(simf$Date, simf$PPopDens, type = "l", col = cb_colours[4], ylim = ylim
    , ylab = NA, xlab = NA)
  par(new = TRUE)
  plot(simf$Date, pfire, type = "l", col = cb_colours[5], ylim = ylim
    , ylab = NA, xlab = NA)
  par(new = TRUE)
  plot(simf$Date, simf$ABurntArea, type = "l", col = cb_colours[6], ylim = ylim
    , ylab = NA, xlab = NA)

  names <- c("PBiome", "PFpar", "PNesterov", "PPopDens", "FireProb", "BurntArea")
  legend("topright", legend = names, text.col = cb_colours[1:n], lwd = par()$lwd
    , col = cb_colours[1:n], inset = c(-0.3, 0), xpd = TRUE)
  heading <- paste0("Fire Probability Factors (", site, ")")
  if (simf$Year[1] == simf$Year[length(simf$Year)]) {
    title(paste0(heading, " (", simf$Year[1], ")"))
  } else {
    title(heading)
  }
  if (!is.null(subtitle)) {
    mtext(subtitle, side = 3, line = 1, cex = scale)
  }
  par(mar = mar)
}
plot_year <- function(year) {
  plot_fire(dsimf[dsimf$Year == year,])
}

plot_blaze <- function(simf, alai, heading) {
  mar <- par()$mar
  mar_new <- mar
  mar_new[4] <- mar_new[2]
  par(mar = mar_new)
  colours <- c(cb_colours[4], cb_colours[6], "red", cb_colours[2])
  ylim <- c(min(simf$avail_fuel), max(min_fuel, max(simf$avail_fuel)))
  scalar <- ylim[2] / if (max(simf$ABurntArea) == 0) 1 else max(simf$ABurntArea)
  plot(simf$Date, simf$avail_fuel, type = "l", col = colours[1]
    , xlab = "Date", ylab = NA, ylim = ylim, las = 1)
  lines(simf$Date, simf$ABurntArea * scalar, col = colours[2], xlab = NA
    , ylab = NA)
  lines(simf$Date, simf$avail_fuel, col = colours[1], xlab = NA
    , ylab = NA)
  lines(simf$Date, rep(min_fuel, length(simf$Date)), col = colours[3], xlab = NA
    , ylab = NA)
  names <- c("Available Fuel", "Burnt Area", "Min Fuel", "LAI")
  legend("topleft", legend = names, text.col = colours, lwd = par()$lwd
    , col = colours)
  title(heading)

  par(new = TRUE)
  alai_scalar <- ylim[2] / max(alai$total)
  plot(alai$Date, alai$total * alai_scalar, xlab = NA, ylab = NA, xaxt = "n"
    , yaxt = "n", type = "l", ylim = ylim, col = colours[4])

  #if (min(simf$ABurntArea) != max(simf$ABurntArea)) {
  #  labels <- pretty(range(simf$ABurntArea))
  #  at <- seq(ylim[1], ylim[2], length.out = length(labels))
  #  axis(side = 4, at = at, labels = labels, col = colours[2]
  #    , col.axis = colours[2])
  #mtext("Burnt Area", side = 4, line = 3, col = colours[2])
  #} else {
    labels <- pretty(range(alai$total))
    at <- seq(ylim[1], ylim[2], length.out = length(labels))
    axis(side = 4, at = at, labels = labels, las = 1)
    mtext("LAI", side = 4, line = 3)
  #}
  # axis(side = 2, col = colours[1], col.axis = colours[1])
  mtext("Available Fuel (gC/m2)", side = 2, line = 3)
  par(mar = mar)
}

get_out_dir <- function(site) {
  return(dir)
}

new_plot <- function(site, filename) {
  dir <- paste0(out_dir, "/", site)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  # Finish previous plot.
  if (dev.cur() != 1) {
    dev.off()
  }
  png(paste0(dir, "/", filename), width = width, height = height)

  ncol <- 2
  nrow <- ceiling(length(model_outputs) / ncol)
  mfrow <- c(nrow, ncol)
  par(mfrow = mfrow, cex = scale, lwd = scale)
}

subtitle <- function(text) {
  mtext(text, line = 1)
}

for (site in sites) {
  # Simfire plot.
  new_plot(site, "simfire.png")
  for (out in model_outputs) {
    asimf <- read_site(site, out$name, "dave_asimfire.out")
    plot_fire(asimf)
    subtitle(out$desc)
  }

  # Blaze plot.
  new_plot(site, "blaze.png")
  for (out in model_outputs) {
    asimf <- read_site(site, out$name, "dave_asimfire.out")
    alai <- read_site(site, out$name, "dave_alai.out")
    plot_blaze(asimf, alai, paste0("Blaze Fire/Fuel (", site, ")"))
    subtitle(out$desc)
  }
}

fuel_outputs <- list()
fuel_ymax <- -1e100
i <- 1
for (site in sites) {
  for (out in model_outputs) {
    fuel <- read_site(site, out$name, "dave_afuel.out")
    fuel_outputs[[i]] <- fuel
    m <- fuel$FineLitter + fuel$FineWood + fuel$CoarseWood + fuel$GrassCMass
    fuel_ymax <- max(fuel_ymax, m)
    i <- i + 1
  }
}

for (site_index in seq_along(sites)) {
  site <- sites[site_index]

  new_plot(site, "fuel.png")
  for (out_index in seq_along(model_outputs)) {
    out <- model_outputs[[out_index]]
    i <- site_index - 1 # what a silly language
    j <- out_index - 1 # what a silly language

    table_index <- j + i * length(model_outputs)
    table_index <- table_index + 1 # 😠
    fuel <- fuel_outputs[[table_index]]
    names <- c("FineLitter", "FineWood", "CoarseWood", "GrassCMass")
    stacked_area(fuel$Date, fuel[, names], "Date", "Fuel (gC/m2)", fuel_ymax)
    subtitle(out$desc)

    # Add min_fuel line.
    par(new = TRUE)
    ylim <- c(0, fuel_ymax)
    plot(fuel$Date, rep(min_fuel, length.out = length(fuel$Date)), type = "l"
    , col = "red", xlab = NA, ylab = NA, xaxt = "n", yaxt = "n", ylim = ylim)
    title(paste0(site, " Available Fuel"))
  }
}
rm(fuel_outputs)

debris_outputs <- list()
debris_ymax <- -1e100
i <- 1
for (site in sites) {
  for (out in model_outputs) {
    debris <- read_site(site, out$name, "dave_acoarse_woody_debris.out")
    debris_outputs[[i]] <- debris
    debris_ymax <- max(debris_ymax, debris$LitterHeart + debris$som)
    i <- i + 1
  }
}

for (site_index in seq_along(sites)) {
  site <- sites[site_index]

  new_plot(site, "debris.png")
  for (out_index in seq_along(model_outputs)) {
    out <- model_outputs[[out_index]]
    i <- site_index - 1 # what a silly language
    j <- out_index - 1 # what a silly language

    table_index <- j + i * length(model_outputs)
    table_index <- table_index + 1 # 😠

    debris <- debris_outputs[[table_index]]
    stacked_area(debris$Date
    , debris[, c("LitterHeart", "som")], "Date", "Litter (gC/m2)"
    , ymax = debris_ymax)
    title(paste0("Coarse Woody Debris Composition (", site, ")"))
    subtitle(out$desc)
  }
}
rm(debris_outputs)
cat("Done.")
