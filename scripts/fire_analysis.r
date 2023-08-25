readout <- function(site, out_dir, filename) {
  return(read.table(paste0(site, "/", out_dir, "/", filename), header = TRUE))
}

setwd("~/code/lpj-guess/scripts/portable/lpj-guess-4.2_linux_x86_64/examples")
sites <- c("Litchfield", "CumberlandPlain")
colours <- c("black", "red")
out_dir <- "out"
first <- TRUE
i <- 0
for (site in sites) {
  simfire <- readout(site, out_dir, "simfireanalysis_out.out")
  burned_area <- readout(site, out_dir, "aburned_area_out.out")
  par(new = !first)
  plot(simfire$Year, simfire$PopDens, type = "l", col = colours[i + 1], xaxt = "n", yaxt = "n", xlab = "Year", ylab = "Population Density")
  if (!first) {
    axis(2, tick = TRUE)
  }
  par(new = TRUE)
  plot(burned_area$Year, burned_area$BurntFr, col = colours[i + 1], ylim = c(0, 1), xaxt = "n", yaxt = "n", xlab = "Year", ylab = "")
  first <- FALSE
  i <- i + 1
}
par(xpd = NA)
legend("topleft", legend = c(sites, "Population Density", "Burnt Fraction"), col = c(colours, "black", "black"), text.col = c(colours, "black", "black"), lwd = c(1, 1, 1, NA), pch = c(21, 21, NA, 21), inset = c(-0.1, -0.2), horiz = TRUE)
#legend("topleft", legend = c("Population Density", "Burnt Fraction"), lwd = c(1, NA), pch = c(NA, 21), horiz = TRUE)
axis(4, tick = TRUE)
axis(1, tick = TRUE)