#!/usr/bin/env Rscript

library("animation")
library("png")
library("grid")
library("magick")

# Set the directory containing your PNG files
basedir <- "~/code/lpj-guess/output-analysis"
png_dir <- file.path(basedir, "c4_fraction")

frames <- list.files(png_dir, full.names = TRUE)
out_file <- file.path(basedir, "c4_fraction.gif")

# print("Quantizing images...")
# width <- 512
# res <- sprintf("%dx%d", width, width)
# images <- image_read(frames) %>% image_scale(res) %>% image_quantize(max = 256)

# print("Building animation...")
# animation <- image_animate(images, fps = 2)
# image_write(animation, path = out_file)

start_time <- Sys.time()

# Create GIF animation from the PNG files
saveGIF({
  # Loop through each PNG file and read it
  for (i in 1:length(frames)) {  # Adjust the range as per your number of frames
    progress <- 1.0 * i / length(frames)
    percent <- round(100.0 * progress, 2)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    duration <- elapsed / progress
    remaining = round(duration - elapsed, 0)
    cat(paste0("Working: ", percent, "%, remaining = ", remaining, "s\r"))
    img_file <- frames[i]
    img <- png::readPNG(img_file)
    grid::grid.raster(img)
  }
}, movie.name = out_file, interval = 0.5, ani.width = 1024, ani.height = 1024)
