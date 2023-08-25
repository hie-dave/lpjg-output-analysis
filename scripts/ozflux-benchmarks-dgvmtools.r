#!/usr/bin/env Rscript

load <- function(lib) {
  if (!require(lib, character.only = TRUE)) stop(paste(lib, "is not installed"))
}

load("DGVMTools")
load("dplyr")
load("tidyr")
load("purrr")
load("ggpubr")
load("stringr")
load("htmltools")
load("knitr")
load("knitrProgressBar")

get_file_name <- function () { # https://stackoverflow.com/a/1816487
    frame_files <- lapply(sys.frames(), function(x) x$ofile)
    frame_files <- Filter(Negate(is.null), frame_files)
    frame_files[[length(frame_files)]]
}
ofile <- get_file_name()
cat(paste0("ofile = '", ofile, "'\n"))
setwd(dirname(ofile))

source("ozflux-inputs.r")
source("ozflux-stats.r")
source("ozflux-processing.r")
source("ozflux-plotting.r")

# Iff true, plots will be rendered using plotly. Note that plotly plots are only
# interactive when rendering to HTML; therefore, we only turn on plotly when
# rendering to HTML. This can of course be overriden (e.g. for debugging).
use_plotly <- opts_knit$get("rmarkdown.pandoc.to") == "html"

# Iff true, plots will be rendered in 2x2 panels for each variable for each site
combined_graph <- TRUE

# Iff true, a title will be written above every graph when combined_graph is
# FALSE.
max_title <- 4

# Scalar for all text on all plots. Increase this for larger text.
text_multiplier <- 1.5

if (use_plotly) {
  load("plotly")
}

# source("plotSubannualDave.r")
# source("plotScatterDave.r")

r2 <- data.frame(Site = gridcells$Name)
rmse <- data.frame(Site = gridcells$Name)
nse <- data.frame(Site = gridcells$Name)
rsr <- data.frame(Site = gridcells$Name)
bias <- data.frame(Site = gridcells$Name)

timeseries_plots <- list()
pvo_plots <- list()
subannual_plots <- list()
combined_plots <- list()

p <- progress_estimated(nrow(gridcells) * length(vars))
update_progress(p)

tags <- list()

write_title <- function(title, level, force_print = FALSE) {
  if (level == 1 || level <= max_title) {
    if (use_plotly && !force_print) {
      fname <- paste0("h", level)
      tag <- do.call(fname, list(title))
      tags[[length(tags) + 1L]] <<- tag
    } else {
      hashes <- paste0(rep("#", level), collapse = "")
      cat(paste0("\n\n", hashes, " ", title, "\n\n"))
    }
  }
}

write_plot <- function(plt) {
  if (use_plotly) {
    tags[[length(tags) + 1L]] <<- plt
    # print(htmltools::tagList(plt))
    # htmlwidgets::saveWidget(plt_out, html_out, selfcontained = FALSE, libdir = "lib")
  } else {
    print(plt)
  }
}

################################################################################
# Main Program
################################################################################

for (var in vars) {
  lyr_name <- gsub("dave_", "", var@id)

  r2[[lyr_name]] <- rep(NA, nrow(gridcells))
  rmse[[lyr_name]] <- rep(NA, nrow(gridcells))
  nse[[lyr_name]] <- rep(NA, nrow(gridcells))
  rsr[[lyr_name]] <- rep(NA, nrow(gridcells))
  bias[[lyr_name]] <- rep(NA, nrow(gridcells))

  # Read all observations for this variable.
  data <- read_data(var, versions)

  # Now plot this variable for each site.
  for (i in seq_len(nrow(gridcells))) {
    row <- gridcells[i, ]
    title <- paste(row$Name, var@name)

    # Filter data to this site.
    gc <- get_gridcell(data, row$Lat, row$Lon, row$Name)

    if (nrow(gc@data) == 0) {
      warning("No data for site ", row$Name, "; skipping...")
      if (combined_graph) {
        combined_plots[[length(combined_plots) + 1L]] <- NA
      } else {
        timeseries_plots[[length(timeseries_plots) + 1L]] <- NA
        pvo_plots[[length(pvo_plots) + 1L]] <- NA
        subannual_plots[[length(subannual_plots) + 1L]] <- NA
      }
      p$tick()
      next
    }

    res <- create_plots(gc, var@name, use_plotly = use_plotly
      , text_multiplier = text_multiplier, ncol = 2, marker_size = marker_size
      , do_timeseries = TRUE, do_pvo = TRUE, do_subannual = TRUE)

    r2[i, ncol(r2)] <- res$r2
    rmse[i, ncol(rmse)] <- res$rmse
    nse[i, ncol(nse)] <- res$nse
    rsr[i, ncol(rsr)] <- res$rsr
    bias[i, ncol(bias)] <- res$bias

    # Save plots (we're looping over variables first, because it's faster to
    # read/process the data this way, but we want to group plots by site, rather
    # than by variable).
    if (combined_graph) {
      plt <- create_panel(res$timeseries, res$pvo, res$subannual, use_plotly)
      combined_plots[[length(combined_plots) + 1L]] <- plt
    } else {
      timeseries_plots[[length(timeseries_plots) + 1L]] <- res$timeseries
      pvo_plots[[length(pvo_plots) + 1L]] <- res$pvo
      subannual_plots[[length(subannual_plots) + 1L]] <- res$subannual
    }

    p$tick()
    update_progress(p)
  }
}

# Write tables.
write_title("r2", 1, force_print = TRUE)
print(kable(r2))
write_title("rmse", 1, force_print = TRUE)
print(kable(rmse))
write_title("nse", 1, force_print = TRUE)
print(kable(nse))
write_title("rsr", 1, force_print = TRUE)
print(kable(rsr))
write_title("bias", 1, force_print = TRUE)
print(kable(bias))

# Write graphs by site.
for (i in seq_len(nrow(gridcells))) {
  gridcell <- gridcells[i, ]
  site <- gridcell$Name

  write_title(site, 1)

  for (j in seq_len(length(vars))) {
    var <- vars[[j]]

    index <- (i - 1) * length(vars) + j

    if (combined_graph && !is.list(combined_plots[[index]]))
      next

    if (!combined_graph &&
      (!is.list(timeseries_plots[[index]])
       || !is.list(pvo_plots[[index]])
       || !is.list(subannual_plots[[index]])))
      next

    write_title(paste(site, var@name), 2)

    if (combined_graph) {
      plt <- combined_plots[[index]]
      write_plot(plt)
    } else {
      timeseries_title <- paste0(site, " ", var@name, " Timeseries")
      pvo_title <- paste0(site, " ", var@name, " Predicted vs Observed")
      subannual_title <- paste0(site, " ", var@name, " Mean Subannual Cycle")

      write_title(timeseries_title, 3)
      write_plot(timeseries_plots[[index]])
      write_title(pvo_title, 3)
      write_plot(pvo_plots[[index]])
      write_title(subannual_title, 3)
      write_plot(subannual_plots[[index]])
    }
  }
}
