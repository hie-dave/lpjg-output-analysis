#'
#' Get the quantities plotted in the ozflux benchmarks.
#'
#' @return Returns a list of \seealso{\link{DGVMTools::Quantity}}
#' objects.
#' @author Drew Holzworth
#' @keywords internal
#'
get_vars <- function() {
	return(list(
	  DGVMTools::defineQuantity("dave_gpp", "GPP", "gC m^-2 day^-1")
	# , DGVMTools::defineQuantity("dave_resp", "Respiration", "gC m^-2 day^-1")
	# , DGVMTools::defineQuantity("dave_nee", "NEE", "gC m^-2 day^-1")
	# , DGVMTools::defineQuantity("dave_transpiration", "ET", "mm day^-1")
	# , DGVMTools::defineQuantity("dave_lai", "LAI", "m^2 m^-2")
	# , defineQuantity("cmass", "AboveGround Biomass", "kgC/m2")
	))
}

#'
#' Check if a package is installed.
#'
#' @param pkg: Name of the package.
#'
#' @return Returns TRUE if the package is installed, otherwise FALSE.
#' @keywords internal
#' @authur Drew Holzworth
#'
is_installed <- function(pkg) {
	return(pkg %in% rownames(installed.packages()))
}

#'
#' Generate ozflux benchmarks.
#'
#' @param use_plotly: Iff true, plots will be rendered using plotly. Note that
#' plotly plots are only interactive when rendering to HTML; therefore, we only
#' turn on plotly when rendering to HTML.This can of course be overriden (e.g.
#' for debugging).
#' @param combined_graph: Iff true, plots will be rendered in 2x2 panels for
#' each variable for each site.
#' @param max_title: Maximum number of nested titles.
#' @param marker_size: Marker size. Decrease for smaller markers.
#'
#' @return Returns a list of HTML tags if use_plotly is TRUE. Otherwise all
#' content will be printed, for use in R markdown.
#' @author Drew Holzworth
#' @export
#'
ozflux_benchmarks <- function(
	sources,
	use_plotly = FALSE,
	text_multiplier = 1.5,
	combined_graph = TRUE,
	max_title = 4,
	marker_size = 1
) {
	# Sanitise input sources.
	log_debug("Sanitising input sources...")
	sources <- sanitise_sources(sources)

	if (length(sources) < 1) {
		return()
	}

	# Variables to be plotted.
	log_debug("Getting default variables to plot...")
	vars <- get_vars()

	log_debug("Getting sites to plot...")
	sites <- read_ozflux_sites()

	write_progress <- is_installed("knitrProgressBar")
	log_debug("Write_progress = ", write_progress)
	if (write_progress) {
		log_debug("Initialising progress bar...")
		p <- knitrProgressBar::progress_estimated(nrow(sites) * length(vars))
		knitrProgressBar::update_progress(p)
	}

	log_debug("Initialising stats dataframes...")
	r2 <- data.frame(Site = sites$Name)
	rmse <- data.frame(Site = sites$Name)
	nse <- data.frame(Site = sites$Name)
	rsr <- data.frame(Site = sites$Name)
	bias <- data.frame(Site = sites$Name)

	timeseries_plots <- list()
	pvo_plots <- list()
	subannual_plots <- list()
	combined_plots <- list()

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

	write_paragraph <- function(text) {
		if (use_plotly) {
			# html output
			tag <- htmltools::p(text)
			tags[[length(tags) + 1L]] <- tag
		} else {
			cat(text, "\n\n")
		}
	}

	write_plot <- function(plt) {
		if (use_plotly) {
			tags[[length(tags) + 1L]] <<- plt
		} else {
			print(plt)
		}
	}

	for (var in vars) {
		lyr_name <- gsub("dave_", "", var@id)

		log_diag("Processing variable ", var@name, "...")

		r2[[lyr_name]] <- rep(NA, nrow(sites))
		rmse[[lyr_name]] <- rep(NA, nrow(sites))
		nse[[lyr_name]] <- rep(NA, nrow(sites))
		rsr[[lyr_name]] <- rep(NA, nrow(sites))
		bias[[lyr_name]] <- rep(NA, nrow(sites))

		data <- read_data(list(var), sources)
		for (i in seq_len(nrow(sites))) {
			row <- sites[i, ]
			log_debug("[", var@name, "] Processing site ", row$Name, "...")

			# Filter data to this site.
			gc <- get_gridcell(data, row$Lat, row$Lon, row$Name)

			if (nrow(gc@data) == 0 || !(get_global("obs_lyr") %in% names(gc@data))) {
				warning("No ", var@name, " data for site ", row$Name, "; skipping...")
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
			, text_multiplier = text_multiplier, ncol = 2
			, marker_size = marker_size, do_timeseries = TRUE, do_pvo = TRUE
			, do_subannual = TRUE)

			r2[i, ncol(r2)] <- res$r2
			rmse[i, ncol(rmse)] <- res$rmse
			nse[i, ncol(nse)] <- res$nse
			rsr[i, ncol(rsr)] <- res$rsr
			bias[i, ncol(bias)] <- res$bias

			# Save plots (we're looping over variables first, because it's faster to
			# read/process the data this way, but we want to group plots by site, rather
			# than by variable).
			if (combined_graph) {
				ylab <- get_y_label(var, NULL)
				plt <- create_panel(res$timeseries, res$pvo, res$subannual
					, use_plotly = use_plotly, ylab = ylab)
				combined_plots[[length(combined_plots) + 1L]] <- plt
			} else {
				timeseries_plots[[length(timeseries_plots) + 1L]] <- res$timeseries
				pvo_plots[[length(pvo_plots) + 1L]] <- res$pvo
				subannual_plots[[length(subannual_plots) + 1L]] <- res$subannual
			}

			if (write_progress) {
				p$tick()
				knitrProgressBar::update_progress(p)
			}
		}
	}

	write_table <- function(tbl) {
		print(knitr::kable(tbl, digits = 2))
	}

	r2_desc <- "The coefficient of determination (r<sup>2</sup>) is the proportion of the variation in the dependent variable that is predictable from the independent variable(s). This is unitless and ranges from 0-1."
	nse_desc <- "The Nash–Sutcliffe efficiency is calculated as one minus the ratio of the error variance of the modeled time-series divided by the variance of the observed time-series. In the situation of a perfect model with an estimation error variance equal to zero, the resulting Nash–Sutcliffe Efficiency equals 1 (NSE = 1). Conversely, a model that produces an estimation error variance equal to the variance of the observed time series results in a Nash–Sutcliffe efficiency of 0.0 (NSE = 0). This is unitless."
	rmse_desc <- "The root mean square error (RMSE) of a sample is the quadratic mean of the differences between the observed values and predicted ones. This is in the units of the variable."
	rsr_desc <- "The root mean square error to standard deviation ratio is the RMSE normalised by the standard deviation of the observations. This is unitless"
	bias_desc <- "The bias is the mean difference between the predictions and observations. This is in the units of the variable."

	# Write tables.
	write_title("r<sup>2</sup>", 1, force_print = TRUE)
	write_paragraph(r2_desc)
	write_table(r2)

	write_title("rmse", 1, force_print = TRUE)
	write_paragraph(rmse_desc)
	write_table(rmse)

	write_title("nse", 1, force_print = TRUE)
	write_paragraph(nse_desc)
	write_table(nse)

	write_title("rsr", 1, force_print = TRUE)
	write_paragraph(rsr_desc)
	write_table(rsr)

	write_title("bias", 1, force_print = TRUE)
	write_paragraph(bias_desc)
	write_table(bias)

	# Write graphs by site.
	for (i in seq_len(nrow(sites))) {
		gridcell <- sites[i, ]
		site <- gridcell$Name

		write_title(site, 1)

		for (j in seq_len(length(vars))) {
			var <- vars[[j]]

			index <- (j - 1) * nrow(sites) + i

			if (combined_graph && !is.list(combined_plots[[index]])) {
				next
			}

			if (!combined_graph &&
				(!is.list(timeseries_plots[[index]])
				|| !is.list(pvo_plots[[index]])
				|| !is.list(subannual_plots[[index]]))) {
				next
			}

			write_title(paste(site, var@name), 2)

			if (combined_graph) {
				plt <- combined_plots[[index]]
				write_plot(plt)
			} else {
				timeseries_title <- paste0(site, " ", var@name, " Timeseries")
				pvo_title <- paste0(site, " ", var@name
					, " Predicted vs Observed")
				subannual_title <- paste0(site, " ", var@name
					, " Mean Subannual Cycle")

				write_title(timeseries_title, 3)
				write_plot(timeseries_plots[[index]])
				write_title(pvo_title, 3)
				write_plot(pvo_plots[[index]])
				write_title(subannual_title, 3)
				write_plot(subannual_plots[[index]])
			}
		}
	}
	if (use_plotly) {
		return(tags)
	}
}
