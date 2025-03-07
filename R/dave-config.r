#'
#' Configure ozflux versions.
#' @param versions A list of [DGVMTools::version] objects
#' @param log_level Optional log level (0-4, default 2) (see: [set_log_level])
#' @param log_file Log file to be used. Empty string or NULL means stdout
#' @param warning_as_error iff TRUE, treat warnings as errors (default: FALSE)
#'
#' @export
dave_config <- function(versions, log_level, warning_as_error, log_file) {
	# Set default sources.
	if (!missing(versions) && !is.null(versions)) {
		set_global(".versions", sanitise_sources(versions))
	}

	# Set default log level.
	if (!missing(log_level) && !is.null(log_level)) {
		set_log_level(log_level)
	}

	if (!missing(log_file)) {
		set_log_file(log_file)
	}

	# Set whether warnings should be treated as errors.
	if (!missing(warning_as_error)) {
		set_warning_as_error(warning_as_error)
	}
}
