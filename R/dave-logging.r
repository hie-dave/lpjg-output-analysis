
set_global("LOG_LEVEL_ERROR", 0)
set_global("LOG_LEVEL_WARNING", 1)
set_global("LOG_LEVEL_INFORMATION", 2)
set_global("LOG_LEVEL_DIAGNOSTIC", 3)
set_global("LOG_LEVEL_DEBUG", 4)

set_global("log_level", get_global("LOG_LEVEL_INFORMATION"))
set_global("log_file", "")
set_global("warning_as_error", FALSE)

#'
#' Set the log level used by dave_analysis logging functions.
#'
#' This should be in range 0-4. Higher values will result in more log messages
#' being written.
#'
#' @author Drew Holzworth
#' @export
#'
set_log_level <- function(level) {
	debug <- get_global("LOG_LEVEL_DEBUG")
	if (level < 0 || level > debug) {
		error <- get_global("LOG_LEVEL_ERROR")
		log_error("Attempted to set log level to ", level,
			", which is outside valid range [", error, ", ", debug, "]")
	}
	log_debug("Setting log level to ", level)
	set_global("log_level", level)
	log_debug("Successfully set log level to ", level)
}

#'
#' Set the log file used by daveanalysis logging functions.
#'
#' If this includes a directory component, the parent directory must exist.
#' The log file will not be truncated.
#'
#' @param file The file to which log messages will be written.
#' @author Drew Holzworth
#' @export
#'
set_log_file <- function(file) {
	if (is.null(file)) {
		log_debug("Was provided a NULL log file")
		file <- ""
	}
	log_debug("Setting log_file to '", file, "'")
	set_global("log_file", file)
	log_debug("Successfully set log file to '", get_global("log_file"), "'")
}

set_warning_as_error <- function(value) {
	if (value == TRUE) {
		set_global("warning_as_error", TRUE)
	} else if (value == FALSE) {
		set_global("warning_as_error", FALSE)
	} else {
		log_error("Attempted to set warning_as_error to invalid value: ", value)
	}
}

level_to_string <- function(level) {
	if (level == get_global("LOG_LEVEL_ERROR"))
		return("ERR")
	if (level == get_global("LOG_LEVEL_WARNING"))
		return("WRN")
	if (level == get_global("LOG_LEVEL_INFORMATION"))
		return("INF")
	if (level == get_global("LOG_LEVEL_DIAGNOSTIC"))
		return("DIA")
	if (level == get_global("LOG_LEVEL_DEBUG"))
		return("DBG")
	return("MSG")
}

write_log_message <- function(..., level) {
	if (level <= get_global("log_level")) {
		l <- level_to_string(level)
		timestr <- format(Sys.time(), "%H:%M:%S")
		pfx <- paste0("[", timestr, " ", l, "] ")
		msg <- paste0(...)
		msg <- gsub("\n", paste0("\n", pfx), msg)
		file = get_global("log_file")
		cat(paste0(pfx, msg, "\n"), file = file, append = TRUE)
	}
}

log_error <- function(...) {
	stop(..., call. = FALSE)
}

log_warning <- function(...) {
	if (get_global("warning_as_error")) {
		log_error(...)
	} else {
		write_log_message(..., level = get_global("LOG_LEVEL_WARNING"))
	}
}

log_info <- function(...) {
	write_log_message(..., level = get_global("LOG_LEVEL_INFORMATION"))
}

log_diag <- function(...) {
	write_log_message(..., level = get_global("LOG_LEVEL_DIAGNOSTIC"))
}

log_debug <- function(...) {
	write_log_message(..., level = get_global("LOG_LEVEL_DEBUG"))
}
