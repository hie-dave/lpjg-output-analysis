set_global("LOG_LEVEL_ERROR", 0)
set_global("LOG_LEVEL_WARNING", 1)
set_global("LOG_LEVEL_INFORMATION", 2)
set_global("LOG_LEVEL_DIAGNOSTIC", 3)
set_global("LOG_LEVEL_DEBUG", 4)

set_global("log_level", get_global("LOG_LEVEL_INFORMATION"))
set_global("log_file", "")
set_global("warning_as_error", FALSE)
set_global("log_writers", new.env(parent = emptyenv()))

#'
#' Set the log level used by dave_analysis logging functions.
#'
#' This should be in range 0-4. Higher values will result in more log messages
#' being written.
#'
#' @param level Log level (0-4). Higher numbers mean more verbose output
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
#' Get the current log level used by dave_analysis logging functions.
#'
#' @return Log level (0-4)
#' @keywords internal
#'
get_log_level <- function() {
    return(get_global("log_level"))
}

#'
#' Set the log file used by dave_analysis logging functions.
#'
#' If this includes a directory component, the parent directory must exist.
#' The log file will not be truncated.
#'
#' @param file Path to log file. Empty string or NULL means stdout
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

#'
#' Register a custom log writer.
#'
#' Custom writers are called for each log line that passes the current log
#' level filter.
#'
#' @param name Unique name for this writer.
#' @param writer Function accepting `(line, level)`.
#'
#' @return Writer name, invisibly.
#' @keywords internal
#'
register_log_writer <- function(name, writer) {
    if (!is.character(name) || length(name) != 1 || name == "") {
        log_error("log writer name must be a non-empty string")
    }
    if (!is.function(writer)) {
        log_error("log writer must be a function")
    }
    writers <- get_global("log_writers")
    writers[[name]] <- writer
    invisible(name)
}

#'
#' Unregister a custom log writer.
#'
#' @param name Writer name to remove.
#'
#' @return `TRUE` if a writer was removed, otherwise `FALSE`.
#' @keywords internal
#'
unregister_log_writer <- function(name) {
    if (!is.character(name) || length(name) != 1 || name == "") {
        return(FALSE)
    }
    writers <- get_global("log_writers")
    if (!exists(name, envir = writers, inherits = FALSE)) {
        return(FALSE)
    }
    rm(list = name, envir = writers)
    TRUE
}

#'
#' Remove all custom log writers.
#'
#' @return `NULL`, invisibly.
#' @keywords internal
#'
clear_log_writers <- function() {
    set_global("log_writers", new.env(parent = emptyenv()))
    invisible(NULL)
}

write_log_message <- function(..., level) {
    if (level <= get_global("log_level")) {
        l <- level_to_string(level)
        timestr <- format(Sys.time(), "%H:%M:%S")
        pfx <- paste0("[", timestr, " ", l, "] ")
        msg <- paste0(...)
        msg <- gsub("\n", paste0("\n", pfx), msg)
        line <- paste0(pfx, msg, "\n")

        file <- get_global("log_file")
        cat(line, file = file, append = TRUE)

        writers <- get_global("log_writers")
        writer_names <- ls(envir = writers, all.names = TRUE)
        for (name in writer_names) {
            writer <- writers[[name]]
            tryCatch(
                writer(line, level),
                error = function(e) {
                    warning("Log writer '", name, "' failed: ", e$message,
                        call. = FALSE)
                }
            )
        }
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
