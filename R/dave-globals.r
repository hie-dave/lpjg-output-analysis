# An environment to hold global variables for the package.
.pkg_env <- new.env(parent = emptyenv())

#'
#' Set the package-internal global variable with the specified name.
#'
#' @param name: Name of the variable.
#' @param value: Value of the variable.
#'
#' @keywords internal
#' @author Drew Holzworth
#'
set_global <- function(name, value) {
	assign(name, value, .pkg_env)
}

#'
#' Get the package-internal global variable with the specified name.
#'
#' @param name: Name of the variable.
#'
#' @return Returns the value of the variable
#'
#' @keywords internal
#' @author Drew Holzworth
#'
get_global <- function(name) {
	return(.pkg_env[[name]])
}
