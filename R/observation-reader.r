
#'
#' ObservationReader class
#'
#' A class representing a reader for observation data. This class provides a
#' standardized interface for reading observation data from various sources.
#'
#' @slot src Source. A DGVMTools::Source object.
#' @slot file_path Character. Path to the data file.
#' @slot available_quantities Function. Get available quantities.
#' @slot available_layers Function. Get available layers.
#' @slot read_func Function. Function to read the data.
#'
#' @exportClass ObservationReader
#'
setClass(
    "ObservationReader",
    slots = list(
        src = "Source",                    # DGVMTools::Source object
        file_path = "character",           # Path to the data file
        available_quantities = "function", # Get available quantities
        available_layers = "function",     # Get available layers
        read_func = "function"             # Function to read the data
    ),
    prototype = list(
        src = NULL,
        file_path = "",
        available_quantities = function() character(0),
        available_layers = function() character(0),
        read_func = function(var_id, ...) { stop("Not implemented") }
    )
)

#'
#' Read observation data
#'
#' Read observation data for a specific variable using the provided reader.
#'
#' @param reader An ObservationReader object.
#' @param var_id Character. ID of the variable to read.
#' @param ... Additional arguments passed to the reader function.
#'
#' @return A DGVMTools::Field object containing the observation data.
#'
#' @export
#'
setGeneric("read_observation", function(reader, var_id, ...) standardGeneric("read_observation"))

#' @rdname read_observation
setMethod(
    "read_observation",
    signature(reader = "ObservationReader", var_id = "character"),
    function(reader, var_id, ...) {
        if (!var_id %in% reader@available_quantities()) {
            stop("Variable '", var_id, "' not supported by this reader")
        }
        # Call the reader's read_func directly
        result <- reader@read_func(var_id, ...)
        return(result)
    }
)

#'
#' Check if a variable is supported by a reader
#'
#' @param reader An ObservationReader object.
#' @param var_id Character. ID of the variable to check.
#'
#' @return Logical. TRUE if the variable is supported, FALSE otherwise.
#'
#' @export
#'
setGeneric("supports_variable", function(reader, var_id) standardGeneric("supports_variable"))

#' @rdname supports_variable
setMethod(
    "supports_variable",
    signature(reader = "ObservationReader", var_id = "character"),
    function(reader, var_id) {
        return(var_id %in% reader@available_quantities())
    }
)

#'
#' Show method for ObservationReader
#'
#' @param object An ObservationReader object.
#'
#' @return None. Prints information about the reader to the console.
#'
#' @export
#'
setMethod(
    "show",
    signature(object = "ObservationReader"),
    function(object) {
        cat("ObservationReader:", object@src@id, "\n")
        cat("Description:", object@src@name, "\n")
        cat("File path:", object@file_path, "\n")
        cat("Available quantities:",
            paste(object@available_quantities(), collapse = ", "),
            "\n")
        cat("Available layers:",
            paste(object@available_layers(), collapse = ", "),
            "\n")
    }
)

#' Registry for observation readers
#'
#' This registry stores all registered observation readers and provides
#' functions to register, find, and list readers.
#'

#'
#' Register an observation reader
#'
#' @param reader An ObservationReader object to register
#' @return The registered reader (invisibly)
#' @export
#'
register_reader <- function(reader) {
    if (!is(reader, "ObservationReader")) {
        stop("Reader must be an ObservationReader object")
    }
    readers_env <- get_global("observation_readers")
    readers_env[[reader@src@id]] <- reader
}

#'
#' Find readers that support a given variable
#'
#' @param var_id Character. ID of the variable to find readers for
#' @return A list of readers that support the variable
#' @export
#'
find_readers_for_var <- function(var_id) {
    readers_env <- get_global("observation_readers")
    readers <- as.list(readers_env)
    supported_readers <- list()
    for (id in names(readers)) {
        reader <- readers[[id]]
        if (var_id %in% reader@available_quantities()) {
            supported_readers[[id]] <- reader
        }
    }
    return(supported_readers)
}

#'
#' Get a reader by ID
#'
#' @param id Character. ID of the reader to get
#' @return The reader object or NULL if not found
#' @export
#'
get_reader <- function(id) {
    readers_env <- get_global("observation_readers")
    if (!id %in% names(readers_env)) {
        return(NULL)
    }
    return(readers_env[[id]])
}

#'
#' Get the names of all registered observation readers.
#'
#' @return A character vector of reader IDs
#' @export
#'
list_readers <- function() {
    readers_env <- get_global("observation_readers")
    return(names(readers_env))
}

#'
#' Get all registered observation readers.
#'
#' @return List of ObservationReader objects.
#' @export
#'
list_all_readers <- function() {
    readers_env <- get_global("observation_readers")
    return(as.list(readers_env))
}

#'
#' Create a NetCDF reader for the default ozflux-obs.nc file
#'
#' @return An ObservationReader object for the default NetCDF file.
#'
#' @export
#'
create_netcdf_reader <- function() {
    # Get the path to the netcdf file
    obs_dir <- system.file("data", package = get_global("dave_pkgname"))
    file_path <- file.path(obs_dir, get_global("obs_file"))

    # Define the supported variables
    # TODO: should read this dynamically.
    obs_vars <- get_observed_vars()
    supported_vars <- sapply(obs_vars, function(x) x@id)

    # Create mapping from observation variables to model variables
    # For the default NetCDF, the mapping is identity (same IDs)
    model_var_mapping <- list()
    for (var_id in supported_vars) {
        model_var_mapping[[var_id]] <- paste0("dave_", var_id)
    }

    # Define the read function
    read_func <- function(var_id, sites = NULL, ...) {
        # Use existing code to read from the NetCDF file
        obs_source <- get_observed_source()

        # Find the quantity object for this variable
        var_obj <- NULL
        for (v in get_observed_vars()) {
            if (v@id == var_id) {
                var_obj <- v
                break
            }
        }

        if (is.null(var_obj)) {
            stop("Variable '", var_id, "' not found in observed variables")
        }
        verbose <- get_global("log_level") >= get_global("LOG_LEVEL_DEBUG")
        # Use existing DGVMTools functions to read the data
        field <- DGVMTools::getField(obs_source,
                                     var_obj,
                                     layers = var_obj@id,
                                     file.name = get_global("obs_file"),
                                     verbose = verbose)
        return(field)
    }

    # Create and return the reader object
    reader <- new("ObservationReader",
                  src = get_observed_source(),
                  file_path = file_path,
                  available_quantities = function() supported_vars,
                  available_layers = function() supported_vars,
                  read_func = read_func)

    return(reader)
}

#'
#' Create a CSV file reader.
#'
#' @param file_path Character. Path to the CSV file.
#' @param quant Character vector. The quantity encapsulated by this file.
#' @param layers Character vector. List of layers to read. If this is a named
#' vector, the names are used as the variable IDs.
#'
#' @return An ObservationReader object.
#'
register_csv_reader <- function(id,
                                name,
                                file_path,
                                quant,
                                layers,
                                lat_col = "Lat",
                                lon_col = "Lon",
                                site_col = NULL,
                                time_col = "date") {
    src <- defineSource(
        id = id,
        name = name,
        dir = dirname(file_path),
        format = CSV
    )

    return(new("ObservationReader",
        src = src,
        file_path = file_path,
        available_quantities = function() quant,
        available_layers = function() layers,
        read_func = function(var_id, ...) {
            return(get_field_csv(
                src,
                sanitise_variable(var_id),
                layers,
                file.name = file_path,
                lat_col = lat_col,
                lon_col = lon_col,
                site_col = site_col,
                time_col = time_col,
                ...
            ))
        }
    ))
}

# Initialize the registry and register the default readers on package load.
.onLoad <- function(libname, pkgname) {
    # Initialize the readers registry if it doesn't exist
    if (is.null(get_global("observation_readers"))) {
        set_global("observation_readers", new.env(parent = emptyenv()))
    }

    # Register the default NetCDF reader
    register_reader(create_netcdf_reader())
}
