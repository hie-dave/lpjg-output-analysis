## -------------------------------------------------------------------------
## Output metadata model (S4) and package-internal registry.
## -------------------------------------------------------------------------

# Enum-like values (validated strings).
set_global("AGGREGATION_LEVELS", c("gridcell", "stand", "patch", "individual"))
set_global("TEMPORAL_RESOLUTIONS", c("subdaily", "daily", "monthly", "annual"))

set_global("AGGREGATION_LABELS", c(
    gridcell = "Gridcell",
    stand = "Stand",
    patch = "Patch",
    individual = "Individual"
))
set_global("TEMPORAL_RESOLUTION_LABELS", c(
    subdaily = "Subdaily",
    daily = "Daily",
    monthly = "Monthly",
    annual = "Annual"
))

#'
#' Validate an enum-like string value.
#'
#' @param value Character scalar to validate.
#' @param allowed Character vector of allowed values.
#' @param field_name Character scalar used in error messages.
#'
#' @return The validated `value`.
#'
#' @keywords internal
#'
.validate_enum_string <- function(value, allowed, field_name) {
    if (!is.character(value) || length(value) != 1) {
        stop(field_name, " must be a single character value")
    }
    if (!(value %in% allowed)) {
        stop(
            "Invalid ", field_name, ": '", value, "'. Allowed values: ",
            paste(allowed, collapse = ", ")
        )
    }
    value
}

#'
#' Validate an aggregation-level value.
#'
#' @param value Character scalar aggregation level.
#'
#' @return The validated `value`.
#'
#' @keywords internal
#'
validate_aggregation_level <- function(value) {
    .validate_enum_string(value, get_global("AGGREGATION_LEVELS"), "aggregation_level")
}

#'
#' Validate a temporal-resolution value.
#'
#' @param value Character scalar temporal resolution.
#'
#' @return The validated `value`.
#'
#' @keywords internal
#'
validate_temporal_resolution <- function(value) {
    .validate_enum_string(value, get_global("TEMPORAL_RESOLUTIONS"), "temporal_resolution")
}

#'
#' Get the display label for an aggregation level.
#'
#' @param value Character scalar aggregation level.
#'
#' @return Character scalar label.
#'
#' @keywords internal
#'
get_aggregation_label <- function(value) {
    value <- validate_aggregation_level(value)
    get_global("AGGREGATION_LABELS")[[value]]
}

#'
#' Get the display label for a temporal resolution.
#'
#' @param value Character scalar temporal resolution.
#'
#' @return Character scalar label.
#'
#' @keywords internal
#'
get_temporal_resolution_label <- function(value) {
    value <- validate_temporal_resolution(value)
    get_global("TEMPORAL_RESOLUTION_LABELS")[[value]]
}

# Model constants / metadata-layer helpers (ported from ModelConstants.cs).
set_global("MODEL_LAYER_LON", "Lon")
set_global("MODEL_LAYER_LAT", "Lat")
set_global("MODEL_LAYER_YEAR", "Year")
set_global("MODEL_LAYER_DAY", "Day")
set_global("MODEL_LAYER_STAND", "stand")
set_global("MODEL_LAYER_PATCH", "patch")
set_global("MODEL_LAYER_INDIV", "indiv")
set_global("MODEL_LAYER_PFT", "pft")
set_global("MODEL_MONTH_COLS", c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Total"
))

#'
#' Get metadata layers for a given aggregation and temporal context.
#'
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#'
#' @return Character vector of metadata layer names.
#'
#' @keywords internal
#'
get_metadata_layers <- function(aggregation_level, temporal_resolution) {
    aggregation_level <- validate_aggregation_level(aggregation_level)
    temporal_resolution <- validate_temporal_resolution(temporal_resolution)

    layers <- c(
        get_global("MODEL_LAYER_LON"),
        get_global("MODEL_LAYER_LAT"),
        get_global("MODEL_LAYER_YEAR")
    )

    if (temporal_resolution %in% c("annual", "daily", "monthly", "subdaily")) {
        layers <- c(layers, get_global("MODEL_LAYER_DAY"))
    }

    if (aggregation_level == "stand") {
        layers <- c(layers, get_global("MODEL_LAYER_STAND"))
    } else if (aggregation_level == "patch") {
        layers <- c(
            layers,
            get_global("MODEL_LAYER_STAND"),
            get_global("MODEL_LAYER_PATCH")
        )
    } else if (aggregation_level == "individual") {
        layers <- c(
            layers,
            get_global("MODEL_LAYER_STAND"),
            get_global("MODEL_LAYER_PATCH"),
            get_global("MODEL_LAYER_INDIV"),
            get_global("MODEL_LAYER_PFT")
        )
    }

    unique(layers)
}

# Interface-like layer definition class hierarchy.
setClass("LayerDefinitions", contains = "VIRTUAL")

setClass(
    "StaticLayers",
    contains = "LayerDefinitions",
    slots = c(
        units_by_layer = "character",
        aggregation_level = "character",
        temporal_resolution = "character"
    )
)

setClass(
    "DynamicLayers",
    contains = "LayerDefinitions",
    slots = c(
        aggregation_level = "character",
        temporal_resolution = "character",
        default_units = "character"
    )
)

setGeneric("is_data_layer", function(x, layer, ...) standardGeneric("is_data_layer"))
setGeneric("get_units", function(x, layer, ...) standardGeneric("get_units"))

#'
#' Normalize static layer definitions to a named layer->unit vector.
#'
#' @param layer_units Layer definitions as named vector/list, unnamed character
#'   vector of layer names, or `data.frame(layer, unit)`.
#' @param default_units Character scalar default unit for unnamed layer names.
#'
#' @return Named character vector of layer units.
#'
#' @keywords internal
#'
.normalise_layer_units <- function(layer_units, default_units = "") {
    if (!is.character(default_units) || length(default_units) != 1) {
        stop("default_units must be a single character value")
    }

    if (is.data.frame(layer_units)) {
        if (!all(c("layer", "unit") %in% colnames(layer_units))) {
            stop("layer_units data.frame must contain columns 'layer' and 'unit'")
        }
        units_by_layer <- as.character(layer_units$unit)
        names(units_by_layer) <- as.character(layer_units$layer)
        return(units_by_layer)
    }

    if (is.list(layer_units)) {
        layer_units <- unlist(layer_units, recursive = FALSE, use.names = TRUE)
    }

    if (!is.character(layer_units)) {
        stop("layer_units must be character, named list, or data.frame(layer, unit)")
    }
    if (length(layer_units) == 0) {
        return(setNames(character(0), character(0)))
    }

    layer_names <- names(layer_units)
    if (is.null(layer_names)) {
        return(setNames(rep(default_units, length(layer_units)),
                        as.character(layer_units)))
    }

    if (all(layer_names == "")) {
        return(setNames(rep(default_units, length(layer_units)),
                        as.character(layer_units)))
    }

    if (any(layer_names == "")) {
        stop("layer_units must have either all names or no names")
    }

    out <- as.character(layer_units)
    names(out) <- names(layer_units)
    out
}

#'
#' Create a `StaticLayers` object.
#'
#' @param layer_units Layer definitions.
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#' @param default_units Character scalar default unit for unnamed layer names.
#'
#' @return A `StaticLayers` object.
#'
#' @keywords internal
#'
create_static_layers <- function(layer_units,
                                 aggregation_level,
                                 temporal_resolution,
                                 default_units = "") {
    units_by_layer <- .normalise_layer_units(layer_units, default_units)
    aggregation_level <- validate_aggregation_level(aggregation_level)
    temporal_resolution <- validate_temporal_resolution(temporal_resolution)
    new(
        "StaticLayers",
        units_by_layer = units_by_layer,
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution
    )
}

#'
#' Create a `DynamicLayers` object.
#'
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#' @param default_units Character scalar default unit for data layers.
#'
#' @return A `DynamicLayers` object.
#'
#' @keywords internal
#'
create_dynamic_layers <- function(
    aggregation_level,
    temporal_resolution,
    default_units
) {
    aggregation_level <- validate_aggregation_level(aggregation_level)
    temporal_resolution <- validate_temporal_resolution(temporal_resolution)
    if (temporal_resolution == "monthly") {
        stop("Monthly resolution is not supported for dynamic layers")
    }
    if (!is.character(default_units) || length(default_units) != 1) {
        stop("default_units must be a single character value")
    }
    new(
        "DynamicLayers",
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution,
        default_units = default_units
    )
}

setMethod("is_data_layer", signature(x = "StaticLayers", layer = "character"),
          function(x, layer, ...) {
              if (length(layer) != 1) {
                  stop("layer must be a single character value")
              }
              !(layer %in% get_metadata_layers(
                  aggregation_level = x@aggregation_level,
                  temporal_resolution = x@temporal_resolution
              ))
          })

setMethod("is_data_layer", signature(x = "DynamicLayers", layer = "character"),
          function(x, layer, ...) {
              if (length(layer) != 1) {
                  stop("layer must be a single character value")
              }
              !(layer %in% get_metadata_layers(
                  aggregation_level = x@aggregation_level,
                  temporal_resolution = x@temporal_resolution
              ))
          })

setMethod("get_units", signature(x = "StaticLayers", layer = "character"),
          function(x, layer, ...) {
              if (!is_data_layer(x, layer)) {
                  stop("Layer '", layer, "' not found")
              }
              if (!(layer %in% names(x@units_by_layer))) {
                  stop("Layer '", layer, "' is not a static data layer")
              }
              x@units_by_layer[[layer]]
          })

setMethod("get_units", signature(x = "DynamicLayers", layer = "character"),
          function(x, layer, ...) {
              if (!is_data_layer(x, layer, ...)) {
                  stop("Layer '", layer, "' is a metadata layer")
              }
              x@default_units
          })

# Output-file metadata.
setClass(
    "OutputFileMetadata",
    slots = c(
        id = "character",
        name = "character",
        description = "character",
        units = "character",
        aggregation_level = "character",
        temporal_resolution = "character",
        layers = "LayerDefinitions"
    )
)

#'
#' Create output metadata.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar default unit string.
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#' @param layers Optional `LayerDefinitions` object or layer spec.
#'
#' @return An `OutputFileMetadata` object.
#'
#' @keywords internal
#'
create_output_metadata <- function(
    id,
    name,
    description,
    units = "",
    aggregation_level,
    temporal_resolution,
    layers = NULL
) {
    if (!is.character(id) || length(id) != 1 || id == "") {
        stop("id must be a non-empty character value")
    }
    if (!is.character(name) || length(name) != 1) {
        stop("name must be a single character value")
    }
    if (!is.character(description) || length(description) != 1) {
        stop("description must be a single character value")
    }
    if (!is.character(units) || length(units) != 1) {
        stop("units must be a single character value")
    }
    aggregation_level <- validate_aggregation_level(aggregation_level)
    temporal_resolution <- validate_temporal_resolution(temporal_resolution)
    if (is.null(layers)) {
        layers <- create_dynamic_layers(aggregation_level,
                                        temporal_resolution,
                                        units)
    } else if (is.list(layers) || is.data.frame(layers) || is.character(layers)) {
        layers <- create_static_layers(layers, aggregation_level,
                                       temporal_resolution, units)
    }

    if (!is(layers, "LayerDefinitions")) {
        stop("layers must inherit from LayerDefinitions")
    }

    new(
        "OutputFileMetadata",
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution,
        layers = layers
    )
}

#'
#' Create output metadata for a PFT-style output.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar default unit string.
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#' @param layers Optional `LayerDefinitions` object or layer spec.
#'
#' @return An `OutputFileMetadata` object.
#'
#' @keywords internal
#'
create_pft_output_metadata <- function(
    id,
    name,
    description,
    units = "",
    aggregation_level = "patch",
    temporal_resolution = "daily",
    layers = NULL
) {
    create_output_metadata(
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution,
        layers = layers
    )
}

#'
#' Create output metadata for a monthly output.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar default unit string.
#' @param aggregation_level Character scalar aggregation level.
#' @param layers Optional `LayerDefinitions` object or layer spec.
#'
#' @return An `OutputFileMetadata` object.
#'
#' @keywords internal
#'
create_monthly_output_metadata <- function(
    id,
    name,
    description,
    units = "",
    aggregation_level = "gridcell",
    layers = NULL
) {
    create_output_metadata(
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        temporal_resolution = "monthly",
        layers = layers
    )
}

# Registry helpers.
#'
#' Initialise the output metadata registry if required.
#'
#' @return Registry environment (invisibly).
#'
#' @keywords internal
#'
initialise_output_registry <- function() {
    registry <- get_global("output_registry")
    if (is.null(registry)) {
        registry <- new.env(parent = emptyenv())
        set_global("output_registry", registry)
    }
    invisible(registry)
}

#'
#' Register an output metadata object.
#'
#' @param metadata An `OutputFileMetadata` object.
#'
#' @return The registered metadata object (invisibly).
#'
#' @keywords internal
#'
register_output_metadata <- function(metadata) {
    if (!is(metadata, "OutputFileMetadata")) {
        stop("metadata must be an OutputFileMetadata object")
    }
    registry <- initialise_output_registry()
    if (metadata@id %in% names(registry)) {
        log_warning("Overwriting output metadata with ID: ", metadata@id)
    }
    registry[[metadata@id]] <- metadata
    invisible(metadata)
}

#'
#' Get output metadata by ID.
#'
#' @param id Character scalar output ID.
#'
#' @return An `OutputFileMetadata` object, or `NULL` if not found.
#'
#' @export
#'
get_output_metadata <- function(id) {
    registry <- initialise_output_registry()
    if (!id %in% names(registry)) {
        return(NULL)
    }
    registry[[id]]
}

#'
#' Guess output metadata from an output filename.
#'
#' The filename stem is mapped with the convention `file_x = "x.out"`.
#'
#' @param filename Character scalar filename or path.
#'
#' @return An `OutputFileMetadata` object, or `NULL` if no match is found.
#'
#' @export
#'
guess_metadata_from_filename <- function(filename) {
    # TODO: implement .ins file parser, to generate map of file types (IDs) to
    # file names. That would be a separate API entrypoint - this would probably
    # still be needed as a "best-guess" which in practice will almost always be
    # correct.

    # Trim directory and file extension.
    filename <- basename(filename)
    filename <- tools::file_path_sans_ext(filename)

    # Follow the convention of file_x = "x.out" by prepending file_ prefix.
    id <- paste0("file_", filename)
    get_output_metadata(id)
}

#'
#' Get all registered output IDs.
#'
#' @return Character vector of output IDs.
#'
#' @export
#'
get_output_metadata_ids <- function() {
    registry <- initialise_output_registry()
    names(registry)
}

#'
#' Clear the output metadata registry.
#'
#' Primarily intended for tests.
#'
#' @return `NULL`, invisibly.
#'
#' @keywords internal
#'
clear_output_registry <- function() {
    set_global("output_registry", new.env(parent = emptyenv()))
}

#'
#' Evaluate a static layer expression.
#'
#' Handles malformed `c(...)` expressions by removing empty arguments.
#'
#' @param expr Expression representing a layer definition.
#' @param env Environment in which to evaluate `expr`.
#'
#' @return Evaluated layer definition object.
#'
#' @keywords internal
#'
.evaluate_layers_expr <- function(expr, env) {
    if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
        parts <- as.list(expr)[-1]
        is_empty_arg <- vapply(parts, function(p) {
            is.symbol(p) && identical(as.character(p), "")
        }, logical(1))
        parts <- parts[!is_empty_arg]
        part_names <- names(parts)
        values <- lapply(parts, function(p) eval(p, envir = env))
        names(values) <- part_names
        return(do.call(c, values))
    }
    eval(expr, envir = env)
}

#'
#' Register an output with dynamic data layers.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar unit string.
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#'
#' @return The registered metadata object (invisibly).
#'
#' @keywords internal
#'
add_output_dynamic <- function(
    id,
    name,
    description,
    units,
    aggregation_level,
    temporal_resolution
) {
    metadata <- create_output_metadata(
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution,
        layers = NULL
    )
    register_output_metadata(metadata)
}

#'
#' Register an output with static data layers.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar unit string.
#' @param layers Static layer definitions.
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#'
#' @return The registered metadata object (invisibly).
#'
#' @keywords internal
#'
add_output_static <- function(
    id,
    name,
    description,
    units,
    layers,
    aggregation_level,
    temporal_resolution
) {
    layers_value <- .evaluate_layers_expr(substitute(layers), parent.frame())

    metadata <- create_output_metadata(
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution,
        layers = layers_value
    )
    register_output_metadata(metadata)
}

#'
#' Register a PFT output.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar unit string.
#' @param aggregation_level Character scalar aggregation level.
#' @param temporal_resolution Character scalar temporal resolution.
#'
#' @return The registered metadata object (invisibly).
#'
#' @keywords internal
#'
add_pft_output <- function(
    id,
    name,
    description,
    units,
    aggregation_level,
    temporal_resolution
) {
    metadata <- create_pft_output_metadata(
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        temporal_resolution = temporal_resolution,
        layers = create_dynamic_layers(aggregation_level, temporal_resolution,
                                       units)
    )
    register_output_metadata(metadata)
}

#'
#' Register a monthly output using standard month columns.
#'
#' @param id Character scalar output ID.
#' @param name Character scalar short name.
#' @param description Character scalar description.
#' @param units Character scalar unit string.
#' @param aggregation_level Character scalar aggregation level.
#'
#' @return The registered metadata object (invisibly).
#'
#' @keywords internal
#'
add_monthly_output <- function(
    id,
    name,
    description,
    units = "",
    aggregation_level = "gridcell"
) {
    layers <- create_static_layers(get_global("MODEL_MONTH_COLS"),
                                   aggregation_level, "monthly", units)
    metadata <- create_monthly_output_metadata(
        id = id,
        name = name,
        description = description,
        units = units,
        aggregation_level = aggregation_level,
        layers = layers
    )
    register_output_metadata(metadata)
}
