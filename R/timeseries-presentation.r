# Shared labels for the site-level Shiny time series. Keep complete trace
# identities in hover text, but put only distinguishing fields in the legend.
timeseries_trace_labels <- function(data, fields, gridcell_by_job = FALSE) {
  traces <- data[!duplicated(data$Series), , drop = FALSE]
  values <- lapply(fields, function(field) {
    value <- as.character(traces[[field]])
    value[is.na(value)] <- ""
    value
  })
  names(values) <- fields
  full_values <- values

  if (gridcell_by_job && "Gridcell" %in% fields) {
    gridcells <- tapply(data$Gridcell, data$Job, function(value) {
      length(unique(value[nzchar(value)]))
    })
    values$Gridcell[gridcells[traces$Job] <= 1L] <- ""
  }

  varying <- fields[vapply(values, function(value) {
    length(unique(value)) > 1L
  }, logical(1))]
  combine <- function(i, columns, source) {
    parts <- vapply(columns, function(column) source[[column]][i], character(1))
    parts <- parts[nzchar(parts)]
    if (length(parts)) paste(parts, collapse = " / ") else "Series"
  }
  compact <- vapply(seq_len(nrow(traces)), combine, character(1), varying, values)
  full <- vapply(seq_len(nrow(traces)), combine, character(1), fields, full_values)
  if (anyDuplicated(compact)) {
    # A catalog may contain distinct jobs with the same displayed metadata.
    # Keep their legend entries separate and deterministic.
    compact <- make.unique(compact, sep = " #")
  }
  data.frame(Series = traces$Series, Name = compact, Full = full,
             stringsAsFactors = FALSE)
}

timeseries_quantity_title <- function(quantity) {
  metadata <- resolve_quantity_metadata(quantity)
  if (!is.null(metadata)) {
    description <- metadata$metadata@description
    if (length(description) == 1L && !is.na(description) &&
        nzchar(description)) return(description)
    if (!is.null(metadata$name) && nzchar(metadata$name)) return(metadata$name)
  }
  readable_name(quantity)
}

timeseries_plot_title <- function(data, site = NULL, title = NULL) {
  if (!is.null(title) && length(title) == 1L && !is.na(title) &&
      nzchar(trimws(title))) return(trimws(title))
  quantities <- unique(data$Quantity)
  quantity_names <- vapply(quantities, timeseries_quantity_title, character(1))
  quantity_title <- if (length(quantity_names) <= 3L) {
    paste(quantity_names, collapse = " + ")
  } else paste(length(quantity_names), "LPJ-GUESS outputs")
  context <- character()
  if (!is.null(site) && length(site) == 1L && nzchar(site)) {
    context <- site
  } else if ("Site" %in% names(data) && length(unique(data$Site)) == 1L) {
    context <- unique(data$Site)
  }
  if ("Simulation" %in% names(data) &&
      length(unique(data$Simulation)) == 1L) {
    context <- c(context, unique(data$Simulation))
  }
  paste(c(quantity_title, context), collapse = " — ")
}

timeseries_y_title <- function(data) {
  quantities <- unique(data$Quantity)
  if (length(quantities) != 1L) return("Value")
  quantity <- quantities[[1L]]
  metadata <- resolve_quantity_metadata(quantity)
  if (is.null(metadata)) return("Value")
  layers <- unique(data$Layer)
  units <- vapply(layers, function(layer) {
    info <- resolve_quantity_metadata(quantity, layer = layer)
    if (is.null(info) || is.null(info$units) || length(info$units) != 1L ||
        is.na(info$units)) "" else info$units
  }, character(1))
  if (length(unique(units)) != 1L || !nzchar(units[1L])) return("Value")
  name <- metadata$name
  if (is.null(name) || !nzchar(name)) name <- quantity
  paste0(name, " [", units[1L], "]")
}

timeseries_legend_layout <- function(location = "right", direction = "vertical") {
  if (length(location) != 1L || !location %in% c("left", "right", "top", "bottom")) {
    location <- "right"
  }
  if (length(direction) != 1L || !direction %in% c("horizontal", "vertical")) {
    direction <- "vertical"
  }
  orientation <- if (identical(direction, "horizontal")) "h" else "v"
  position <- switch(location,
    left = list(x = -0.02, y = 1, xanchor = "right", yanchor = "top"),
    top = list(x = 0.5, y = 1.02, xanchor = "center", yanchor = "bottom"),
    bottom = list(x = 0.5, y = -0.18, xanchor = "center", yanchor = "top"),
    list(x = 1.02, y = 1, xanchor = "left", yanchor = "top"))
  c(list(orientation = orientation), position)
}
