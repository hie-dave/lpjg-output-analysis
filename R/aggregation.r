
#'
#' Convert the coordinate to the closest coordinate on a 1-degree grid.
#'
#' @param coord: The coordinate value.
#'
#' @keywords internal
#'
convert_to_1degree <- function(coord) {
    floor(coord) + 0.5
}

#'
#' For a given coordinate, return the closest coordinate from the list coords.
#'
#' @param coord A coordinate value.
#' @param coords List of coordinate values.
#'
#' @keywords internal
#'
closest <- function(coord, coords) {
    return(coords[[which.min(abs(coord - coords))]])
}

#'
#' Get names of columns by which the data frame should be grouped when
#' aggregating spatially.
#'
#' @param df A data frame.
#'
#' @keywords internal
#'
get_grouping_names <- function(df) {
    cols <- c("Lon_1deg", "Lat_1deg")
    for (col in c("Day", "Month", "Year")) {
        if (col %in% names(df)) {
            cols <- c(cols, col)
        }
    }
    return(cols)
}

#'
#' Aggregate the given data frame to 1-degree by taking the mean of the closest
#' neighbours to each gridcell on a 1-degree grid.
#'
#' @param df A data frame.
#'
#' @return A data-frame on a 1-degree grid.
#'
#' @import dplyr
#' @export
#'
aggregate_to_1deg <- function(df) {
    df <- df %>% mutate(Lon_1deg = convert_to_1degree(Lon),
                        Lat_1deg = convert_to_1degree(Lat))
    groups <- get_grouping_names(df)
    df_1deg <- df %>%
        group_by(across(all_of(groups))) %>%
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
                  .groups = 'drop')
    df_1deg$Lon <- df_1deg$Lon_1deg
    df_1deg$Lat <- df_1deg$Lat_1deg
    df_1deg <- df_1deg[, setdiff(names(df_1deg), c("Lon_1deg", "Lat_1deg"))]
    df_1deg <- as.data.table(df_1deg)
    return(df_1deg)
}

#'
#' Aggregate the given data frame to the grid specified by the given set of
#' coordinates, by taking the mean of the nearest neighbours on the target grid.
#'
#' @param df A data frame.
#' @param lons List of longitude values on the target grid.
#' @param lats List of latitude values on the target grid.
#'
#' @return A data-frame on a the target grid.
#'
#' @import dplyr
#' @export
#'
aggregate_to <- function(df, lons, lats) {
    df <- df %>% mutate(Lon_1deg = sapply(Lon, \(c) closest(c, lons)),
                        Lat_1deg = sapply(Lat, \(c) closest(c, lats)))
    groups <- get_grouping_names(df)
    df_1deg <- df %>%
        group_by(across(all_of(groups))) %>%
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)),
                  .groups = 'drop')
    df_1deg$Lon <- df_1deg$Lon_1deg
    df_1deg$Lat <- df_1deg$Lat_1deg
    df_1deg <- df_1deg[, setdiff(names(df_1deg), c("Lon_1deg", "Lat_1deg"))]
    df_1deg <- as.data.table(df_1deg)
    return(df_1deg)
}
