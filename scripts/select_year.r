require("ncdf4")
require("CFtime")

run_cmd <- function(cmd, args, silent = FALSE) {
    if (!silent) {
        cat(paste(cmd, paste(args, collapse = " ")), "\n")
    }
    result <- system2(cmd, args = args, stdout = TRUE, stderr = TRUE)
    status <- attr(result, "status")
    if (!is.null(status)) {
        cat(paste(result, collapse = "\n"), "\n")
        stop("Command failed. Check stdout/stderr for more details.")
    } else if (!silent && length(result) > 0) {
        cat(paste(result, collapse = "\n"), "\n")
    }
}

select_year <- function(year, input_file, output_file, lat_name = "latitude",
                        lon_name = "longitude", silent = TRUE) {
    # Check if cdo is installed.
    if (Sys.which("cdo") == "") {
        stop("ERROR: cdo is not installed")
    }
    if (Sys.which("ncpdq") == "") {
        stop("ERROR: nco is not installed")
    }
    if (normalizePath(input_file) == normalizePath(output_file)) {
        stop("Input and output files are pointing to the same file. This would work, but is not recommended.")
    }

    # Reorder dimensions for cdo (it requires time as first dimension).
    tmp0 <- tempfile(fileext = ".nc")
    dims <- paste("time", lat_name, lon_name, sep = ",")
    args <- c("-O", "-a", dims, input_file, tmp0)
    run_cmd("ncpdq", args = args, silent = silent)

    # Use cdo to extract the specific year of data required.
    tmp1 <- tempfile(fileext = ".nc")
    args <- c("-O", paste0("selyear,", year), tmp0, tmp1)
    run_cmd("cdo", args, silent = silent)

    # Reorder dimensions back to something optimal for LPJ-Guess.
    dims <- paste(lat_name, lon_name, "time", sep = ",")
    time_chunk_size <- 8760 # fixme: read this from input file
    compression_level <- 5  # fixme: read this from input file
    args <- c("-O", "-a", dims,
              "--cnk_dmn", paste0("time,", time_chunk_size),
              "--cnk_dmn", paste0(lat_name, ",1"),
              "--cnk_dmn", paste0(lon_name, ",1"),
              paste0("-L", compression_level),
              tmp1, output_file)
    run_cmd("ncpdq", args = args, silent = silent)
}

# Setting throrough to TRUE will give the "correct" answer, but is much slower.
# Setting thorough to FALSE will look at the first/last time values only, and
# interpolate between these. This should usually be correct, and is faster.
list_years <- function(input_file, thorough = FALSE) {
    nc <- nc_open(input_file)
    calendar <- ncatt_get(nc, "time", "calendar")$value
    units <- ncatt_get(nc, "time", "units")$value
    if (thorough) {
        values <- ncvar_get(nc, "time")
        dates <- as.Date(as_timestamp(CFtime(units, calendar, values)))
        years <- unique(format(dates, "%Y"))
    } else {
        nvalues <- nc$dim$time$len
        first <- ncvar_get(nc, "time", start = 1, count = 1)
        last <- ncvar_get(nc, "time", start = nvalues, count = 1)
        dates <- as.Date(as_timestamp(CFtime(units, calendar, c(first, last))))
        years <- unique(format(dates, "%Y"))
        years <- min(years):max(years)
    }
    nc_close(nc)
    return(years)
}
