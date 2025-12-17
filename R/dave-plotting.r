
#'
#' Create an animation of a gridded output variable over time.
#'
#' @param src The source to be animated. E.g. a directory containing LPJ-GUESS
#' output files.
#' @param var The variable to animate (e.g. "lai" for lai.out).
#' @param layer The layer to animate. E.g. "TeBE", "Total", etc.
#' @param title The title of the animation.
#' @param out_file The output file to save the animation to. If NULL, the
#' animation will not be saved (but will be returned).
#' @param renderer The renderer to use for the animation. If NULL, the default
#' renderer will be used.
#'
#' @return A gganimate object.
#' @export
dave_animation <- function(src,
                           var,
                           layer,
                           title,
                           out_file = NULL,
                           renderer = NULL) {
    src <- sanitise_source(src)
    var <- sanitise_variable(var)

    fld <- read_data(src, var, layers = layer, read_obs = FALSE)
    dave_animate_field(fld, title, layer, out_file = out_file, renderer = renderer)
}

dave_animate_field <- function(fld, title, layer, out_file = NULL, renderer = NULL) {
    data <- fld@data

    data$date <- calc_date(data)
    cols <- fld@quant@colours(20)
    p <- ggplot(data) +
        geom_raster(aes(x = Lon, y = Lat, fill = get(layer))) +
        gganimate::transition_time(date) +
        gganimate::ease_aes("linear") +
        labs(title = title, subtitle = "Date: {frame_time}") +
        scale_fill_gradientn(name = layer, colors = cols) +
        theme_minimal()
    p <- gganimate::animate(p, renderer = renderer)
    if (!is.null(out_file)) {
        gganimate::anim_save(out_file)
    } else {
        return(p)
    }
}

# Compute dominance metrics for a set of PFT columns in a data.table
.compute_dominance_metrics <- function(dt, pfts) {
    stopifnot(all(pfts %in% names(dt)))
    # 1) Clamp negative component values to 0 for dominance/entropy calculations
    pos_cols <- paste0(".__pos__", pfts)
    dt[, (pos_cols) := lapply(.SD, function(v) pmax(v, 0)), .SDcols = pfts]

    # 2) Totals across selected PFTs (non-negative components)
    dt[, total__tmp := rowSums(.SD, na.rm = TRUE), .SDcols = pos_cols]

    # 3) Dominant PFT (use clamped components). Set to NA where total == 0 (undefined)
    dt[, dominant_pft := pfts[max.col(.SD, ties.method = "first")], .SDcols = pos_cols]
    dt[total__tmp == 0 | !is.finite(total__tmp), dominant_pft := NA_character_]

    # 4) Top and second values for dominance margin (from clamped components)
    get_second <- function(x) {
        x <- as.numeric(x)
        x <- x[is.finite(x)]
        if (length(x) < 2) return(0)
        sx <- sort(x, decreasing = TRUE)
        sx[2]
    }
    dt[, top__tmp := do.call(pmax, c(as.list(.SD), list(na.rm = TRUE))), .SDcols = pos_cols]
    dt[, second__tmp := apply(.SD, 1L, get_second), .SDcols = pos_cols]

    # 5) Dominance strength metrics (0 when total == 0)
    dt[, dom_share := fifelse(total__tmp > 0, top__tmp / pmax(total__tmp, 1e-12), 0)]
    dt[, dom_margin := fifelse(total__tmp > 0, (top__tmp - second__tmp) / pmax(total__tmp, 1e-12), 0)]

    # 6) Normalized entropy (0 = one PFT dominates, 1 = all equal); NA when total == 0
    eps <- 1e-12
    dt[, entropy := {
        comp <- as.matrix(.SD)
        comp[!is.finite(comp)] <- 0
        # proportions from non-negative components
        p <- comp / pmax(total__tmp, eps)
        p[p < 0] <- 0
        p[!is.finite(p)] <- 0
        H <- -rowSums(p * log(p + eps), na.rm = TRUE)
        Hn <- H / log(length(pfts))
        Hn[total__tmp == 0] <- NA_real_
        Hn
    }, .SDcols = pos_cols]

    # 7) Clean up temps
    dt[, c("top__tmp", "second__tmp", pos_cols) := NULL]
    dt[]
}

# Utility to rescale a numeric vector to [0,1] safely
.rescale01 <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) == 0) return(ifelse(is.finite(x), 1, NA_real_))
    (x - rng[1]) / diff(rng)
}

# Utility: generate n shades from a base color (lighter to base)
.vary_shades <- function(base_hex, n) {
    n <- max(1L, as.integer(n))
    pal <- grDevices::colorRampPalette(c("#ffffff", base_hex))
    # darker to lighter (or reverse if preferred)
    rev(pal(n))
}

# Build a grouped palette: pft_groups is a named list group -> character vector of PFT names
# group_base_colors is a named character vector group -> base color hex
# Returns a named character vector pft -> color hex, covering only pfts present in input list
.build_grouped_palette <- function(pfts, pft_groups, group_base_colors) {
    # Validate input
    if (is.null(pft_groups) || !length(pft_groups)) stop("pft_groups must be a non-empty named list")
    if (is.null(group_base_colors) || !length(group_base_colors)) stop("group_base_colors must be a non-empty named vector of hex colors")
    if (is.null(names(pft_groups)) || any(!nzchar(names(pft_groups)))) stop("pft_groups must be a named list")

    # Flatten mapping with shades per group
    out <- character(0)
    for (grp in names(pft_groups)) {
        members <- intersect(pft_groups[[grp]], pfts)
        if (!length(members)) next
        base_col <- group_base_colors[[grp]]
        if (is.null(base_col)) stop("Missing base color for group '", grp, "'")
        shades <- .vary_shades(base_col, length(members))
        names(shades) <- members
        out <- c(out, shades)
    }
    # Any remaining pfts not covered by groups get fallback distinct hues
    remaining <- setdiff(pfts, names(out))
    if (length(remaining)) {
        fallback <- grDevices::hcl.colors(length(remaining), palette = "Dark 3")
        names(fallback) <- remaining
        out <- c(out, fallback)
    }
    # Ensure the palette is ordered by pfts for deterministic behavior
    out[pfts]
}

# Animate dominant PFT using hue (category) and alpha (dominance strength)
# alpha_var can be "dom_share", "dom_margin", or "1 - entropy"
dave_animate_dominance <- function(src,
                                   var,
                                   pfts,
                                   title = "Dominant PFT (hue) and dominance strength (alpha)",
                                   alpha_var = c("dom_share", "dom_margin", "one_minus_entropy"),
                                   out_file = NULL,
                                   renderer = NULL,
                                   palette = NULL,
                                   pft_groups = NULL,            # named list: group -> vector of PFTs
                                   group_base_colors = NULL,     # named vector: group -> hex color
                                   palette_order = c("as_is", "frequency"),
                                   alpha_range = c(0.3, 1),
                                   width = NULL,
                                   height = NULL,
                                   units = "px",
                                   res = 96,
                                   fps = 4,
                                   nframes = NULL,
                                   end_pause = 0,
                                   show_progress = TRUE,
                                   progress_height_frac = 0.02,
                                   progress_fill = "#222222",
                                   progress_track_fill = "#DDDDDD",
                                   progress_alpha = 0.7,
                                   progress_position = c("inside", "outside"),
                                   progress_bottom_pad_frac = 0.03,
                                   bottom_margin_pt = 12,
                                   show_axis_titles = TRUE,
                                   show_axis_text = TRUE,
                                   show_axis_ticks = TRUE) {
    src <- sanitise_source(src)
    var <- sanitise_variable(var)
    fld <- read_data(src, var, read_obs = FALSE)

    data <- fld@data
    data$date <- calc_date(data)

    # Compute dominance metrics
    .compute_dominance_metrics(data, pfts)

    # Choose alpha driver
    alpha_var <- match.arg(alpha_var)
    if (alpha_var == "one_minus_entropy") {
        data$alpha_val <- 1 - data$entropy
    } else {
        data$alpha_val <- data[[alpha_var]]
    }
    data$alpha_scaled <- .rescale01(data$alpha_val)
    # Map to desired alpha range
    data$alpha_mapped <- alpha_range[1] + data$alpha_scaled * (alpha_range[2] - alpha_range[1])

    # Build a palette if not supplied
    palette_order <- match.arg(palette_order)
    # Frequency of dominance per PFT (to map the most distinct colors to most frequent)
    freq_tab <- data[is.finite(alpha_mapped) & !is.na(dominant_pft), .N, by = dominant_pft]
    setorderv(freq_tab, "N", -1)
    dom_levels_freq <- freq_tab$dominant_pft
    # All PFTs seen in data (may be subset of pfts)
    all_dom_seen <- sort(unique(data$dominant_pft))
    # Determine desired order when we are not given an explicit palette
    if (palette_order == "frequency") {
        dom_levels <- c(dom_levels_freq, setdiff(all_dom_seen, dom_levels_freq))
    } else { # "as_is": respect the order in user-supplied `pfts`
        dom_levels <- intersect(pfts, all_dom_seen)
        # Append any unexpected levels seen in data but not in pfts at the end (stable)
        dom_levels <- c(dom_levels, setdiff(all_dom_seen, dom_levels))
    }
    if (is.null(palette)) {
        if (!is.null(pft_groups)) {
            # If groups provided, attempt to build a grouped palette
            if (is.null(group_base_colors)) {
                # Provide a sensible default base color set if not supplied
                # Choose distinct hue anchors for up to 8 groups
                default_bases <- c(
                    "Trees" = "#1b9e77",    # green
                    "C3G"   = "#377eb8",    # blue
                    "C4G"   = "#e69f00",    # orange
                    "Shrubs"= "#984ea3",    # purple
                    "Other" = "#4daf4a",    # alt green
                    "Woody" = "#a65628",    # brown
                    "Herb"  = "#d95f02",    # orange-red
                    "Misc"  = "#7570b3"     # indigo
                )
                # Map any unknown group name to a cycling default base
                missing_groups <- setdiff(names(pft_groups), names(default_bases))
                if (length(missing_groups)) {
                    extra_cols <- grDevices::hcl.colors(length(missing_groups), palette = "Set 2")
                    names(extra_cols) <- missing_groups
                    group_base_colors <- c(default_bases, extra_cols)
                } else {
                    group_base_colors <- default_bases
                }
            }
            # Order PFTs by frequency when requested so the earlier (more distinct) shades/hues
            # are assigned to the most frequent members
            pfts_ordered <- if (palette_order == "frequency") {
                c(intersect(dom_levels, pfts), setdiff(pfts, dom_levels))
            } else pfts
            palette <- .build_grouped_palette(pfts_ordered, pft_groups, group_base_colors)
        } else {
            # Fall back to a large qualitative palette with strong separation
            if (requireNamespace("pals", quietly = TRUE)) {
                cols <- pals::glasbey(length(dom_levels))
            } else {
                # Fallback to a qualitative HCL palette available in base R
                cols <- grDevices::hcl.colors(length(dom_levels), palette = "Set 3")
            }
            palette <- stats::setNames(cols, dom_levels)
        }
    } else {
        # When a palette is provided, treat its names and order as canonical.
        # Use the palette's order for factor levels so comparisons across runs stay consistent.
        dom_levels <- names(palette)
        # Ensure all dom_levels are unique, non-empty strings
        if (is.null(dom_levels) || any(!nzchar(dom_levels))) stop("Provided palette must be a named vector (PFT -> color)")
        # Keep drop=FALSE later so unused levels remain visible/consistent
    }
    data$dominant_pft <- factor(data$dominant_pft, levels = dom_levels)
    # Choose time column: if daily data exists, use date; otherwise use Year (discrete)
    time_col <- if ("Day" %in% names(data)) "date" else "Year"
    frames <- sort(unique(data[[time_col]]))

    # Progress bar data (one row per unique frame)
    frames_vec <- sort(unique(data[[time_col]]))
    xr <- range(data$Lon, na.rm = TRUE)
    yr <- range(data$Lat, na.rm = TRUE)
    ybar_h <- diff(yr) * progress_height_frac
    progress_position <- match.arg(progress_position)
    if (progress_position == "inside") {
        # Inside the panel near bottom
        ybar_min <- yr[1] + progress_bottom_pad_frac * diff(yr)
        ylim_extra_low <- yr[1]
        clip_off <- FALSE
    } else {
        # Outside the panel: extend y-limits downward and disable clipping
        pad <- progress_bottom_pad_frac * diff(yr)
        ybar_min <- yr[1] - pad - ybar_h
        ylim_extra_low <- ybar_min - 0.01 * diff(yr)
        clip_off <- TRUE
    }
    if (show_progress && is.finite(ybar_h) && ybar_h > 0) {
        pb <- data.frame(
            xmin = xr[1],
            xmax = xr[1] + (0:(length(frames_vec) - 1)) / max(1L, length(frames_vec) - 1) * diff(xr),
            ymin = ybar_min,
            ymax = ybar_min + ybar_h
        )
        pb[[time_col]] <- frames_vec
        # Track layer (full-length background)
        pb_track <- data.frame(
            xmin = xr[1], xmax = xr[2], ymin = ybar_min, ymax = ybar_min + ybar_h
        )
        pb_track <- pb_track[rep(1, length(frames_vec)), , drop = FALSE]
        pb_track[[time_col]] <- frames_vec
    } else {
        pb <- NULL; pb_track <- NULL
    }

    p <- ggplot(data) +
        geom_raster(aes(x = Lon, y = Lat, fill = dominant_pft, alpha = alpha_mapped)) +
        {if (show_progress && !is.null(pb_track)) geom_rect(data = pb_track, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = progress_track_fill, alpha = progress_alpha) else NULL} +
        {if (show_progress && !is.null(pb)) geom_rect(data = pb, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = progress_fill, alpha = progress_alpha) else NULL} +
        gganimate::transition_manual(get(time_col)) +
        labs(title = title, subtitle = "Date: {current_frame}", fill = "Dominant PFT", alpha = "Dominance") +
        scale_fill_manual(values = palette, limits = dom_levels, drop = FALSE, na.value = "#000000") +
        guides(alpha = "none") +
        theme_minimal()
    # Optionally hide axes components
    if (!show_axis_titles) p <- p + theme(axis.title = element_blank())
    if (!show_axis_text)   p <- p + theme(axis.text = element_blank())
    if (!show_axis_ticks)  p <- p + theme(axis.ticks = element_blank())
    # If progress is outside, extend limits and turn off clipping, add bottom margin
    if (show_progress && progress_position == "outside") {
        p <- p + coord_cartesian(ylim = c(ylim_extra_low, yr[2]), clip = "off") +
            theme(plot.margin = margin(t = 5, r = 5, b = bottom_margin_pt, l = 5))
    }
    if (is.null(nframes)) {
        uniq_frames <- sort(unique(data[[time_col]]))
        nframes <- length(uniq_frames)
    }
    p <- gganimate::animate(p,
                            renderer = renderer,
                            width = width,
                            height = height,
                            units = units,
                            res = res,
                            fps = fps,
                            nframes = nframes,
                            end_pause = end_pause)
    if (!is.null(out_file)) {
        gganimate::anim_save(out_file)
    } else {
        return(p)
    }
}
