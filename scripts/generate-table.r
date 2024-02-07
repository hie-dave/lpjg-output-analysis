library("htmltools")
library("htmlTable")
library("daveanalysis")
library("RColorBrewer")
library("xtable")

get_predicted_name <- function(gc, obs_lyr) {
    names <- names(gc)
    if (length(names) == 0) {
        log_error("Unable to plot: data contains no layers")
    }
    if (length(names) == 1) {
        predicted_name <- names[1]
    } else {
        if (names[1] == obs_lyr) {
            predicted_name <- names[2]
        } else {
            predicted_name <- names[1]
        }
    }

    predicted_name <- names(gc)[[length(names(gc))]]
    return(predicted_name)
}

get_data_var <- function(sources, sites, var) {

    # Sanitise variables to be plotted.
    var <- daveanalysis:::sanitise_variable(var)

    # Read data for this gridcell.
    data <- daveanalysis:::read_data(list(var), sources, site = sites)

    nsite <- nrow(sites)
    result <- list()
    for (i in seq_len(nsite)) {
        row <- sites[i, ]

        gc <- daveanalysis:::get_gridcell(data, row$Lat, row$Lon, row$Name)
        obs_lyr <- daveanalysis:::get_global("obs_lyr")
        obs <- gc@data[, ..obs_lyr]

        predicted_name <- get_predicted_name(gc, obs_lyr)
        pred <- gc@data[, ..predicted_name]

        result[[row$Name]] <- daveanalysis:::compute_r2(obs, pred)
    }
    return(result)
}

get_data <- function(sources, vars, sites = NULL) {

    # Sanitise sites to be plotted.
    sites <- daveanalysis:::sanitise_ozflux_sites(sites)

    # Sanitise data sources.
    sources <- daveanalysis:::sanitise_sources(sources)

    # Get data for one variable at a time.
    data <- list()
    for (var in vars) {
        data[[var]] <- get_data_var(sources, sites, var)
    }
    return(data)
}

get_text_colour <- function(x) {
    return(ifelse(x > 0.25, "black", "white"))
}

get_colour <- function(x, palette, granularity) {
    colours <- colorRampPalette(brewer.pal(11, palette))(granularity)
    return(colours[ceiling(as.double(x) * (granularity - 1) + 1)])
}

apply_colour <- function(x, palette, granularity) {
    bg <- get_colour(x, palette, granularity)
    bg <- sub("#", "", bg)
    fg <- get_text_colour(x)
    style_bg <- sprintf("\\cellcolor[HTML]{%s}", bg)
    style_fg <- sprintf("\\color{%s}", fg)
    return(sprintf("%s %s %.2f", style_bg, style_fg, x))
}

get_css <- function(x, palette, granularity) {
    bg <- get_colour(x, palette, granularity)
    fg <- get_text_colour(x)
    css <- sprintf("background-color: %s; color: %s;", bg, fg)
    return(css)
}

generate_pdf <- function(source, vars, colour_scheme, out_file
    , html_output, granularity = 100) {
    # Get data.
    dave_config(log_level = 0)
    data <- get_data(source, vars)
    table <- as.data.frame(do.call(rbind, data))
    table <- t(table)
    if (html_output) {
        css_table <- apply(table, c(1, 2), get_css
            , palette = colour_scheme, granularity = granularity)
        caption <- "r2"
        table <- apply(table, c(1, 2), function(x) sprintf("%.2f", x))
        table <- htmlTable(table, css.cell = css_table)
        print(h1(caption))
        print(table)
    } else {
        coloured <- apply(table, c(1, 2), apply_colour, palette = colour_scheme
            , granularity = granularity)
        table_x <- xtable(coloured)
        print(table_x, sanitize.text.function = identity)
    }
}

trunk <- "~/code/lpj-guess/output-analysis/trunk"
vars <- c(
    "dave_lai",
    "dave_gpp",
    "dave_resp",
    "dave_nee",
    "dave_et"
)

colour_scheme <- "RdYlGn"
out_file <- "ozflux-trunk-r2.pdf"

generate_pdf(trunk, vars, colour_scheme, out_file, html_output = TRUE)
