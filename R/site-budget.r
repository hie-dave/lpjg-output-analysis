#' Render an OzFlux site budget
#'
#' Render the report shipped with daveanalysis without copying or editing its
#' template. Run the model first and retain the OzFlux benchmark directory
#' structure. See `vignette("site-budget", package = "daveanalysis")` for the
#' required outputs, columns and observation coverage.
#'
#' @param repo Path to the simulation repository root, containing
#'   `benchmarks/ozflux/outputs.ins` and the site directories.
#' @param site One site-directory name, for example `"CumberlandPlain"`.
#' @param pft PFT column to use for PFT-specific diagnostics, for example `"MRS"`.
#'   This is not a report-wide filter for multi-PFT simulations.
#' @param output_file HTML destination, relative to the current working directory
#'   or absolute. Parent directories are created if necessary. An existing file
#'   is overwritten. Defaults to `<site>-budget.html`.
#' @param source_id Model source identifier.
#' @param source_label Model source label used in plots.
#' @param description Text describing the model revision, configuration and run.
#' @param data_dir Optional supplementary-observation directory containing an
#'   `awra` subdirectory. Defaults to NULL, using the registered observations
#'   (including the bundled observations) without adding AWRA readers.
#' @param params Named list of presentation overrides: `aspect_ratio`,
#'   `image_fmt`, `fig_width`, `dpi`, `retina`, `fig_align`, and `out_width`.
#'   Supply simulation settings through the named arguments above.
#' @param quiet Whether to suppress rmarkdown rendering progress.
#'
#' @details The wrapper checks the directory layout and reports all missing
#'   output files together. It accepts `.out` and `.out.gz` files. These checks
#'   do not validate file contents, PFT columns or observation coverage.
#'   Standard observations, including EucFACE, are bundled with the package.
#'   Some report sections require particular observed series to be available
#'   for the selected site. The report is rendered from a temporary directory;
#'   no files are written into the installed package or simulation repository.
#'   Rendering requires the suggested package rmarkdown and Pandoc.
#'
#' @return The absolute path to the rendered HTML file, invisibly.
#' @md
#' @export
#' @examples
#' \dontrun{
#' render_site_budget(
#'   repo = "/path/to/lpj-guess", site = "CumberlandPlain", pft = "MRS",
#'   output_file = "reports/CumberlandPlain.html",
#'   description = "Model revision and simulation settings."
#' )
#' }
render_site_budget <- function(repo, site, pft,
                               output_file = paste0(site, "-budget.html"),
                               source_id = "dave", source_label = source_id,
                               description = "", data_dir = NULL,
                               params = list(), quiet = FALSE) {
    check_string <- function(value, name, allow_empty = FALSE) {
        if (!is.character(value) || length(value) != 1L || is.na(value) ||
            (!allow_empty && !nzchar(trimws(value)))) {
            stop(name, " must be a single ",
                 if (allow_empty) "" else "non-empty ",
                 "string.", call. = FALSE)
        }
    }
    check_string(repo, "repo")
    check_string(site, "site")
    check_string(pft, "pft")
    check_string(output_file, "output_file")
    check_string(source_id, "source_id")
    check_string(source_label, "source_label")
    check_string(description, "description", allow_empty = TRUE)
    if (site %in% c(".", "..") || grepl("[/\\\\]", site)) {
        stop("site must be one site-directory name, not a path.", call. = FALSE)
    }
    if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
        stop("quiet must be TRUE or FALSE.", call. = FALSE)
    }
    presentation <- c("aspect_ratio", "image_fmt", "fig_width", "dpi", "retina",
                      "fig_align", "out_width")
    if (!is.list(params) || (length(params) > 0L &&
        (is.null(names(params)) || anyNA(names(params)) ||
         any(!names(params) %in% presentation) || anyDuplicated(names(params))))) {
        stop("params must be a named list of presentation overrides: ",
             paste(presentation, collapse = ", "), ".", call. = FALSE)
    }
    repo <- path.expand(repo)
    if (!dir.exists(repo)) {
        stop("Simulation repository does not exist: ", repo, call. = FALSE)
    }
    repo <- normalizePath(repo, winslash = "/", mustWork = TRUE)
    if (!is.null(data_dir)) {
        check_string(data_dir, "data_dir")
        data_dir <- path.expand(data_dir)
        if (!dir.exists(file.path(data_dir, "awra"))) {
            stop("data_dir must contain an awra directory: ", data_dir,
                 call. = FALSE)
        }
        data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
    }
    template_dir <- system.file("rmarkdown", "site-budget",
                                package = "daveanalysis", mustWork = TRUE)
    check_site_budget_inputs(repo, site, template_dir)

    if (!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("Install rmarkdown to render a site budget.", call. = FALSE)
    }
    if (!rmarkdown::pandoc_available()) {
        stop("Pandoc is required to render a site budget. Install Pandoc or use RStudio.",
             call. = FALSE)
    }
    output_file <- path.expand(output_file)
    if (!grepl("\\.html$", output_file, ignore.case = TRUE)) {
        stop("output_file must end in .html.", call. = FALSE)
    }
    output_dir <- dirname(output_file)
    if (!dir.exists(output_dir) &&
        !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
        stop("Could not create report directory: ", output_dir, call. = FALSE)
    }
    output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

    work_dir <- tempfile("site-budget-")
    dir.create(work_dir)
    on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)
    assets <- c("site_budget.rmd", "ozflux-benchmarks.css")
    if (!all(file.copy(file.path(template_dir, assets), work_dir))) {
        stop("Could not copy the installed site budget template.", call. = FALSE)
    }

    # Report setup changes package readers, logging and the ggplot theme. Keep
    # those changes local to this render, including when a chunk fails.
    readers <- get_global("observation_readers")
    log_level <- get_global("log_level")
    theme <- ggplot2::theme_get()
    on.exit({
        set_global("observation_readers", readers)
        set_global("log_level", log_level)
        ggplot2::theme_set(theme)
    }, add = TRUE)
    set_global("observation_readers",
               list2env(as.list(readers), parent = emptyenv()))

    render_params <- c(list(repo = repo, site = site, pft = pft,
                            source_id = source_id, source_label = source_label,
                            description = description, data_dir = data_dir), params)
    result <- render_site_budget_document(
        input = file.path(work_dir, "site_budget.rmd"),
        params = render_params,
        output_file = basename(output_file), output_dir = output_dir,
        knit_root_dir = work_dir, envir = new.env(parent = globalenv()),
        quiet = quiet
    )
    invisible(normalizePath(result, winslash = "/", mustWork = TRUE))
}

# Kept separate so tests can exercise the wrapper without running a full model
# analysis. Production always delegates to rmarkdown's renderer.
render_site_budget_document <- function(...) {
    rmarkdown::render(...)
}

check_site_budget_inputs <- function(repo, site, template_dir) {
    ozflux <- file.path(repo, "benchmarks", "ozflux")
    ins <- file.path(ozflux, "outputs.ins")
    layout <- c(ins, file.path(ozflux, site, paste0(site, ".ins")),
                file.path(ozflux, site, "gridlist.txt"))
    missing <- layout[!file.exists(layout)]
    if (length(missing)) {
        stop("Missing OzFlux input files:\n", paste(missing, collapse = "\n"),
             call. = FALSE)
    }
    out_dir <- get_output_dir(ins)
    if (length(out_dir) != 1L || !nzchar(out_dir)) {
        stop("Expected one outputdirectory setting in ", ins, call. = FALSE)
    }
    files <- site_budget_output_files(site, template_dir)
    paths <- file.path(ozflux, site, out_dir, files)
    missing <- files[!file.exists(paths) & !file.exists(paste0(paths, ".gz"))]
    if (length(missing)) {
        stop("Missing model outputs for ", site, " in ",
             file.path(ozflux, site, out_dir), ":\n",
             paste(missing, collapse = "\n"), call. = FALSE)
    }
    invisible(NULL)
}

#' Enable the model outputs required by the site budget
#'
#' Update the OzFlux output instructions before running LPJ-GUESS. Uses the
#' same installed output manifest as [render_site_budget()].
#'
#' @param repo Path to the simulation repository root containing
#'   `benchmarks/ozflux/outputs.ins`.
#'
#' @details Edits `outputs.ins` in place. Enables all common site-budget
#'   outputs and both site-dependent soil-moisture outputs, so the configuration
#'   works for multiple sites. Existing settings are uncommented as needed and
#'   missing settings are appended. Required output filenames are set to the
#'   names expected by the report, replacing custom or empty filenames.
#'   `ifdailyoutput` is set to `1`. Unrelated settings, `outputdirectory`, and
#'   trailing comments are retained. Imported instruction files are not edited.
#'
#'   When a setting already has an active entry, commented alternatives are
#'   left alone; otherwise the first commented entry is enabled. Multiple
#'   active entries or an unrecognised value syntax for a required setting
#'   cause an error before the file is written. Calling the helper again on
#'   an already configured file makes no changes.
#'
#'   This configures output only: it does not run LPJ-GUESS or create model
#'   results. The model version must support the requested diagnostics, and
#'   the run's instruction files must import this `outputs.ins` without later
#'   overriding these settings.
#'
#' @return The absolute path to the updated instruction file, invisibly.
#' @seealso [render_site_budget()]
#' @md
#' @export
#' @examples
#' \dontrun{
#' enable_required_outputs("/path/to/lpj-guess")
#' # Run the model, then render_site_budget(...).
#' }
enable_required_outputs <- function(repo) {
    if (!is.character(repo) || length(repo) != 1L || is.na(repo) ||
        !nzchar(trimws(repo))) {
        stop("repo must be a single non-empty string.", call. = FALSE)
    }
    ins <- file.path(path.expand(repo), "benchmarks", "ozflux", "outputs.ins")
    if (!file.exists(ins) || dir.exists(ins)) {
        stop("Output instructions not found: ", ins, call. = FALSE)
    }
    ins <- normalizePath(ins, winslash = "/", mustWork = TRUE)
    original <- readLines(ins, warn = FALSE)
    lines <- original
    files <- site_budget_output_files()
    settings <- setNames(paste0('"', files, '"'),
                         paste0("file_", sub("\\.out$", "", files)))
    settings <- c(settings, ifdailyoutput = "1")
    for (key in names(settings)) {
        # Match whole parameter names, including commented alternatives.
        pattern <- paste0("^[ \t]*!?[ \t]*", key, "([ \t]|$)")
        candidates <- grep(pattern, lines)
        active <- candidates[!grepl("^[ \t]*!", lines[candidates])]
        if (length(active) > 1L) {
            stop("Multiple active entries for ", key, " in ", ins,
                 ". Resolve these before enabling outputs.", call. = FALSE)
        }
        if (!length(candidates)) {
            lines <- c(lines, paste(key, settings[[key]]))
            next
        }
        index <- if (length(active)) active[[1]] else candidates[[1]]
        value_pattern <- if (key == "ifdailyoutput") "[01]" else '"[^"]*"'
        full_pattern <- paste0("^([ \t]*)!?([ \t]*)", key,
                               "[ \t]+", value_pattern, "([ \t]*(!.*)?)$")
        matched <- regmatches(lines[index], regexec(full_pattern, lines[index]))[[1]]
        if (!length(matched)) {
            stop("Cannot parse ", key, " on line ", index, " of ", ins,
                 ". Expected ", if (key == "ifdailyoutput") "0 or 1"
                 else "a quoted filename", ".", call. = FALSE)
        }
        # Keep alignment and trailing comments while removing a comment marker.
        enabled <- sub("^([ \t]*)!", "\\1", lines[index])
        lines[index] <- sub(paste0("(", key, "[ \t]+)", value_pattern),
                            paste0("\\1", settings[[key]]), enabled)
    }
    if (!identical(lines, original)) {
        # Stage the complete result first, so validation errors cannot leave a
        # partially edited instruction file.
        staged <- tempfile("site-budget-outputs-", tmpdir = dirname(ins))
        on.exit(unlink(staged), add = TRUE)
        writeLines(lines, staged)
        if (!file.copy(staged, ins, overwrite = TRUE)) {
            stop("Could not update output instructions: ", ins, call. = FALSE)
        }
    }
    invisible(ins)
}

site_budget_output_files <- function(site = NULL, template_dir = system.file(
        "rmarkdown", "site-budget", package = "daveanalysis", mustWork = TRUE)) {
    files <- readLines(file.path(template_dir, "required-outputs.txt"))
    soil <- if (is.null(site)) c("dave_swmm_100.out", "dave_swavail_100.out")
            else if (site == "CumberlandPlain") "dave_swmm_100.out"
            else "dave_swavail_100.out"
    c(files, soil)
}
