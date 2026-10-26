site_budget_fixture <- function(site = "CumberlandPlain") {
    root <- tempfile("budget repo ")
    out <- file.path(root, "benchmarks", "ozflux", site, "out")
    dir.create(out, recursive = TRUE)
    writeLines('outputdirectory "out"',
               file.path(root, "benchmarks", "ozflux", "outputs.ins"))
    writeLines("", file.path(dirname(out), paste0(site, ".ins")))
    writeLines("150.0 -33.0", file.path(dirname(out), "gridlist.txt"))
    template <- system.file("rmarkdown", "site-budget", package = "daveanalysis")
    files <- c(readLines(file.path(template, "required-outputs.txt")),
               if (site == "CumberlandPlain") "dave_swmm_100.out"
               else "dave_swavail_100.out")
    invisible(file.create(file.path(out, files)))
    list(root = root, out = out, template = template)
}

test_that("site budget preflight reports missing outputs together and accepts gzip", {
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    unlink(file.path(f$out, c("dave_lai.out", "dave_gpp.out")))
    expect_error(check_site_budget_inputs(f$root, "CumberlandPlain", f$template),
                 "dave_gpp.out\ndave_lai.out")
    file.create(file.path(f$out, c("dave_lai.out.gz", "dave_gpp.out.gz")))
    expect_silent(check_site_budget_inputs(f$root, "CumberlandPlain", f$template))
    unlink(file.path(f$out, "dave_swmm_100.out"))
    expect_error(check_site_budget_inputs(f$root, "CumberlandPlain", f$template),
                 "dave_swmm_100.out")

    other <- site_budget_fixture("Warra")
    withr::defer(unlink(other$root, recursive = TRUE))
    expect_silent(check_site_budget_inputs(other$root, "Warra", other$template))
    unlink(file.path(other$out, "dave_swavail_100.out"))
    expect_error(check_site_budget_inputs(other$root, "Warra", other$template),
                 "dave_swavail_100.out")
})

test_that("site budget validates layout and outputdirectory", {
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    ins <- file.path(f$root, "benchmarks", "ozflux", "outputs.ins")
    writeLines("! no outputdirectory", ins)
    expect_error(check_site_budget_inputs(f$root, "CumberlandPlain", f$template),
                 "one outputdirectory")
    writeLines(rep('outputdirectory "out"', 2), ins)
    expect_error(check_site_budget_inputs(f$root, "CumberlandPlain", f$template),
                 "one outputdirectory")
    unlink(file.path(dirname(f$out), "gridlist.txt"))
    expect_error(check_site_budget_inputs(f$root, "CumberlandPlain", f$template),
                 "Missing OzFlux input files")
})

test_that("wrapper rejects ambiguous settings before rendering", {
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    expect_error(render_site_budget(f$root, c("Warra", "Whroo"), "MRS"), "site must")
    expect_error(render_site_budget(f$root, "../Warra", "MRS"), "not a path")
    expect_error(render_site_budget(f$root, "CumberlandPlain", ""), "pft must")
    expect_error(render_site_budget(f$root, "CumberlandPlain", "MRS",
                                   params = list(repo = f$root)), "presentation")
    expect_error(render_site_budget(f$root, "CumberlandPlain", "MRS",
                                   params = list(dpi = 96, dpi = 120)), "presentation")
    expect_error(render_site_budget(f$root, "CumberlandPlain", "MRS",
                                   data_dir = f$root), "awra directory")
})

test_that("wrapper renders temporary assets and restores state on success", {
    skip_if_not_installed("rmarkdown")
    skip_if_not(rmarkdown::pandoc_available())
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    withr::local_dir(f$root)
    dir.create(file.path(f$root, "awra"))
    readers <- get_global("observation_readers")
    level <- get_global("log_level")
    theme <- ggplot2::theme_get()
    inputs <- character()
    environments <- list()
    testthat::local_mocked_bindings(
        render_site_budget_document = function(...) {
            args <- list(...)
            inputs <<- c(inputs, args$input)
            environments[[length(environments) + 1L]] <<- args$envir
            expect_true(file.exists(file.path(dirname(args$input),
                                              "ozflux-benchmarks.css")))
            expect_equal(readLines(args$input),
                         readLines(file.path(f$template, "site_budget.rmd")))
            expect_equal(args$params$repo, normalizePath(f$root))
            expect_equal(args$params$data_dir, normalizePath(f$root))
            expect_equal(args$params$dpi, 120)
            expect_equal(args$params$pft, "MRS")
            expect_equal(args$params$source_label, "run1")
            assign("temporary_budget_reader", TRUE,
                   envir = get_global("observation_readers"))
            set_global("log_level", 0)
            ggplot2::theme_set(ggplot2::theme_void())
            path <- file.path(args$output_dir, args$output_file)
            writeLines("<html>Budget</html>", path)
            path
        }, .package = "daveanalysis"
    )
    for (i in 1:2) {
        result <- withVisible(render_site_budget(
            f$root, "CumberlandPlain", "MRS", "reports/site budget.html",
            source_id = "run1", data_dir = f$root, params = list(dpi = 120)
        ))
        expect_false(result$visible)
        expect_equal(result$value, normalizePath("reports/site budget.html"))
        expect_identical(get_global("observation_readers"), readers)
        expect_false("temporary_budget_reader" %in% names(readers))
        expect_identical(get_global("log_level"), level)
        expect_identical(ggplot2::theme_get(), theme)
    }
    expect_false(any(file.exists(dirname(inputs))))
    expect_false(identical(environments[[1]], environments[[2]]))
})

test_that("wrapper cleans up and restores state after a render failure", {
    skip_if_not_installed("rmarkdown")
    skip_if_not(rmarkdown::pandoc_available())
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    readers <- get_global("observation_readers")
    level <- get_global("log_level")
    theme <- ggplot2::theme_get()
    input <- NULL
    testthat::local_mocked_bindings(
        render_site_budget_document = function(...) {
            args <- list(...)
            input <<- args$input
            expect_null(args$params$data_dir)
            assign("temporary_budget_reader", TRUE,
                   envir = get_global("observation_readers"))
            set_global("log_level", 0)
            ggplot2::theme_set(ggplot2::theme_void())
            stop("deliberate render failure")
        }, .package = "daveanalysis"
    )
    expect_error(render_site_budget(f$root, "CumberlandPlain", "MRS",
                                    output_file = file.path(f$root, "report.html")),
                 "deliberate render failure")
    expect_false(dir.exists(dirname(input)))
    expect_identical(get_global("observation_readers"), readers)
    expect_false("temporary_budget_reader" %in% names(readers))
    expect_identical(get_global("log_level"), level)
    expect_identical(ggplot2::theme_get(), theme)
})

test_that("wrapper works with the real renderer on a small parameterised report", {
    skip_if_not_installed("rmarkdown")
    skip_if_not(rmarkdown::pandoc_available())
    f <- site_budget_fixture("Warra")
    withr::defer(unlink(f$root, recursive = TRUE))
    testthat::local_mocked_bindings(
        render_site_budget_document = function(...) {
            args <- list(...)
            # Exercise Pandoc, parameter passing, stylesheet resolution and
            # output paths without requiring a complete scientific simulation.
            original <- readLines(args$input)
            yaml_end <- which(original == "---")[[2]]
            writeLines(c(original[seq_len(yaml_end)], "",
                         "```{r}", "cat(params$site, params$pft, params$description)",
                         "stopifnot(is.null(params$data_dir))", "```"), args$input)
            do.call(rmarkdown::render, args)
        }, .package = "daveanalysis"
    )
    path <- render_site_budget(f$root, "Warra", "MRS",
                               output_file = file.path(f$root, "reports", "budget.html"),
                               description = "integration marker", quiet = TRUE)
    expect_true(file.exists(path))
    html <- paste(readLines(path), collapse = "\n")
    expect_match(html, "Warra MRS integration marker", fixed = TRUE)
    expect_match(html, "1600px", fixed = TRUE)
    expect_false(dir.exists(file.path(dirname(path), "site_budget_files")))
})

test_that("output helper enables the manifest and preserves unrelated instructions", {
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    ins <- file.path(f$root, "benchmarks", "ozflux", "outputs.ins")
    original <- c('outputdirectory "out"', 'import "other.ins"',
                  'ifdailyoutput 0 ! daily output',
                  '  ! file_dave_lai   "" ! LAI comment',
                  'file_dave_gpp "custom.out" ! GPP comment',
                  '!file_dave_gpp "alternative.out"',
                  '!file_dave_cmass_leaf_brown "unrelated.out"',
                  '! explanatory comment')
    writeLines(original, ins)
    result <- withVisible(enable_required_outputs(f$root))
    expect_false(result$visible)
    expect_equal(result$value, normalizePath(ins))
    lines <- readLines(ins)
    expect_true(all(original[c(1, 2, 6, 7, 8)] %in% lines))
    expect_true('ifdailyoutput 1 ! daily output' %in% lines)
    expect_true('   file_dave_lai   "dave_lai.out" ! LAI comment' %in% lines)
    expect_true('file_dave_gpp "dave_gpp.out" ! GPP comment' %in% lines)
    files <- site_budget_output_files()
    expect_length(files, 100)
    for (file in files) {
        key <- paste0("file_", sub("\\.out$", "", file))
        entry <- grep(paste0("^[ \t]*", key, "[ \t]+"), lines, value = TRUE)
        expect_length(entry, 1)
        expect_match(entry, paste0('"', file, '"'), fixed = TRUE)
    }
    before <- readBin(ins, "raw", n = file.info(ins)$size)
    enable_required_outputs(f$root)
    expect_identical(readBin(ins, "raw", n = file.info(ins)$size), before)
})

test_that("output helper selects only the first commented alternative", {
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    ins <- file.path(f$root, "benchmarks", "ozflux", "outputs.ins")
    writeLines(c('outputdirectory "out"', '! file_dave_lai "first.out"',
                 '! file_dave_lai "second.out"', '! ifdailyoutput 0'), ins)
    enable_required_outputs(f$root)
    lines <- readLines(ins)
    expect_true(' file_dave_lai "dave_lai.out"' %in% lines)
    expect_true('! file_dave_lai "second.out"' %in% lines)
    expect_true(' ifdailyoutput 1' %in% lines)
})

test_that("output helper validates all changes before writing", {
    f <- site_budget_fixture()
    withr::defer(unlink(f$root, recursive = TRUE))
    ins <- file.path(f$root, "benchmarks", "ozflux", "outputs.ins")
    for (entries in list(c('file_dave_lai "a.out"', 'file_dave_lai "b.out"'),
                        'file_dave_lai unquoted.out', 'ifdailyoutput invalid')) {
        original <- c('outputdirectory "out"', entries)
        writeLines(original, ins)
        expect_error(enable_required_outputs(f$root),
                     "Multiple active entries|Cannot parse")
        expect_identical(readLines(ins), original)
    }
    expect_error(enable_required_outputs(character()), "single non-empty string")
    expect_error(enable_required_outputs(NA_character_), "single non-empty string")
    expect_error(enable_required_outputs(file.path(f$root, "missing")),
                 "Output instructions not found")
})
