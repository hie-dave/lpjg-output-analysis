custom_ozflux_fixture <- function(site = "MyCustomSite") {
  root <- tempfile("ozflux-custom-")
  site_dir <- file.path(root, "benchmarks", "ozflux", site)
  dir.create(file.path(site_dir, "out"), recursive = TRUE)
  writeLines('outputdirectory "out"',
             file.path(root, "benchmarks", "ozflux", "outputs.ins"))
  writeLines('import "../outputs.ins"', file.path(site_dir, paste0(site, ".ins")))
  writeLines("150 -33", file.path(site_dir, "gridlist.txt"))
  writeLines(c("Lon Lat Year Day Total", "150 -33 2020 1 2"),
             file.path(site_dir, "out", "gpp.out"))
  list(root = root, site = site)
}

test_that("Shiny offers a custom OZFLUX site without a special flag", {
  fixture <- custom_ozflux_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE))
  source <- DGVMTools::defineSource("custom", "Custom", fixture$root, OZFLUX)

  expect_false(fixture$site %in% read_ozflux_sites()$Name)
  expect_length(get_sites_ozflux(source), 0)
  expect_equal(get_sites_ozflux(source, include_custom_sites = TRUE), fixture$site)
  sites <- get_available_sites_ozflux(list(source), include_custom_sites = TRUE)
  expect_equal(sites$Name, fixture$site)
  expect_equal(sites$Lon, 150)
  expect_equal(sites$Lat, -33)

  testthat::local_mocked_bindings(
    shinyApp = function(ui, server) list(ui = ui, server = server),
    runApp = function(app, ...) app,
    reactivePoll = function(intervalMillis, session, checkFunc, valueFunc) {
      shiny::reactive(valueFunc())
    },
    .package = "daveanalysis"
  )
  app <- ozflux_shiny(source)
  html <- htmltools::renderTags(app$ui)$html
  expect_match(html, 'value="MyCustomSite"', fixed = TRUE)
  expect_match(html, 'value="gpp"', fixed = TRUE)

  shiny::testServer(app$server, {
    session$setInputs(site = fixture$site, var = "gpp",
                      layers_103_112_112 = "Total",
                      do_timeseries = TRUE, do_pvo = FALSE,
                      do_subannual = FALSE, colour_by = "constant",
                      line_by = "constant", point_by = "none")
    expect_equal(timeseries_data()$Value, 2)
    expect_equal(timeseries_data()$Quantity, "gpp")
  })
})

test_that("Shiny still validates source formats independently of site names", {
  fixture <- custom_ozflux_fixture()
  withr::defer(unlink(fixture$root, recursive = TRUE))
  ozflux <- DGVMTools::defineSource("custom", "Custom", fixture$root, OZFLUX)
  other_dir <- tempfile()
  dir.create(other_dir)
  withr::defer(unlink(other_dir, recursive = TRUE))
  other <- DGVMTools::defineSource("other", "Other", other_dir, DGVMTools::GUESS)

  testthat::local_mocked_bindings(
    shinyApp = function(ui, server) list(ui = ui, server = server),
    runApp = function(app, ...) app,
    .package = "daveanalysis"
  )
  expect_error(ozflux_shiny(list(ozflux, other)), "requires OZFLUX sources")
  expect_warning(app <- ozflux_shiny(list(ozflux, other),
                                    allow_unrecognised = TRUE), "Ignoring 1 non-OZFLUX")
  expect_match(htmltools::renderTags(app$ui)$html, fixture$site, fixed = TRUE)
})

test_that("secondary plots read the source that contains a custom site", {
  first <- custom_ozflux_fixture("FirstCustomSite")
  second <- custom_ozflux_fixture("SecondCustomSite")
  withr::defer(unlink(first$root, recursive = TRUE))
  withr::defer(unlink(second$root, recursive = TRUE))
  sources <- list(
    DGVMTools::defineSource("first", "First", first$root, OZFLUX),
    DGVMTools::defineSource("second", "Second", second$root, OZFLUX)
  )
  used_sources <- character(0)

  testthat::local_mocked_bindings(
    shinyApp = function(ui, server) list(ui = ui, server = server),
    runApp = function(app, ...) app,
    read_data = function(sources, ...) {
      used_sources <<- vapply(sources, function(source) source@id, character(1))
      "read"
    },
    .package = "daveanalysis"
  )
  app <- ozflux_shiny(sources)
  get_data <- get("get_data", envir = environment(app$server))
  expect_equal(get_data("gpp", second$site, "Total"), "read")
  expect_equal(used_sources, "second")
})
