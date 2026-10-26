runner_shiny_fixture <- function() {
  root <- tempfile("runner-shiny-")
  jobs <- expand.grid(site = c("Alpha", "Beta"),
                      simulation = c("base", "warm"),
                      stringsAsFactors = FALSE)
  paths <- paste(jobs$site, jobs$simulation, sep = "/")
  dir.create(root)
  writeLines(paste0('simulations = [',
                    paste(sprintf('"%s"', paths), collapse = ", "), ']'),
             file.path(root, "index.toml"))
  for (i in seq_len(nrow(jobs))) {
    site <- jobs$site[i]
    simulation <- jobs$simulation[i]
    directory <- file.path(root, site, simulation)
    dir.create(file.path(directory, "out"), recursive = TRUE)
    writeLines(c(
      paste0('key = "', simulation, '"'),
      paste0('name = "', simulation, '"'),
      paste0('base_ins = "', file.path(root, paste0(site, ".ins")), '"'),
      'ins_file = "generated.ins"',
      'generated_at_utc = 2024-05-01T13:15:00Z',
      'pfts = []',
      '[[factors]]',
      'name = "warming"',
      paste0('value = "', simulation, '"')),
      file.path(directory, "manifest.toml"))
    writeLines(c('outputdirectory "out"', 'file_lai "lai.out"',
                 'file_agpp "agpp.out"'),
               file.path(directory, "generated.ins"))
    year <- if (site == "Alpha") 2000L else 2002L
    value <- i * 10L
    writeLines(c("Lon Lat Year Day stand patch Total Tree",
      sprintf("150 -33 %d 0 1 1 %d %d", year, value, value + 1L),
      sprintf("150 -33 %d 1 1 1 %d %d", year, value + 2L, value + 3L),
      sprintf("150 -33 %d 0 1 2 %d %d", year, value + 4L, value + 5L)),
      file.path(directory, "out", "lai.out"))
    writeLines(c("Lon Lat Year Total",
                 sprintf("150 -33 %d %d", year, value * 2L)),
               file.path(directory, "out", "agpp.out"))
  }
  root
}

test_that("runner catalog discovers sites, simulations, and output files", {
  root <- runner_shiny_fixture()
  withr::defer(unlink(root, recursive = TRUE))
  catalog <- runner_shiny_catalog(root)
  expect_equal(nrow(catalog), 8L)
  expect_setequal(catalog$site, c("Alpha", "Beta"))
  expect_setequal(catalog$simulation, c("base", "warm"))
  expect_setequal(catalog$quantity, c("lai", "agpp"))
  expect_equal(catalog$factors[[1L]][["warming"]], "base")
  expect_true(all(file.exists(catalog$path)))
  expect_true(all(c("Total", "Tree") %in%
                  runner_shiny_columns(catalog$path[catalog$quantity == "lai"][1L])))
})

test_that("selected runs keep independent dates, entities, and quantities", {
  root <- runner_shiny_fixture()
  withr::defer(unlink(root, recursive = TRUE))
  catalog <- runner_shiny_catalog(root)
  records <- lapply(seq_len(nrow(catalog)), function(i) {
    file <- catalog[i, , drop = FALSE]
    list(data = data.table::fread(file$path, data.table = FALSE),
         columns = if (file$quantity == "lai") c("Total", "Tree") else "Total",
         quantity = file$quantity, site = file$site,
         simulation = file$simulation, job = file$job,
         factors = file$factors[[1L]])
  })
  data <- runner_shiny_timeseries_data(records)
  expect_equal(nrow(data), 28L)
  expect_equal(length(unique(data$Series)), 20L)
  expect_setequal(unique(as.character(data$Date)), c("2000-01-01", "2000-01-02",
                                                  "2000-12-31", "2002-01-01",
                                                  "2002-01-02", "2002-12-31"))
  expect_equal(length(unique(data$Series[data$Quantity == "lai" &
                                              data$Site == "Alpha" &
                                              data$Simulation == "base"])), 4L)
  expect_equal(length(unique(data$Series[data$Quantity == "agpp"])), 4L)
  expect_true(all(is.na(data$Patch[data$Quantity == "agpp"])))
  expect_setequal(unname(runner_shiny_style_choices(data)),
                  c("constant", "Layer", "Quantity", "Site", "Simulation",
                    "Source", "Factor:warming", "Stand", "Patch", "Gridcell"))
  expect_setequal(unique(data[["Factor:warming"]]), c("base", "warm"))
})

test_that("site and simulation can independently control plot appearance", {
  root <- runner_shiny_fixture()
  withr::defer(unlink(root, recursive = TRUE))
  catalog <- runner_shiny_catalog(root)
  rows <- catalog[catalog$quantity == "agpp", , drop = FALSE]
  data <- runner_shiny_timeseries_data(lapply(seq_len(nrow(rows)), function(i) {
    file <- rows[i, , drop = FALSE]
    list(data = data.table::fread(file$path, data.table = FALSE),
         columns = "Total", quantity = file$quantity, site = file$site,
         simulation = file$simulation, job = file$job,
         factors = file$factors[[1L]])
  }))
  traces <- plotly::plotly_build(runner_shiny_timeseries_plot(
    data, "Site", "Factor:warming", "none"))$x$data
  expect_length(traces, 4L)
  expect_equal(length(unique(vapply(traces, function(x) x$line$color,
                                     character(1)))), 2L)
  expect_equal(length(unique(vapply(traces, function(x) x$line$dash,
                                     character(1)))), 2L)
  expect_true(all(vapply(traces, function(x) x$mode == "lines", logical(1))))
})

test_that("annual and subdaily outputs retain their distinct timestamps", {
  annual <- list(data = data.frame(Year = 2000, Total = 1),
                 columns = "Total", quantity = "agpp", site = "Alpha",
                 simulation = "base", job = "annual")
  subdaily <- list(data = data.frame(Year = c(2000, 2000), Day = c(0, 0),
                                   timestep = c(0, 1), Total = c(2, 3)),
                   columns = "Total", quantity = "lai", site = "Alpha",
                   simulation = "base", job = "subdaily")
  data <- runner_shiny_timeseries_data(list(annual, subdaily))
  expect_s3_class(data$Date, "POSIXct")
  expect_equal(data$Date[data$Value == 3],
               as.POSIXct("2000-01-01 12:00:00", tz = "UTC"))
  expect_equal(data$Date[data$Value == 1],
               as.POSIXct("2000-12-31 00:00:00", tz = "UTC"))
})

test_that("legend shows only differing sites and axis shows known units", {
  simulation <- "ifresptempacclimation-0_sla-20"
  records <- list(
    list(data = data.frame(Lon = 150.72, Lat = -33.62, Year = 2020,
                           total = 1), columns = "total", quantity = "aagb",
         site = "CumberlandPlain", simulation = simulation, job = "CumberlandPlain/base"),
    list(data = data.frame(Lon = 145.45, Lat = -16.1, Year = 2021,
                           total = 2), columns = "total", quantity = "aagb",
         site = "CapeTribulation", simulation = simulation, job = "CapeTribulation/base")
  )
  data <- runner_shiny_timeseries_data(records)
  plot <- plotly::plotly_build(runner_shiny_timeseries_plot(
    data, "Site", "constant", "none", "bottom", "horizontal"))
  expect_equal(vapply(plot$x$data, function(x) x$name, character(1)),
               c("CumberlandPlain", "CapeTribulation"))
  expect_equal(plot$x$layout$title,
               paste("Above-Ground Biomass", simulation, sep = " — "))
  expect_equal(plot$x$layout$yaxis$title, "AGB [kg/m2]")
  expect_equal(plot$x$layout$legend$orientation, "h")
  expect_equal(plot$x$layout$legend$yanchor, "top")
  expect_match(plot$x$data[[1L]]$text[[1L]], "150.72, -33.62", fixed = TRUE)
})

test_that("gridcell is shown only when a job contains multiple gridcells", {
  record <- list(data = data.frame(Lon = c(150, 151), Lat = c(-33, -34),
                                   Year = 2020, total = c(1, 2)),
                 columns = "total", quantity = "aagb", site = "Alpha",
                 simulation = "base", job = "Alpha/base")
  data <- runner_shiny_timeseries_data(list(record))
  plot <- plotly::plotly_build(runner_shiny_timeseries_plot(
    data, "Gridcell", "constant", "none"))
  expect_setequal(vapply(plot$x$data, function(x) x$name, character(1)),
                  c("150, -33", "151, -34"))
})

test_that("mixed or unknown units use Value and title can be overridden", {
  make_record <- function(quantity, columns) {
    frame <- data.frame(Year = 2020, PopDens = 1, FireProb = 0.5,
                        Total = 2)
    list(data = frame, columns = columns, quantity = quantity,
         site = "Alpha", simulation = "base", job = quantity)
  }
  mixed_layers <- runner_shiny_timeseries_data(list(
    make_record("dave_dsimfire", c("PopDens", "FireProb"))))
  expect_equal(timeseries_y_title(mixed_layers), "Value")
  dflux <- list(data = data.frame(Year = 2020, Veg = 1, Repr = 2),
                columns = c("Veg", "Repr"), quantity = "dflux",
                site = "Alpha", simulation = "base", job = "dflux")
  expect_equal(timeseries_y_title(runner_shiny_timeseries_data(list(dflux))),
               "C Fluxes [kgC/m2/day]")
  unknown <- runner_shiny_timeseries_data(list(make_record("unlisted", "Total")))
  expect_equal(timeseries_y_title(unknown), "Value")
  combined <- runner_shiny_timeseries_data(list(make_record("agpp", "Total"),
                                                make_record("aagb", "Total")))
  expect_equal(timeseries_y_title(combined), "Value")
  plot <- plotly::plotly_build(runner_shiny_timeseries_plot(
    combined, "Quantity", "constant", "none", title = "Factorial comparison"))
  expect_equal(plot$x$layout$title, "Factorial comparison")
  expect_equal(plot$x$layout$yaxis$title, "Value")
})

test_that("Shiny can select multiple sites, runs, and outputs together", {
  root <- runner_shiny_fixture()
  withr::defer(unlink(root, recursive = TRUE))
  app <- runner_shiny(root, run = FALSE)
  expect_s3_class(app, "shiny.appobj")
  shiny::testServer(app$serverFuncSource(), {
    session$setInputs(site = c("Alpha", "Beta"),
                      simulation = c("base", "warm"),
                      quantity = c("lai", "agpp"),
                      columns_108_97_105 = "Total",
                      columns_97_103_112_112 = "Total")
    expect_equal(nrow(selected_files()), 8L)
    data <- timeseries_data()
    expect_equal(nrow(data), 16L)
    expect_equal(length(unique(data$Series)), 12L)
  })
})
