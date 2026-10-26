record <- function(data, quantity = "gpp", layers = "Total", source = "model") {
  list(data = data, quantity = quantity, layers = layers, source = source)
}

test_that("dates and full entity identities survive alignment and shuffled rows", {
  a <- data.frame(Year = 2020, Day = c(365, 0, 0), stand = c(1, 2, 1),
                  patch = 1, indiv = 1, Total = c(30, 20, 10))
  b <- a[c(3, 1, 2), ]
  b$Total <- c(100, 300, 200)
  out <- ozflux_timeseries_data(list(record(a), record(b, "npp")))
  expect_equal(nrow(out), 6L)
  expect_equal(length(unique(out$Series)), 4L)
  match_keys <- c("Date", "Entity", "Source", "Layer")
  paired <- merge(out[out$Quantity == "gpp", ], out[out$Quantity == "npp", ],
                  by = match_keys)
  expect_equal(paired$Value.y, 10 * paired$Value.x)
  expect_equal(out$Date[out$Value == 30], as.Date("2020-12-31"))
})

test_that("mixed aggregation and annual outputs are not broadcast or dropped", {
  annual <- data.frame(Year = c(2019, 2020), Total = c(7, 8))
  patch <- data.frame(Year = 2020, Day = c(0, 365, 365),
                      stand = c(1, 1, 2), patch = 1, Total = 1:3)
  out <- ozflux_timeseries_data(list(record(annual), record(patch, "npp")))
  expect_equal(nrow(out), 5L)
  expect_equal(out$Date[out$Entity == "Gridcell"],
               as.Date(c("2019-12-31", "2020-12-31")))
  expect_equal(length(unique(out$Series)), 3L)
})

test_that("columns and sources stay distinct and missing values survive", {
  frame <- data.frame(Year = 2020:2021, Total = c(1, NA), Tree = c(2, 3))
  out <- ozflux_timeseries_data(list(record(frame, layers = c("Total", "Tree")),
                                    record(frame, source = "other")))
  expect_equal(nrow(out), 6L)
  expect_equal(length(unique(out$Series)), 3L)
  expect_equal(sum(is.na(out$Value)), 2L)
  expect_error(ozflux_timeseries_data(list(record(rbind(frame, frame)))),
               "Duplicate date/entity")
  expect_error(ozflux_timeseries_data(list(record(
    data.frame(Year = 2020, indiv = 1, Total = 1)))), "Missing entity")
})

test_that("plot styles support lines only, points only and entity variation", {
  out <- ozflux_timeseries_data(list(record(data.frame(
    Year = 2020:2021, stand = c(1, 2), Total = 1:2))))
  for (styles in list(c("Layer", "constant", "none"),
                      c("Quantity", "none", "Stand"),
                      c("Stand", "Layer", "Quantity"))) {
    plot <- ozflux_timeseries_plot(out, styles[1], styles[2], styles[3], "Site")
    traces <- plotly::plotly_build(plot)$x$data
    expect_length(traces, 2)
    expected <- if (styles[2] == "none") "markers" else
      if (styles[3] == "none") "lines" else "lines+markers"
    expect_true(all(vapply(traces, function(x) x$mode == expected, logical(1))))
  }
})

test_that("Ozflux legend omits common fields and uses output metadata", {
  a <- record(data.frame(Year = 2020, Total = 1), "aagb", "Total", "model-a")
  b <- record(data.frame(Year = 2020, Total = 2), "aagb", "Total", "model-b")
  data <- ozflux_timeseries_data(list(a, b))
  plot <- plotly::plotly_build(ozflux_timeseries_plot(
    data, "Source", "constant", "none", "Site", "top", "horizontal"))
  expect_equal(vapply(plot$x$data, function(x) x$name, character(1)),
               c("model-a", "model-b"))
  expect_equal(plot$x$layout$title, "Above-Ground Biomass — Site")
  expect_equal(plot$x$layout$yaxis$title, "AGB [kg/m2]")
  expect_equal(plot$x$layout$legend$orientation, "h")
  expect_equal(plot$x$layout$legend$yanchor, "bottom")

  one <- plotly::plotly_build(ozflux_timeseries_plot(
    ozflux_timeseries_data(list(a)), "constant", "constant", "none", "Site"))
  expect_false(one$x$data[[1L]]$showlegend)
})

test_that("hierarchy style choices follow the available aggregation levels", {
  frame <- data.frame(Year = 2020, Total = 1)
  for (level in 0:3) {
    if (level > 0) frame[[c("stand", "patch", "indiv")[level]]] <- 1
    out <- ozflux_timeseries_data(list(record(frame)))
    expect_equal(unname(ozflux_entity_choices(out)),
                 c("Stand", "Patch", "Cohort")[seq_len(level)])
  }
})

test_that("hierarchy styles group descendants and disambiguate repeated IDs", {
  frame <- data.frame(Year = 2020, stand = c(1, 1, 1, 2),
    patch = c(1, 1, 2, 1), indiv = c(1, 2, 1, 1), Total = 1:4)
  out <- ozflux_timeseries_data(list(record(frame), record(
    data.frame(Year = 2020, Total = 10), "npp")))
  cohorts <- out[out$Quantity == "gpp", ]
  expect_equal(length(unique(cohorts$Stand)), 2L)
  expect_equal(length(unique(cohorts$Patch)), 3L)
  expect_equal(length(unique(cohorts$Cohort)), 4L)
  expect_equal(length(unique(cohorts$Series)), 4L)
  expect_true(all(is.na(out[out$Quantity == "npp", c("Stand", "Patch", "Cohort")])))
  plot <- plotly::plotly_build(ozflux_timeseries_plot(
    out, "Stand", "Patch", "Cohort", "Site"))
  traces <- plot$x$data
  expect_length(traces, 5L)
  model_traces <- traces[vapply(traces, function(x) startsWith(x$name, "gpp /"), logical(1))]
  expect_equal(length(unique(vapply(model_traces, function(x) x$line$color, character(1)))), 2L)
  expect_equal(length(unique(vapply(model_traces, function(x) x$line$dash, character(1)))), 3L)
  expect_equal(length(unique(vapply(model_traces, function(x) x$marker$symbol, character(1)))), 4L)
})

test_that("fresh file snapshots exclude stale outputs and detect changes", {
  directory <- tempfile()
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE))
  files <- file.path(directory, c("gpp.out", "npp.out.gz", "lai.out"))
  file.create(files)
  timestamp <- as.POSIXct("2020-01-01", tz = "UTC")
  Sys.setFileTime(files[1], timestamp)
  Sys.setFileTime(files[2], timestamp + 10)
  Sys.setFileTime(files[3], timestamp - 1)
  expect_setequal(ozflux_fresh_output_files(directory), files[1:2])
  Sys.setFileTime(files[3], timestamp + 21)
  expect_equal(ozflux_fresh_output_files(directory), files[3])
  unlink(files)
  expect_length(ozflux_fresh_output_files(directory), 0)
})
