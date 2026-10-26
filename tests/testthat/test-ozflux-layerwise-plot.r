test_that("layerwise plot helper facets and groups entity-aware data", {
    data <- data.table::data.table(
        Lon = 1,
        Lat = 2,
        Year = 2000,
        Day = rep(1:4, 4),
        date = as.Date("2000-01-01") + rep(1:4, 4),
        source = rep(c("a", "b"), each = 8),
        quantity = "q",
        variable = rep(c("L1", "L2"), each = 4, times = 2),
        stand = 1,
        patch = rep(c(1, 2), each = 2, times = 4),
        value = seq_len(16)
    )

    plot <- plot_layerwise_table(
        data,
        facet_by = "variable",
        colour_by = "source",
        linetype_by = "entity",
        entity_layout = "series",
        geometry = "line"
    )

    expect_s3_class(plot, "ggplot")
    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$layout$layout), 2)
    expect_true("linetype" %in% names(built$data[[1]]))
})

test_that("layerwise plot helper can facet by entity", {
    data <- data.table::data.table(
        Lon = 1,
        Lat = 2,
        Year = 2000,
        Day = rep(1:3, 2),
        date = as.Date("2000-01-01") + rep(1:3, 2),
        source = "a",
        quantity = "q",
        variable = "L1",
        stand = 1,
        patch = rep(c(1, 2), each = 3),
        value = seq_len(6)
    )

    plot <- plot_layerwise_table(
        data,
        facet_by = "none",
        colour_by = "none",
        entity_layout = "facet",
        geometry = "line"
    )

    built <- ggplot2::ggplot_build(plot)
    expect_equal(nrow(built$layout$layout), 2)
})
