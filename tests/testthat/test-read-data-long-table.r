test_that("field_to_long_table preserves individual and PFT columns as identifiers", {
    field <- new("Field",
        id = "hydraulics",
        data = data.table::data.table(
            Lon = 150,
            Lat = -34,
            Year = 2014,
            Day = 0,
            stand = 1,
            patch = 1,
            layer = 2,
            indiv = 42,
            pft = "TeBE",
            kmax = 1.2,
            lai = 3.4
        )
    )
    metadata <- data.table::data.table(
        field_layer = c("kmax", "lai"),
        source = "src",
        quantity = "dave_hydraulics",
        variable = c("kmax", "lai")
    )

    out <- field_to_long_table(field, metadata)

    expect_equal(sort(unique(out$variable)), c("kmax", "lai"))
    expect_equal(out$layer, c(2, 2))
    expect_equal(out$indiv, c(42, 42))
    expect_equal(out$pft, c("TeBE", "TeBE"))
    expect_equal(out$date, as.Date(c("2014-01-01", "2014-01-01")))
    expect_type(out$value, "double")
    expect_false("2" %in% out$variable)
    expect_false("pft" %in% out$variable)
    expect_false("indiv" %in% out$variable)
})

test_that("field_to_long_table dates annual and leap-year daily records", {
    metadata <- data.table::data.table(
        field_layer = "Total", source = "src", quantity = "q",
        variable = "Total"
    )
    annual <- new("Field", id = "annual", data = data.table::data.table(
        Year = c(2019, 2020), Total = c(1, 2)
    ))
    daily <- new("Field", id = "daily", data = data.table::data.table(
        Year = c(2020, 2020, 2020, 2021), Day = c(0, 59, 364, 364),
        Total = c(1, 2, 3, 4)
    ))

    expect_equal(field_to_long_table(annual, metadata)$date,
                 as.Date(c("2019-12-31", "2020-12-31")))
    expect_equal(field_to_long_table(daily, metadata)$date,
                 as.Date(c("2020-01-01", "2020-02-29", "2020-12-30",
                           "2021-12-31")))
})

test_that("field_to_long_table preserves time of day for subdaily records", {
    field <- new("Field", id = "subdaily", data = data.table::data.table(
        Year = 2020, Day = c(0, 0, 0, 59, 365),
        timestep = c(0, 1, 47, 24, 47), Total = 1:5
    ))
    metadata <- data.table::data.table(
        field_layer = "Total", source = "src", quantity = "q",
        variable = "Total"
    )

    out <- field_to_long_table(field, metadata)

    expect_s3_class(out$date, "POSIXct")
    expect_equal(out$date, as.POSIXct(c(
        "2020-01-01 00:00:00", "2020-01-01 00:30:00",
        "2020-01-01 23:30:00", "2020-02-29 12:00:00",
        "2020-12-31 23:30:00"
    ), tz = "UTC"))
})
