test_that("append_required_metadata_layers adds detailed metadata when present", {
    md <- create_output_metadata(
        id = "file_test",
        name = "Test",
        description = "Test",
        aggregation_level = "patch",
        temporal_resolution = "daily"
    )

    testthat::local_mocked_bindings(
        resolve_quantity_metadata = function(var_id, layer = NULL) {
            list(metadata = md)
        }
    )

    out <- append_required_metadata_layers(
        "test",
        "gpp",
        c("Lon", "Lat", "Year", "Day", "stand", "patch", "gpp")
    )

    expect_equal(out, c("gpp", "stand", "patch"))
})

test_that("append_required_metadata_layers keeps legacy behavior when metadata missing in file", {
    md <- create_output_metadata(
        id = "file_test",
        name = "Test",
        description = "Test",
        aggregation_level = "patch",
        temporal_resolution = "daily"
    )

    testthat::local_mocked_bindings(
        resolve_quantity_metadata = function(var_id, layer = NULL) {
            list(metadata = md)
        }
    )

    out <- append_required_metadata_layers(
        "test",
        "gpp",
        c("Lon", "Lat", "Year", "Day", "gpp")
    )

    expect_equal(out, "gpp")
})

test_that("append_required_metadata_layers preserves null and avoids duplicates", {
    md <- create_output_metadata(
        id = "file_test",
        name = "Test",
        description = "Test",
        aggregation_level = "patch",
        temporal_resolution = "daily"
    )

    testthat::local_mocked_bindings(
        resolve_quantity_metadata = function(var_id, layer = NULL) {
            list(metadata = md)
        }
    )

    expect_null(append_required_metadata_layers("test", NULL, character(0)))

    out <- append_required_metadata_layers(
        "test",
        c("gpp", "patch"),
        c("Lon", "Lat", "Year", "Day", "stand", "patch", "gpp")
    )

    expect_equal(out, c("gpp", "patch", "stand"))
})
