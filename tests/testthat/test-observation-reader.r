# This file is for testing the functions in R/observation-reader.r

# Mock S4 objects are defined in 'helper-mocks.r'

test_that("Observation registry works correctly", {

    # Ensure the registry is clean before we start
    clear_observation_registry()

    # 1. Test that getting a non-existent reader throws an error
    expect_error(
        get_reader("non_existent_reader"),
        "Reader 'non_existent_reader' not found"
    )

    # 2. Register a dummy reader and retrieve it
    dummy_source <- new("Source", id = "dummy_reader")
    dummy_reader <- new("ObservationReader", src = dummy_source)
    register_reader(dummy_reader)
    retrieved_reader <- get_reader("dummy_reader")
    expect_equal(retrieved_reader, dummy_reader)

    # 3. Test that re-registering warns and overwrites
    new_dummy_source <- new("Source", id = "dummy_reader") # Same ID
    new_dummy_reader <- new("ObservationReader", src = new_dummy_source, file_path = "new/path")
    expect_output(
        register_reader(new_dummy_reader),
        "Overwriting existing observation reader with ID: dummy_reader"
    )
    retrieved_reader_after_overwrite <- get_reader("dummy_reader")
    expect_equal(retrieved_reader_after_overwrite, new_dummy_reader)

    # 4. Test clearing the registry
    clear_observation_registry()
    expect_error(get_reader("dummy_reader"))
})

test_that("create_csv_reader creates a functional reader", {

    # 1. Setup: Create a temporary directory and a dummy CSV file
    temp_dir <- withr::local_tempdir()
    dummy_file_path <- file.path(temp_dir, "my_data.csv")
    dummy_data <- data.table::data.table(
        MyDate = as.Date("2020-01-01"),
        MyLat = -35.0,
        MyLon = 148.0,
        MySite = "TestSite",
        Value1 = 10,
        Value2 = 20
    )
    data.table::fwrite(dummy_data, dummy_file_path)

    # 2. Create the reader
    csv_reader <- create_csv_reader(
        id = "test_csv",
        file_path = dummy_file_path,
        quant = "test_quant",
        layers = c("value_one" = "Value1", "value_two" = "Value2"),
        lat_col = "MyLat",
        lon_col = "MyLon",
        site_col = "MySite",
        time_col = "MyDate",
        time_fmt = "%Y-%m-%d"
    )

    # 3. Validate the reader object
    expect_s4_class(csv_reader, "ObservationReader")
    expect_equal(csv_reader@src@id, "test_csv")
    expect_equal(csv_reader@available_quantities(), "test_quant")
    expect_equal(csv_reader@available_layers(), c("value_one" = "Value1", "value_two" = "Value2"))

    # 4. Test the read_func by mocking get_field_csv
    mock_field <- new("Field", id = "mock_field")

    testthat::local_mocked_bindings(
        get_field_csv = function(source, quant, layers, target.STAInfo, file.name, lat_col, lon_col, site_col, time_col, date_fmt, sites, detect_ozflux_site, ...) {
            # Check that the arguments passed from read_func are correct
            expect_equal(source@id, "test_csv")
            expect_equal(quant@id, "test_quant")
            expect_equal(layers, c("value_one" = "Value1", "value_two" = "Value2"))
            expect_equal(target.STAInfo, NULL)
            expect_equal(file.name, dummy_file_path)
            expect_equal(lat_col, "MyLat")
            expect_equal(lon_col, "MyLon")
            expect_equal(site_col, "MySite")
            expect_equal(time_col, "MyDate")
            expect_equal(date_fmt, "%Y-%m-%d")
            expect_equal(sites, "SomeSite") # Example site passed through ...

            # Return a known object to confirm the function call worked
            return(mock_field)
        }
    )

    # Call the reader's read function
    result_field <- csv_reader@read_func("test_quant", sites = "SomeSite")

    # Check that we got an object of the correct class and ID back
    expect_s4_class(result_field, "Field")
    expect_equal(result_field@id, "mock_field")
})
