# This file is for testing the functions in R/format-csv.r

# NOTE: When running tests interactively (e.g. line-by-line), you may need to
# run `devtools::load_all()` first to make sure all your package functions are loaded.
# Mock S4 objects are defined in 'helper-mocks.r'

test_that("get_site_names_by_coord correctly identifies sites by coordinates", {
    # 1. Setup: Create a sample data table of sites
    site_data <- data.table::data.table(
        Name = c("SiteA", "SiteB", "SiteC"),
        Lat = c(-35.0, -36.0, -37.0),
        Lon = c(148.0, 149.0, 150.0)
    )

    # 2. Define test coordinates
    test_lats <- c(-35.0, -36.05, -40.0, -35.0000001)
    test_lons <- c(148.0, 149.05, 150.0, 148.0000001)

    # 3. Run the vectorized function
    site_names <- get_site_names_by_coord(test_lats, test_lons, all_sites = site_data)

    # 4. Check the results
    # Expect: Exact match, close match, no match (NA), and near-exact match
    expected_names <- c("SiteA", "SiteB", NA, "SiteA")
    expect_equal(site_names, expected_names)

    # Test with empty input
    expect_equal(get_site_names_by_coord(numeric(0), numeric(0), site_data), character(0))
})

test_that("available_quantities_csv finds csv files", {

    # Create a temporary directory and some dummy files.
    temp_dir <- withr::local_tempdir()

    # Create dummy files
    # TODO: test with .gz files
    file.create(file.path(temp_dir, "gpp.csv"))
    file.create(file.path(temp_dir, "nee.csv"))
    file.create(file.path(temp_dir, "notes.txt")) # Should be ignored

    # Create a mock source object pointing to our temp directory
    mock_source <- new("Source", dir = temp_dir, id = "test_source")

    # Run the function
    quantities <- available_quantities_csv(mock_source)

    # Check the results
    expect_equal(sort(quantities), c("gpp", "nee"))
})

test_that("available_layers_csv reads header correctly", {
    # Create a temporary directory and a dummy CSV file.
    temp_dir <- withr::local_tempdir()

    # Define the headers we expect to find.
    expected_headers <- c("Time", "SiteA", "SiteB", "SiteC")

    # Create an empty data.table with these headers and write it to a file.
    # The function only reads the header, so the file content doesn't matter.
    dummy_data <- data.table::as.data.table(
        setNames(replicate(length(expected_headers),
                           numeric(0),
                           simplify = FALSE),
                 expected_headers)
    )
    name <- "test_quantity"
    file.create(file.path(temp_dir, paste0(name, ".csv")))
    data.table::fwrite(dummy_data, file.path(temp_dir, paste0(name, ".csv")))

    # Create mock objects that will allow the function to find our dummy file.
    mock_source <- new("Source", dir = temp_dir)
    mock_quant <- new("Quantity", id = name)

    # Run the function.
    layers <- available_layers_csv(mock_source, mock_quant)

    # Assert that the returned layers match the headers in our dummy file.
    expect_equal(layers, expected_headers)
})

test_that("get_field_csv reads and filters data correctly", {
    # 1. Setup: Create a temporary directory and a dummy CSV file with data.
    temp_dir <- withr::local_tempdir()

    dummy_data <- data.table::data.table(
        date = as.Date(c("2000-01-01", "2000-01-02", "2000-01-01", "2000-01-02")),
        Lat = c(-35.0, -35.0, -36.0, -36.0),
        Lon = c(148.0, 148.0, 149.0, 149.0),
        GPP = c(1.1, 1.2, 2.1, 2.2),
        NPP = c(0.5, 0.6, 1.0, 1.1)
    )
    data.table::fwrite(dummy_data, file.path(temp_dir, "test_quant.csv"))

    # Mock objects needed for the function call
    mock_source <- new("Source", dir = temp_dir)
    mock_quant <- new("Quantity", id = "test_quant")
    mock_stainfo <- new("STAInfo", spatial.extent.id = "test_extent")

    # --- Test Case 1: Read the entire file ---
    # Note: sites = NULL should read all sites by calling filter_stainfo.
    # We are stubbing filter_stainfo to prevent it from running its real logic.
    testthat::local_mocked_bindings(filter_stainfo = function(x) NULL)
    field_all <- get_field_csv(
        source = mock_source,
        quant = mock_quant,
        target.STAInfo = mock_stainfo,
        date_fmt = "%Y-%m-%d"
    )
    expect_equal(nrow(field_all@data), 4)
    expect_equal(colnames(field_all@data), c("Lon", "Lat", "Year", "Day", "GPP", "NPP"))

    # --- Test Case 2: Filter by layers ---
    field_layers <- get_field_csv(
        source = mock_source,
        quant = mock_quant,
        target.STAInfo = mock_stainfo,
        layers = "GPP",
        date_fmt = "%Y-%m-%d"
    )
    # Expect time, site identifiers, and the selected layer
    expect_equal(colnames(field_layers@data), c("Lon", "Lat", "Year", "Day", "GPP"))

    # --- Test Case 3: Filter by sites ---
    all_sites <- data.table::data.table(Name = c("SiteA", "SiteB"), Lat = c(-35.0, -36.0), Lon = c(148.0, 149.0))

    # Stub the call to read_ozflux_sites() that happens inside filter_sites().
    # We target `filter_sites` because that is the function that directly
    # calls the dependency (`read_ozflux_sites`) we need to mock.
    testthat::local_mocked_bindings(read_ozflux_sites = function() all_sites)

    field_sites <- get_field_csv(
        source = mock_source,
        quant = mock_quant,
        target.STAInfo = mock_stainfo,
        sites = "SiteA",
        date_fmt = "%Y-%m-%d"
    )
    expect_equal(nrow(field_sites@data), 2)
    # Check that only data for the correct site is present
    expect_true(all(field_sites@data$Lat == -35.0))

    # --- Test Case 4: Filter by a non-existent site ---
    # This should return an empty data table gracefully.
    field_none <- get_field_csv(
        source = mock_source,
        quant = mock_quant,
        target.STAInfo = mock_stainfo,
        sites = "SiteC", # This site does not exist
        date_fmt = "%Y-%m-%d"
    )
    expect_equal(nrow(field_none@data), 0)

    # --- Test Case 5: Filter and rename layers ---
    field_renamed <- get_field_csv(
        source = mock_source,
        quant = mock_quant,
        target.STAInfo = mock_stainfo,
        layers = c("GrossPrimaryProductivity" = "GPP"),
        date_fmt = "%Y-%m-%d"
    )
    expect_equal(colnames(field_renamed@data), c("Lon", "Lat", "Year", "Day", "GrossPrimaryProductivity"))
})

test_that("get_field_csv_pivots_on_duplicate_named_layers", {
    # Create a data table.
    dummy_data <- data.table::data.table(
        date = as.Date(c("2000-01-01", "2000-01-02", "2000-01-03")),
        Lat = c(-35.0, -35.0, -36.0),
        Lon = c(148.0, 148.0, 149.0),
        GPP = c(1.1, 1.2, 2.1),
        NPP = c(0.5, 0.6, 1.0)
    )
    temp_dir <- withr::local_tempdir()
    data.table::fwrite(dummy_data, file.path(temp_dir, "test_quant.csv"))

    # Create a mock source object pointing to our temp directory
    mock_source <- new("Source", dir = temp_dir)
    mock_quant <- new("Quantity", id = "test_quant")
    mock_stainfo <- new("STAInfo", spatial.extent.id = "test_extent")

    # Read 2 layers, but give them the same name.
    layers <- c("GPP", "NPP")
    names(layers) <- rep("x", length(layers))

    # Run the function.
    field <- get_field_csv(
        source = mock_source,
        quant = mock_quant,
        layers = layers,
        date_fmt = "%Y-%m-%d"
    )

    # Assert that the returned field has the correct data.
    dt <- field@data

    # Should have pivoted on layer name to long format.
    expect_equal(nrow(dt), 6)
    expect_equal(ncol(dt), 6) # Lon, Lat, Year, Day, x_layer, x
    expect_equal(colnames(dt), c("Lon", "Lat", "Year", "Day", "x_layer", "x"))
    expect_equal(unique(dt$x_layer), c("GPP", "NPP"))
})

test_that("get_file_path constructs correct path", {
    mock_source <- new("Source", dir = "/my/data")
    mock_quant <- new("Quantity", id = "gpp")

    expected_path <- "/my/data/gpp.csv"

    # Handle OS-specific path separators
    if (.Platform$OS.type == "windows") {
        expected_path <- gsub("/", "\\\\", expected_path)
    }

    expect_equal(get_file_path(mock_source, mock_quant), expected_path)
})
