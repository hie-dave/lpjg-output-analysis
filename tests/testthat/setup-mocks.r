# This file contains mock object definitions for use in tests. testthat
# automatically sources files in tests/testthat/ that start with 'helper-'.

# Mock DGVMTools Source and Quantity objects for testing, as the functions
# require them. This avoids a hard dependency on DGVMTools being perfectly
# installed and configured just to run unit tests on functions that use its
# objects.

setClass("Source", slots = list(dir = "character",
                                id = "character",
                                name = "character"))

setClass("Quantity", slots = list(id = "character",
                                  name = "character",
                                  units = "character",
                                  colours = "function",
                                  format = "character",
                                  standard_name = "character"))

setClass("STAInfo", slots = list(spatial.extent.id = "character",
                                 year.aggregate.method = "character",
                                 first.year = "numeric",
                                 last.year = "numeric",
                                 spatial.extent = "character",
                                 spatial.aggregate.method = "character",
                                 subannual.resolution = "character",
                                 subannual.aggregate.method = "character",
                                 subannual.original = "character"),
         prototype = list(spatial.extent.id = NA_character_,
                          year.aggregate.method = "none",
                          first.year = NA_real_,
                          last.year = NA_real_,
                          spatial.extent = NA_character_,
                          spatial.aggregate.method = "none",
                          subannual.resolution = "none",
                          subannual.aggregate.method = "none"))

setClass("Field", slots = list(id = "character",
                               data = "ANY", # data.frame or data.table
                               quant = "ANY", # Using ANY for simplicity in mocks
                               source = "ANY"),
         contains = "STAInfo")
