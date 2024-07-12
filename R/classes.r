
#'
#' Dave Benchmark class
#'
#' This is the class used for all DAVE benchmarks. It extends the base Benchmark
#' class provided by the DGVMBenchmarks package, because the class exported from
#' that package is missing a few crucial fields.
#'
#' @slot simulation_format Format of the simulation
#' @slot dataset_source Reference/source of the dataset
setClass("DaveBenchmark",
    slots = c(simulation_format = "character",
              dataset_source = "character"),
    contains = "Benchmark",
)
