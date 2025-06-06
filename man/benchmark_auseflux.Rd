% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark-auseflux.r
\name{benchmark_auseflux}
\alias{benchmark_auseflux}
\title{Do AusEFlux benchmarks for a particular variable.}
\usage{
benchmark_auseflux(var, settings, params, tables)
}
\arguments{
\item{var}{Variable to be benchmarked. Should be one of: GPP, NEE, ER}

\item{settings}{Benchmark settings}

\item{params}{Benchmark parameters}

\item{tables}{Benchmark tables}
}
\value{
Returns a list containing the benchmark results, or NULL if DGVMBenchmarks is not available
}
\description{
Do AusEFlux benchmarks for a particular variable.
}
