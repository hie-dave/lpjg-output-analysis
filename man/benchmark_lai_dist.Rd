% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmark-lai-dist.r
\name{benchmark_lai_dist}
\alias{benchmark_lai_dist}
\title{Benchmark species LAI distribution.}
\usage{
benchmark_lai_dist(settings, params, tables)
}
\arguments{
\item{settings}{Benchmark settings}

\item{params}{Benchmark parameters}

\item{tables}{Benchmark tables (rbind-ed and outputted as result$tables)}
}
\value{
\link{list} Returns a list containing the benchmark results
}
\description{
Note that the subannual (monthly) LAI output does not distinguish between
PFTs, so this benchmark only considers annual LAI.
}
