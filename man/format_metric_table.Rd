% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tellaus.r
\name{format_metric_table}
\alias{format_metric_table}
\title{Format a metric table.}
\usage{
format_metric_table(metrics, metric_table)
}
\arguments{
\item{metrics}{Names of enabled metrics.}

\item{metric_table}{The metric table.}
}
\value{
\link{kable} A kable table.
}
\description{
Colour code the metric values according to their goodess, add links to plots,
etc.
}
