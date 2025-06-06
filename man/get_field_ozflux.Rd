% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ozflux.r
\name{get_field_ozflux}
\alias{get_field_ozflux}
\title{Get a field for OZFLUX LPJ-GUESS.}
\usage{
get_field_ozflux(
  source,
  quant,
  layers = NULL,
  target_stainfo,
  file_name,
  verbose,
  sites = NULL,
  ...
)
}
\arguments{
\item{source}{A \code{\linkS4class{Source}} containing metadata about the
LPJ-GUESS run.}

\item{quant}{A string that defines the output file from LPJ-GUESS to be
opened. For example, "lai".}

\item{layers}{A string (or a vector of strings) specifying which layer
columns are to be read. NULL (default) means read all available layers.}

\item{target_stainfo}{The spatial-temporal target domain.}

\item{file_name}{Character string holding the name of the file. This may be
left blank.}
}
\value{
Returns a list containing firstly the data.table containing the data,
and secondly the STA.info.
}
\description{
An internal function that reads data from an ozflux run. Note that the dave_*
output files (e.g. dave_lai.out) will be read if available.
}
\keyword{internal}
