# LPJ-Guess Output Analysis and Benchmarking

This repository contains R code for visualising LPJ-Guess outputs, and for
generating the ozflux site-level benchmarks report.

The code is structured as an R package, and provides a number of convenient
functions for visualising model outputs. Currently, these are mainly targeted at
point-level simulations.

To install the package, run the following R code:

```R
install.packages("remotes")
remotes::install_github("hie-dave/lpjg-output-analysis", dependencies = TRUE)
```

To view the package documentation:

```R
help(package = "daveanalysis")
# or:
# help("ozflux_plot")
```
