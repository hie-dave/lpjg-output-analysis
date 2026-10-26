# LPJ-GUESS Output Analysis and Benchmarking

This repository contains R code for visualising LPJ-GUESS outputs, and for
generating the ozflux site-level benchmarks report.

See the [site budget vignette](vignettes/site-budget.Rmd) for required LPJ-GUESS model
outputs, observation data, simulation layout and rendering instructions.

The code is structured as an R package, and provides a number of convenient
functions for visualising LPJ-GUESS model outputs. Currently, these are mainly targeted at
point-level simulations.

To explore site-level factorial runs produced by `lpjguessRunner`, pass the
top-level runner output directory (the one containing `index.toml`) to the
interactive app:

```R
daveanalysis::runner_shiny("/path/to/runner/out")
```

The app discovers sites from instruction file basenames and simulations from
their manifests. Select multiple sites, simulations, outputs, and columns to
compare time series. Colour, line type, and point type can vary by site,
simulation, stand, patch, cohort, or manifest factor. `lpjguessRunner` must be
installed to use this app.
Legend entries show only the fields that distinguish series. The plot controls
let you place the legend on any side and set a custom title. For a single known
output with consistent units, the y-axis shows those units automatically.

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

To prepare and render a site budget:

```R
repo <- "/path/to/lpj-guess"
daveanalysis::enable_required_outputs(repo)
# Run LPJ-GUESS for your sites, then:
daveanalysis::render_site_budget(
  repo = repo, site = "CumberlandPlain", pft = "MRS",
  output_file = "reports/CumberlandPlain.html"
)
```

The template and standard observations are installed with the package. Keep
simulation settings in your own R script; package updates supply the latest
report template without requiring local report edits. See the site budget
vignette linked above for required outputs and optional AWRA observations.

`enable_required_outputs()` edits `benchmarks/ozflux/outputs.ins` to enable the
required files and daily output. The report template is shipped under `inst/`
and is included in the installed package; users do not need `scripts/`.
