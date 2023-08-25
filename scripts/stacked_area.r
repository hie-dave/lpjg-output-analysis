
# Colour palette optimised for people with various kinds of colour-blindness.
# Wong, B. (2011) Color blindness, Nature Methods, Vol 8, No. 6.
cb_colours <- c(
  "#e69f00",
  "#56b4e9",
  "#cc79a7",
  "#009e73",
  "#0072b2",
  "#d55e00",
  "#f0e442",
  "#000000"
)

#' Return a vector of colours of the specified length, to be used for plotting.
get_colour_palette <- function(n) {
  if (n <= length(cb_colours)) {
    return(cb_colours[1:n])
  }
  return(hcl.colors(n))
}

stacked_area <- function(x, ys, xlab, ylab, ymax = NA, names = NULL) {
  indices <- c()
  if (is.na(ymax)) {
    ymax <- -1e300
  }
  ymin <- 1e300
  sum <- rep(0, length(x))
  for (i in seq_along(ys)) {
    yi <- ys[[i]]
    if (!all(yi == 0)) {
      indices <- c(indices, i)
      sum <- sum + yi
      ymax <- max(ymax, sum)
      ymin <- min(ymin, min(yi))
    }
  }
  colours <- get_colour_palette(length(indices))
  #ylim <- get_ylim(ys)
  ylim <- c(ymin, ymax)
  xlim <- c(min(x), max(x))
  i <- 1
  sum <- rep(0, length(x))
  rx <- rev(x)
  is <- c()
  if (is.null(names) && is.data.frame(ys)) {
    names <- colnames(ys)
  }
  for (i in indices) {
    y <- ys[[i]]
    if (!all(y == 0)) {
      yi <- y + sum
      par(new = length(is) > 0)
      colour <- colours[length(is) + 1]
      plot(x, yi, xlim = xlim, ylim = ylim, xlab = NA, ylab = NA, yaxt = "n"
        , type = "l", col = colour)
      polygon(c(x, rx), c(yi, rev(sum)), xlim = xlim, ylim = ylim, col = colour
        , border = NA)
      sum <- yi
      is <- c(is, i)
    }
    i <- i + 1
  }
  labels <- pretty(range(ylim))
  at <- seq(ylim[1], ylim[2], length.out = length(labels))
  axis(side = 2, at = at, labels = labels, las = 1)
  #par(xpd = NA)
  if (length(is) > 0 && !is.null(names)) {
    legend("topleft", legend = names[is], text.col = colours
          , lwd = par()$lwd, col = colours)
  }
  mtext(xlab, side = 1, line = 3)
  mtext(ylab, side = 2, line = 3)
}