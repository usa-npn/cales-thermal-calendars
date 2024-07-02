#' Label upper limit, possibly with "≥"
#' 
#' Ensures that one of the breaks is the upper range of the data.  In the case
#' that the data contains infinite values (`Inf`), the last break is the maximum
#' finite value, but labeled prepended with "≥".
#' 
#' This is basically a modified copy of `scales::breaks_extended()` to be used
#' when `oob = scales::oob_squish_infinite`.
#' 
#' @param x numeric vector
#' @param n number of breaks (result won't be exact)
#' @param ... other arguments passed to labeling::extended
#'
#' @examples
#' breaks_squish_doy(x = c(1:10, Inf))
breaks_squish_doy <- function(x, n = 5, ...) {
  n <- n+1 #because we'll probably add one for the max value
  if (length(x) == 0) {
    return(numeric())
  }
  rng <- range(x[is.finite(x)])
  breaks <- labeling::extended(rng[1], rng[2], n, ...)
  max_floor <- floor(rng[2])
  #never include breaks outside of the range
  breaks <- breaks[breaks >= rng[1] & breaks < max_floor]
  #always add the max as a final break (NOTE, this might not look pretty)
  breaks <- c(breaks, max_floor)
  # if there are infinite values, prepend the final break with "≥"
  if (any(is.infinite(x))) {
    if (any(x[is.infinite(x)] > max(x[is.finite(x)]))) {
      names(breaks) <- c(breaks[1:(length(breaks)-1)], paste0("≥", max_floor))
    }
  }
  breaks
}
