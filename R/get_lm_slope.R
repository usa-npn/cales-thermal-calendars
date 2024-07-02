#' Pixel-wise regression to determine trends through time
#'
#' Takes a stack of yearly DOY to get to GDD and returns raster with a layer of
#' slopes and a layer of fdr adjusted p-values
#' 
#' @param gdd_stack SpatRaster; with layers for years and values of DOY
#' 
#' @returns SpatRaster with layers for slope and FDR correct p-values from
#'   linear regressions
get_lm_slope <- function(gdd_stack) {
  years <- as.integer(names(gdd_stack))

  slope_rast <- app(gdd_stack, getTrend, years = years)
  slope_rast[[2]] <- app(slope_rast[[2]], p.adjust, method = "fdr")
  names(slope_rast) <- c("slope", "p.value")
  
  #return
  slope_rast
}

#' This gets applied to each pixel with `app()` in `get_lm_slope()`
#' @param x; integer vector of DOY values
#' @param years; integer vector of years
#' 
#' @returns named numeric vector with slope coefficient and p-value from Anova
getTrend <- function(x, years) {
  #only attempt to fit a line if at least half the years have finite, non-NA
  #values for a particular pixel
  if (sum(is.finite(x)) < length(years)/2) {
    c(slope = NA, p.val = NA)
  } else {
    # remove Inf's and NaNs that aren't automatically dropped by lm()
    df <- data.frame(x = x, years = years) |> dplyr::filter(is.finite(x))
    m <- lm(x ~ years, data = df)
    c(slope = m$coefficients[2], p.val = car::Anova(m)$`Pr(>F)`[1])
  }
}