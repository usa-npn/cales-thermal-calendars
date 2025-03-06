#' Summarize over normals period
#'
#' @param stack a gdd_doy_stack for a particular GDD threshold
#' @param years a climate normals period, defaults to 1991:2020
#'
#' @return
summarize_normals <- function(stack, years = 1991:2020) {
  
  if(!(all(as.character(years)  %in% names(stack)))) {
    warning("`stack` does not contain all years in `years`")
  }
  
  years <- as.character(years)
  years <- years[ which(years %in% names(stack))]
  
  stack[[years]] |> 
    terra::app(function(x) {
      #rm Inf unless ALL are Inf
      if(!all(is.infinite(x))) {
        x <- x[is.finite(x)]
      }
      c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), count = \(x) sum(!is.na(x)))
    })
}

