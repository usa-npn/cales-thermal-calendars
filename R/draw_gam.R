#' Save partial effects plots of gams
#'
#' @param gam a gam model produced by `mgcv::gam()` or `mgcv::bam()`
#' @param path path to save figure
#' @param height figure height, passed to `ggsave()`
#' @param width figure width, passed to `ggsave()`
#' @param bg figure background color, passed to `ggsave()`
#' @param ... other arguments passed to `ggsave()`
#' 
draw_gam <- function(gam, path = "output/gams/", height = 10, width = 10, bg = "white", ...) {
  model_name <- deparse(substitute(gam))
  ggplot2::theme_set(ggplot2::theme_minimal())
  p <- gratia::draw(gam, rug = FALSE)
  
  ggplot2::ggsave(paste(model_name, "png", sep = "."), 
                  path = path, height = height, width = width, bg = bg, ...)
}

