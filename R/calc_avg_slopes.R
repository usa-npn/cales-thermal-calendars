# library(targets)
# library(mgcv)
# gam <- tar_read(gam_50000_50)
# newdata <- tar_read(slope_newdata)
# # draw avg slopes
# library(marginaleffects)

calc_avg_slopes <- function(gam, newdata = NULL) {
  slopes <-
    marginaleffects::avg_slopes(
      gam,
      newdata = newdata,
      variables = "year_scaled",
      type = "response",
      by = c("y", "x"),
      p_adjust = "BY",
      hypothesis = 0,
      df = insight::get_df(gam, type = "model") #TODO: not 100% sure if this is appropriate
    )
  #return as tibble
  as_tibble(slopes)
}
