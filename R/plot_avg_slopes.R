# slopes_df
# tar_load(roi)
# library(tidyterra)
# library(ggpattern)

plot_avg_slopes <- function(slopes_df, roi) {
  
  gam_pval_rast <- 
    rast(slopes_df |> select(x, y, p.value)) < 0.05
  
  non_sig <- 
    as.polygons(gam_pval_rast) |>
    filter(p.value == 0)  |>
    mutate(p.value = as.factor(p.value)) 
  crs(non_sig) <- "ESRI:102010"
  
  p <-
    ggplot(slopes_df) +
    geom_raster(aes(x = x, y = y, fill = estimate)) +
    geom_spatvector(data = roi, fill = NA, inherit.aes = FALSE) +
    geom_sf_pattern(
      data = st_as_sf(non_sig),
      aes(pattern_fill = ""), #TODO trick to get legend to show up, but there's a new way to do this in ggplot2 I think
      pattern = "crosshatch",
      fill = NA,
      colour = NA,
      pattern_alpha = 0.5, #maybe not necessary
      pattern_size = 0.05, #make lines smaller
      pattern_spacing = 0.01, #make lines closer together
      pattern_res = 200, #make lines less pixelated
    ) +
    scale_pattern_fill_manual(values = c("grey30")) +
    scale_fill_continuous_diverging(na.value = "transparent", rev = TRUE) +
    labs(fill = "∆DOY/yr",
         pattern_fill = "p > 0.05",
         title = "Trend in DOY to reach 50 GDD",
         x = "",
         y = "") +
    coord_sf(crs = "ESRI:102010") +
    theme_minimal()
  filename <- paste0(deparse(substitute(slopes_df)), ".png")
  ggsave(
    filename = filename,
    plot = p, 
    path = "output/gams/",
    bg = "white",
    width = 5,
    height = 5
  )
}
