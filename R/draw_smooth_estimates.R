#draw model partial effects plot
# library(gratia)
# library(targets)
# library(colorspace)
# library(ggplot2)
# library(tidyterra)
# library(terra)
# tar_load(gam_50000_50)
# tar_load(roi)


draw_smooth_estimates <- function(gam, roi) {
  if (!inherits(gam, "gam")) { #if target doesn't exist...
    return(NULL)
  }
  roi <- project(roi, crs("ESRI:102010"))
  gam_name <- deparse(substitute(gam))

  smooth_est <- 
    gratia::smooth_estimates(gam, dist = 0.1, n_3d = 6, unconditional = TRUE, overall_uncertainty = TRUE)
  
  ti_xy_rast <- 
    smooth_est |> 
    filter(.smooth == "ti(x,y)") |> 
    select(x, y, .estimate) |> 
    as_tibble() |> 
    rast()
  
  crs(ti_xy_rast) <- "ESRI:102010"
  
  ti_xy_rast <- crop(ti_xy_rast, roi, mask = TRUE)
  
  p_ti_xy <- 
    ggplot() +
    geom_spatraster(data = ti_xy_rast) +
    geom_spatvector(data = roi, fill = "NA") +
    scale_fill_continuous_diverging(rev = TRUE, na.value = "transparent") +
    labs(fill = "Partial Effect", title = "ti(x,y)")
  
  ti_yr <- smooth_est |> 
    filter(.smooth == "ti(year_scaled)")
  
  p_ti_yr <-
    ggplot(ti_yr, aes(x = year_scaled, y = .estimate, ymin = .estimate - .se, ymax = .estimate + .se)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    labs(y = "Partial Effect", title = "ti(year_scaled)")
  
  ti_xyyr_rast <-
    smooth_est |> 
    filter(.smooth == "ti(x,y,year_scaled)") |> 
    select(x, y, year_scaled, .estimate) |> 
    mutate(year_scaled = as.character(year_scaled)) |> 
    as.data.frame() |> 
    rast(type = "xylz")
  
  crs(ti_xyyr_rast) <- "ESRI:102010"
  
  ti_xyyr_rast <- crop(ti_xyyr_rast, roi, mask = TRUE)
  
  p_ti_xyyr <- 
    ggplot() +
    geom_spatraster(data = ti_xyyr_rast) +
    facet_wrap(~lyr) +
    scale_fill_continuous_diverging(rev = TRUE, na.value = "transparent")  +
    labs(title = "ti(x,y,year_scaled)", fill = "Partial Effect")
  
  library(patchwork)
  
  p <- (p_ti_xy | p_ti_yr) / p_ti_xyyr & theme_minimal()
  
  ggsave(
    filename = paste(gam_name, "png", sep = "."),
    plot = p, 
    path = "output/gams/",
    width = 12, 
    height = 10, 
    bg = "white"
  )
  
}