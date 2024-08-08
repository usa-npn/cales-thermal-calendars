make_gam_df <- function(gdd_stack, res) {
  gdd_rast <- 
    project(gdd_stack, crs("ESRI:102010"), res = res)
  
  gdd_df <- 
    gdd_rast |> 
    as_tibble(xy = TRUE, na.rm = TRUE) |> 
    pivot_longer(c(-x, -y), names_to = "year", values_to = "DOY", names_transform = list(year = as.integer)) |>
    mutate(year_scaled = year - min(year))
  
  gdd_df
}
