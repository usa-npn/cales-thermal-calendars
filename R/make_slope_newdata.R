make_slope_newdata <- function(rast, res_m) {
  rast |> 
    project(crs("ESRI:102010"), res = res_m, method = "near") |> 
    as.data.frame(xy = TRUE, na.rm = TRUE) |> 
    tidyr::pivot_longer(c(-x,-y), names_to = "year_scaled", values_to = "DOY") |> 
    dplyr::mutate(year_scaled = as.numeric(year_scaled)) |> 
    select(-DOY)
}
