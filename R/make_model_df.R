make_model_df <- function(gdd_doy_stack, agg_factor = 8) {
  terra::aggregate(gdd_doy_stack, fact = agg_factor) |> 
    tidyterra::as_tibble(xy = TRUE, na.rm = TRUE) |>
    pivot_longer(
      c(-x,-y),
      names_to = "year",
      values_to = "doy",
      names_transform = list(year = as.numeric)
    ) |> 
    mutate(year_scaled = year - min(year))
}