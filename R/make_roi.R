#States in NE CASC: https://www.usgs.gov/programs/climate-adaptation-science-centers/northeast-casc
make_roi <- function() {
  maps::map(
    database = "state",
    regions = c(
      "Connecticut",
      "Delaware",
      "Kentucky",
      "Maine",
      "Maryland",
      "Massachusetts",
      "New Hampshire",
      "New Jersey",
      "New York",
      "Pennsylvania",
      "Rhode Island",
      "Virginia",
      "Vermont",
      "West Virginia"
    ),
    plot = FALSE,
    fill = TRUE
  ) |> 
    sf::st_as_sf() |> 
    sf::st_combine() |> 
    terra::vect()
}

