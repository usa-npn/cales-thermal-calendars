make_roi <- function() {
  maps::map(
    database = "state",
    regions = c(
      #States in NE CASC: https://www.usgs.gov/programs/climate-adaptation-science-centers/northeast-casc
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
      "West Virginia",

      # from thermal-trends manuscript
      "Connecticut",
      "Delaware",
      "Illinois",
      "Indiana",
      "Iowa",
      "Maine",
      "Maryland",
      "Massachusetts",
      "Michigan",
      "Minnesota",
      "Missouri",
      "New Hampshire",
      "New Jersey",
      "New York",
      "Ohio",
      "Pennsylvania",
      "Rhode Island",
      "Vermont",
      "West Virginia",
      "Wisconsin",
      "Kentucky",
      "Virginia"
    ) |>
      unique(),
    plot = FALSE,
    fill = TRUE
  ) |>
    sf::st_as_sf() |>
    sf::st_combine() |>
    terra::vect()
}

