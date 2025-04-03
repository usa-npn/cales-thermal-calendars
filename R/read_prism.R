#' Read in PRISM data from zip files and convert to ºF
#'
#' @param rast_dir path to directory containing zip files for a particular year
read_prism <- function(rast_dir) {
  files <- fs::dir_ls(rast_dir, glob = "*.zip")

  #convert filenames to DOY to use for layer names
  doys <- files |>
    fs::path_file() |>
    stringr::str_extract("\\d{8}") |>
    lubridate::ymd() |>
    lubridate::yday()

  #construct paths with /vsizip/ to read inside .zip files
  bils <-
    files |>
    fs::path_file() |>
    fs::path_ext_set(".bil")
  rast_paths <- paste0("/vsizip/", fs::path(files, bils))

  #read in multi-layer rasters
  prism <- terra::rast(rast_paths)
  names(prism) <- doys

  #convert to ºF
  prism <- prism * (9 / 5) + 32
  terra::units(prism) <- "ºF"
  #sort layers by DOY
  prism <- terra::subset(prism, as.character(min(doys):max(doys)))
  #return
  prism
}
