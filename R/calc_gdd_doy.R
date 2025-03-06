#' Calculate DOY to reach a theshold GDD
#'
#' @param rast_dir Path to directory containing PRISM mean temp data for a
#'   single year. Assumes folder name is just the year.
#' @param roi SpatVector object with boundaries of region of interest
#' @param gdd_threshold Threshold GDD
#' @param gdd_base Temperature base, in ºC, for calculating GDD
#'
#' @return SpatRaster
calc_gdd_doy <- function(rast_dir, roi, gdd_threshold, gdd_base = 0) {
  files <- dir_ls(rast_dir, glob = "*.zip")

  #convert filenames to DOY to name layers later
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
  rast_paths <- fs::path("/vsizip", files, bils)

  #read in as raster stacks
  prism <- terra::rast(rast_paths)
  names(prism) <- doys
  units(prism) <- "ºC"
  
  #sort layers by DOY
  prism <- subset(prism, as.character(min(doys):max(doys)))
  
  #crop to roi
  roi <- project(roi, prism)
  prism_ne <- crop(prism, roi, mask = TRUE)
  
  # convert to degree days
  # function for a single layer:
  calc_dd <- function(tmean, base = 0) {
    if (base != 0) {
      dd <- tmean - base
    } else {
      dd <- tmean
    }
    dd[dd < 0] <- 0
    dd
  }
  
  #apply to every layer
  gdd <- terra::app(prism_ne, calc_dd, base = gdd_base)
  
  # convert to accumulated dd
  agdd <- cumsum(gdd)
  
  # DOY to reach a single threshold
  gdd_doy <- which.lyr(agdd > gdd_threshold)
  
  # Change `NA`s that represent never reaching the threshold GDD to `Inf`s.
  # These will be treated the same for modeling (i.e. dropped), but will allow
  # different treatment for plotting
  gdd_doy[is.na(gdd_doy) & !is.na(agdd[[1]])] <- Inf
  
  names(gdd_doy) <- 
    fs::path_file(rast_dir) #gets just the end folder name which should be the year
  
  #return:
  gdd_doy
}

#' Calculate DOY a threshold GDD is reached (Baskerville-Emin method)
#' 
#' Calculates accumulated GDD with the Baskerville-Emin method and then
#' calculates the DOY that a particular threshold is met and returns a raster of
#' DOY values.
#' @param tmin_dir Path to directory containing PRISM daily min temp data for a
#'   single year. Assumes folder name is just the year.
#' @param tmax_dir Path to directory containing PRISM daily max temp data for a
#'   single year.  Assumes folder name is just the year.
#' @param roi SpatVector object with boundaries of region of interest
#' @param gdd_threshold Threshold GDD in ºF
#' @param gdd_base Temperature base, in ºF, for calculating GDD
#'
#' @return SpatRaster with DOY the threshold GDD is reached.
calc_gdd_be_doy <- function(tmin_dir, tmax_dir, roi, gdd_threshold, gdd_base = 32) {
  prism_tmin <- read_prism(tmin_dir)
  prism_tmax <- read_prism(tmax_dir)
  
  #crop to roi
  roi <- terra::project(roi, prism_tmin)
  prism_tmin_roi <- terra::crop(prism_tmin, roi, mask = TRUE)
  prism_tmax_roi <- terra::crop(prism_tmax, roi, mask = TRUE)
  
  #create sds
  prism_sds <- terra::sds(prism_tmin_roi, prism_tmax_roi)
  
  # calculate degree days
  gdd <- terra::app(prism_sds, function(i) {
    calc_gdd_be(
      tmin = i[1], #first dataset in prism_sds
      tmax = i[2], #second dataset in prism_sds
      base = gdd_base
    )
  })
  
  # convert to accumulated gdd
  agdd <- cumsum(gdd)
  
  # DOY to reach a single threshold
  gdd_doy <- terra::which.lyr(agdd > gdd_threshold)
  
  # Change `NA`s that represent never reaching the threshold GDD to `Inf`s.
  # These will be treated the same for modeling (i.e. dropped), but will allow
  # different treatment for plotting
  gdd_doy[is.na(gdd_doy) & !is.na(agdd[[1]])] <- Inf
  
  names(gdd_doy) <- 
    fs::path_file(tmin_dir) #gets just the end folder name which should be the year
  
  #return:
  gdd_doy
}