#' Calculate DOY a threshold GDD is reached
#'
#' Calculates the DOY that a threshold AGDD is met and returns a raster of DOY
#' values.
#'
#' @param tmin_dir Path to directory containing PRISM daily min temp data for a
#'   single year. Assumes folder name is just the year.
#' @param tmax_dir Path to directory containing PRISM daily max temp data for a
#'   single year.  Assumes folder name is just the year.
#' @param roi SpatVector object with boundaries of region of interest
#' @param gdd_threshold Threshold GDD in ºF
#' @param gdd_base Temperature base, in ºF, for calculating GDD
#' @param method Method to use for GDD calculation.  Either Baskerville-Emin
#'   (`"BE"`, the default) or simple averaging (`"simple"`).
#'
#' @return SpatRaster with DOY the threshold GDD is reached.
calc_gdd_doy <- function(
  tmin_dir,
  tmax_dir,
  roi,
  gdd_threshold,
  gdd_base = 32,
  method = c("BE", "simple")
) {
  method <- match.arg(method)
  prism_tmin <- read_prism(tmin_dir)
  prism_tmax <- read_prism(tmax_dir)

  #crop to roi
  roi <- terra::project(roi, prism_tmin)
  prism_tmin_roi <- terra::crop(prism_tmin, roi, mask = TRUE)
  prism_tmax_roi <- terra::crop(prism_tmax, roi, mask = TRUE)

  if (method == "simple") {
    prism_tmean_roi <- mean(prism_tmin_roi, prism_tmax_roi)
    gdd <- terra::app(prism_tmean_roi, calc_gdd_simple, gdd_base)
  } else {
    #create sds
    prism_sds <- terra::sds(prism_tmin_roi, prism_tmax_roi)
    # calculate degree days
    gdd <- terra::lapp(prism_sds, function(x, y) {
      calc_gdd_be(
        tmin = x, #first dataset in prism_sds
        tmax = y, #second dataset in prism_sds
        base = gdd_base
      )
    })
  }
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


#' Baskerville-Emin method for GDD calculation
#'
#' @param tmin Numeric vector; min daily temp in ºF.
#' @param tmax Numeric vector; max daily temp in ºF.
#' @param base Base temp in ºF.
#' @references
#' https://www.canr.msu.edu/uploads/files/Research_Center/NW_Mich_Hort/be_method.pdf
calc_gdd_be <- function(tmin = NULL, tmax = NULL, base = 32) {
  stopifnot(length(base) == 1)
  withr::local_options(list(warn = 2)) #turn warnings into errors in the scope of this function
  .mapply(
    function(tmin, tmax) {
      #for each day...
      #NAs beget NAs
      if (is.na(tmin) | is.na(tmax)) {
        return(NA)
      }
      #check that tmax >= tmin
      if (tmin > tmax) {
        stop("tmin > tmax!")
      }
      #step 2
      if (tmax < base) {
        return(0)
      }
      #step 3
      tmean <- (tmin + tmax) / 2

      #step4
      if (tmin >= base) {
        #simple case
        return(tmean - base)
      }

      #step5
      W <- (tmax - tmin) / 2
      x <- (base - tmean) / W
      #special case for floating-point errors when `x` is (almost) equal to 1 or -1
      if (isTRUE(all.equal(x, 1))) {
        x <- 1
      }
      if (isTRUE(all.equal(x, -1))) {
        x <- -1
      }

      A <- asin(x)

      gdd <- ((W * cos(A)) - ((base - tmean) * ((pi / 2) - A))) / pi
      return(gdd)
    },
    dots = list(tmin = tmin, tmax = tmax),
    MoreArgs = NULL
  ) |>
    as.numeric()
}

#' Simple averaging method for GDD calculation
#'
#' @param tmean Numeric vector; daily mean temperature in ºF
#' @param base Base temp in ºF
calc_gdd_simple <- function(tmean, base = 32) {
  stopifnot(length(base) == 1)
  if (base != 0) {
    gdd <- tmean - base
  } else {
    gdd <- tmean
  }
  gdd[gdd < 0] <- 0
  gdd
}
