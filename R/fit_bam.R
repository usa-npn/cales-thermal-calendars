fit_bam <- function(data, k_spatial) {
  safe_bam <- purrr::possibly(mgcv::bam, NA)
  safe_bam(
    DOY ~ ti(x, y, bs = "cr", d = 2, k = k_spatial) +
      ti(year_scaled, bs = "cr", k = 20) +
      ti(x, y, year_scaled, d = c(2,1), bs = c("cr", "cr"), k = c(100, 20)),
    data = data,
    method = "fREML"
  )
}

# fit_ncv <- function(data, nei, k_spatial = 25, k_year = 10, threads = 2) {
#   mgcv::gam(
#     doy ~ ti(y, x, bs = "cr", d = 2, k = k_spatial) +
#       ti(year_scaled, bs = "cr", k = k_year) +
#       ti(y, x, year_scaled, d = c(2,1), bs = c("cr", "cr"), k = c(k_spatial, k_year)),
#     data = data,
#     method = "NCV",
#     nei = nei,
#     control = gam.control(ncv.threads = threads)
#   )
# }
