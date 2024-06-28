# fit_spatiotemporal_bam <- function(data) {
#   mgcv::bam(
#     doy ~ te(y, x, year_scaled, d = c(2, 1), bs = c("sos", "cs")),
#     data = data,
#     method = "REML"
#   )
# }

fit_bam <- function(data, k_spatial = 25, k_year = 10) {
  mgcv::bam(
    doy ~ ti(y, x, bs = "cr", d = 2, k = k_spatial) +
      ti(year_scaled, bs = "cr", k = k_year) +
      ti(y, x, year_scaled, d = c(2,1), bs = c("cr", "cr"), k = c(k_spatial, k_year)),
    data = data,
    method = "REML"
  )
}

fit_ncv <- function(data, nei, k_spatial = 25, k_year = 10, threads = 2) {
  mgcv::gam(
    doy ~ ti(y, x, bs = "cr", d = 2, k = k_spatial) +
      ti(year_scaled, bs = "cr", k = k_year) +
      ti(y, x, year_scaled, d = c(2,1), bs = c("cr", "cr"), k = c(k_spatial, k_year)),
    data = data,
    method = "NCV",
    nei = nei,
    control = gam.control(ncv.threads = threads)
  )
}
