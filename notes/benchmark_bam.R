library(microbenchmark)
microbenchmark(list = list(
  REML = {mgcv::bam(
    DOY ~ 
      ti(x, y, bs = "cr", d = 2, k = 50) +                                # <1>
      ti(year_scaled, bs = "cr", k = 10) +                                # <2>
      ti(x, y, year_scaled, d = c(2,1), bs = "cr", k = c(50, 20)),        # <3>
    data = gdd_df,
    method = "REML"
  )},
  fREML = {mgcv::bam(
    DOY ~ 
      ti(x, y, bs = "cr", d = 2, k = 50) +                                # <1>
      ti(year_scaled, bs = "cr", k = 20) +                                # <2>
      ti(x, y, year_scaled, d = c(2,1), bs = "cr", k = c(50, 20)),        # <3>
    data = gdd_df,
    method = "fREML"
  )},
  fREML_discrete = {mgcv::bam(
    DOY ~ 
      ti(x, y, bs = "cr", d = 2, k = 50) +                                # <1>
      ti(year_scaled, bs = "cr", k = 20) +                                # <2>
      ti(x, y, year_scaled, d = c(2,1), bs = "cr", k = c(50, 20)),        # <3>
    data = gdd_df,
    method = "fREML",
    discrete = TRUE
  )},
  fREML_discrete_2threads = {mgcv::bam(
    DOY ~ 
      ti(x, y, bs = "cr", d = 2, k = 50) +                                # <1>
      ti(year_scaled, bs = "cr", k = 20) +                                # <2>
      ti(x, y, year_scaled, d = c(2,1), bs = "cr", k = c(50, 20)),        # <3>
    data = gdd_df,
    method = "fREML",
    discrete = TRUE,
    nthreads = 2
  )}
), times = 5)

#seems like regular "REML" is fastest?
