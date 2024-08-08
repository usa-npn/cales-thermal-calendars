library(targets)
library(mgcv)
library(gratia)
library(bbmle)

tar_load(gam_df_50)
tar_load(model_50)

bam_te <- mgcv::bam(
  doy ~ te(y, x, year_scaled, d = c(2, 1), bs = c("sos", "cs"), k = c(25, 30)),
  data = gam_df_50,
  method = "REML"
)
AICctab(model_50, bam_te)


m2 <- 
  mgcv::bam(
    doy ~ ti(y, x, bs = "sos", d = 2, k = 20) +
      ti(year_scaled, bs = "cs", k = 15) + 
      ti(y, x, year_scaled, d = c(2,1), bs = c("sos", "cs"), k = c(20, 15)), 
    data = gam_df_50,
    method = "REML"
  )


AICctab(model_50, m2)

k.check(m2)
k.check(model_50)
# appraise(model_50)
draw(model_50)


