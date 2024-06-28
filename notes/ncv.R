library(targets)
library(tictoc)
library(gratia)
library(mgcv)
tar_load_globals()
tar_load(gdd_doy_stack_50)
tar_load(gam_df_50)
tar_load(nei)

dim(gam_df_50)
ggplot(gam_df_50, aes(x = x, y = y)) +
  geom_raster(aes(fill = doy)) +
  scale_fill_viridis_c() +
  coord_sf(crs = "EPSG:32618")

str(nei)
#TODO currently this nei only does NCV spatially, not spatio-temporally. I think the spatio-temporal version would extend this to think of time as a third dimension and each "neighborhood" would be a rectangular prism (±x, ±y, ±time) around a central pixel where the distance (in "pixels") in space could be different than the distance in time.

#check that this is doing what I think it's doing
pl <- list()
for(pli in sample(1:nrow(gam_df_50), 9)){
  # for(pli in c(10:18)) {
  pl[[paste0(pli)]] <- ggplot(gam_df_50, aes(x=x, y=y, fill=doy)) +
    geom_raster() +
    geom_point(aes(x=x, y=y), size=2, color =  "red",
               data=gam_df_50[nei$k[(nei$m[pli-1]+1):nei$m[pli]], ]) +
    scale_fill_viridis_c() +
    coord_sf(crs = "EPSG:32618") +
    theme_minimal()
}

patchwork::wrap_plots(pl, ncol=3, nrow=3) + patchwork::plot_layout(guides = "collect", axes = "collect")

# Fit some GAMs.
# Could use `bam()` for the REML version, but it doesn't work with "NCV", so using gam() for both to even the playing field
k_spatial <- NULL
tic()
doy_reml <- mgcv::gam(
  doy ~ ti(y, x, bs = "cr", d = 2, k = k_spatial) +
    ti(year_scaled, bs = "cr") +
    ti(y, x, year_scaled, d = c(2,1), bs = c("cr", "cr"), k = c(k_spatial, 5)),
  data = gam_df_50,
  method = "REML"
)
toc() #2 sec with default knots, 3.4 sec for 35 knots
k.check(doy_reml) #not enough knots (edf = 23 for k' = 24, edf = 32 for k' = 34)

#NCV
tic()
doy_ncv <- mgcv::gam(
  doy ~ ti(y, x, bs = "cr", d = 2, k = k_spatial) +
    ti(year_scaled, bs = "cr") +
    ti(y, x, year_scaled, d = c(2,1), bs = c("cr", "cr"), k = c(k_spatial, 5)),
  data = gam_df_50,
  method = "NCV",
  nei = nei,
  control = gam.control(ncv.threads = 2)
)
toc() #432 sec with default knots, 214 w/ 2 threads.
k.check(doy_ncv) #but maybe 25 knots is enough? (edf = 14)

summary(doy_ncv)
summary(doy_reml)
gratia::draw(doy_ncv)
gratia::draw(doy_reml)
