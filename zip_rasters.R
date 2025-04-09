library(fs)
count_rasts <- dir_ls("output/rasters/", regexp = "_count_")
zip("output/rasters/count_years.zip", count_rasts)

mean_rasts <- dir_ls("output/rasters/", regexp = "_mean_")
zip("output/rasters/means.zip", mean_rasts)

sd_rasts <- dir_ls("output/rasters/", regexp = "_sd_")
zip("output/rasters/sds.zip", sd_rasts)
