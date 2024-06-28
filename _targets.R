# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(geotargets)
library(crew)
library(crew.cluster)

# Detect whether you're on HPC & not with an Open On Demand session (which cannot submit SLURM jobs) and set appropriate controller
slurm_host <- Sys.getenv("SLURM_SUBMIT_HOST")
hpc <- grepl("hpc\\.arizona\\.edu", slurm_host) & !grepl("ood", slurm_host)
# If on HPC, use SLURM jobs for parallel workers

controller_hpc_light <- crew.cluster::crew_controller_slurm(
  "hpc_light",
  workers = 3, 
  seconds_idle = 300, #  time until workers are shut down after idle
  garbage_collection = TRUE, # run garbage collection between tasks
  launch_max = 5L, # number of unproductive launched workers until error
  slurm_partition = "standard",
  slurm_time_minutes = 60, #wall time for each worker
  slurm_log_output = "logs/crew_log_%A.out",
  slurm_log_error = "logs/crew_log_%A.err",
  slurm_memory_gigabytes_per_cpu = 5,
  slurm_cpus_per_task = 3, #use 3 cpus per worker
  script_lines = c(
    "#SBATCH --account theresam",
    "module load gdal/3.8.5 R/4.3 eigen/3.4.0",
    "export LD_PRELOAD=/opt/ohpc/pub/libs/gnu8/openblas/0.3.7/lib/libopenblas.so" #use OpenBLAS
  )
)

#a controller with more cores for use with NCV (one thread per core)
controller_hpc_heavy <- crew.cluster::crew_controller_slurm(
  "hpc_heavy",
  workers = 2, 
  seconds_idle = 300, #  time until workers are shut down after idle
  garbage_collection = TRUE, # run garbage collection between tasks
  launch_max = 5L, # number of unproductive launched workers until error
  slurm_partition = "standard",
  slurm_time_minutes = 2000, #wall time for each worker
  slurm_log_output = "logs/crew_log_%A.out",
  slurm_log_error = "logs/crew_log_%A.err",
  slurm_memory_gigabytes_per_cpu = 5,
  slurm_cpus_per_task = 6, #use 6 cpus per worker
  script_lines = c(
    "#SBATCH --account theresam",
    "module load gdal/3.8.5 R/4.3 eigen/3.4.0",
    "export LD_PRELOAD=/opt/ohpc/pub/libs/gnu8/openblas/0.3.7/lib/libopenblas.so" #use OpenBLAS
  )
)

controller_local <- 
  crew::crew_controller_local("local", workers = 3,
                              seconds_idle = 60)


  #when on HPC, do ALL the thresholds
if (hpc) {
  threshold <- seq(50, 2500, by = 50) 
} else { # If local or on OOD session, use multiple R sessions for workers
  threshold <- c(50, 1000, 2500)
}

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c("fs", "terra", "stringr", "lubridate", "colorspace", "purrr",
               "ggplot2", "tidyterra", "glue", "car", "httr2", "tidyr"),
  controller = crew::crew_controller_group(controller_hpc_heavy, controller_hpc_light, controller_local),
  resources = tar_resources(crew = tar_resources_crew(controller = ifelse(isTRUE(hpc), "hpc_light", "local")))
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:

main <- tar_plan(
  years = 1981:2023,
  tar_target(
    name = prism_tmean,
    command = get_prism_tmean(years),
    pattern = map(years),
    deployment = "main", #prevent downloads from running in parallel
    format = "file_fast" #just check last modified date
  ),
  tar_file(casc_ne_file, "data/Northeast_CASC.zip"),
  tar_terra_vect(casc_ne, read_casc_ne(casc_ne_file)),
  tar_map(
    values = list(threshold = threshold),
    tar_terra_rast(
      gdd_doy,
      calc_gdd_doy(rast_dir = prism_tmean, casc_ne = casc_ne, gdd_threshold = threshold),
      pattern = map(prism_tmean),
      iteration = "list"
    ),
    
    # This converts the output of the dynamic branching to be SpatRasters with
    # multiple layers instead of lists of SpatRasters. Would love to not have to
    # have this target, but there is no way to customize how iteration works.
    tar_terra_rast(
      gdd_doy_stack,
      terra::rast(unname(gdd_doy))
    ),
    tar_target(
      doy_plot,
      plot_doy(gdd_doy_stack, threshold = threshold),
      format = "file"
    ),
    tar_terra_rast(
      doy_trend,
      get_lm_slope(gdd_doy_stack)
    ),
    tar_target(
      trend_plot,
      plot_slopes(doy_trend, threshold = threshold),
      format = "file"
    ),
    tar_terra_rast(
      normals_summary,
      summarize_normals(gdd_doy_stack),
      deployment = "main"
    ),
    tar_target(
      normals_mean_gtiff,
      write_tiff(normals_summary[["mean"]], filename = paste0("normals_mean_", threshold, ".tiff")),
      format = "file"
    ),
    tar_target(
      normals_sd_gtiff,
      write_tiff(normals_summary[["sd"]], filename = paste0("normals_sd_", threshold, ".tiff")),
      format = "file"
    ),
    tar_target(
      normals_mean_plot,
      plot_normals_mean(normals_summary, threshold, height = 7, width = 7),
      format = "file"
    ),
    tar_target(
      normals_sd_plot,
      plot_normals_sd(normals_summary, threshold, height = 7, width = 7),
      format = "file"
    )
  ),
)

get_results <- tar_plan(
  tar_map(#for selected thresholds
    values = list(
      trend_raster = rlang::syms(
        c("doy_trend_50", "doy_trend_1250", "doy_trend_2500")
        # Or to do all of them
        # paste("doy_trend", threshold, sep = "_")
      )
    ),
    tar_target(
      df,
      trend_rast2df(trend_raster)
    )
  )
)

combine_results <- tar_plan(
  tar_combine(
    trend_data,
    get_results
  ),
  tar_file(
    trend_data_csv,
    tar_write_csv(trend_data, "output/data/slopes.csv")
  )
)

reports <- tar_plan(
  # Reports 
  # tar_quarto(spatial_report, path = "docs/spatial-trends-report.qmd", working_directory = "docs"),
  # tar_quarto(readme, path = "README.Qmd", cue = tar_cue("always"))
)

gams <- tar_plan(
  tar_target(
    gam_df_50,
    #project to units of meters and aggregate a LOT for testing
    make_model_df(gdd_doy_stack_50 |> project(crs("EPSG:32618")), agg_factor = 15)
  ),
  tar_target(
    nei,
    make_nei(gam_df_50, buffer = 200000), #200km
    description = "create `nei` object required by mgcv for 'NCV' method"
  ),
  tar_map(
    values = list(k_spatial = c(25, 50, 75)),
    tar_target(
      gam_reml,
      fit_bam(gam_df_50, k_spatial = k_spatial),
      packages = c("mgcv")
    ),
    tar_target(
      gam_ncv,
      fit_ncv(gam_df_50, nei = nei, k_spatial = k_spatial, threads = ifelse(isTRUE(hpc), 6, 2)),
      packages = c("mgcv"),
      resources = tar_resources(
        crew = tar_resources_crew(controller = ifelse(isTRUE(hpc), "hpc_heavy", "local"))
      )
    ),
    tar_file(
      gam_ncv_png,
      draw_gam(gam_ncv)
    ),
    tar_file(
      gam_reml_png,
      draw_gam(gam_reml)
    )
  )      
)

#if on HPC don't render quarto docs (no quarto or pandoc on HPC)
if (isTRUE(hpc)) {
  list(main, gams, get_results, combine_results)
} else {
  list(main, gams, get_results, combine_results, reports)
}