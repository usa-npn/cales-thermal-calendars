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
controller_hpc_light <- 
  crew.cluster::crew_controller_slurm(
    name = "hpc_light",
    workers = 5, 
    seconds_idle = 300, #  time until workers are shut down after idle
    tasks_max = 40, # make workers semi-persistent—launch new SLURM job after 40 targets
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
      "export LD_PRELOAD=/opt/ohpc/pub/libs/gnu8/openblas/0.3.7/lib/libopenblas.so",
      "module load gdal/3.8.5 R/4.4 eigen/3.4.0"
      #add additional lines to the SLURM job script as necessary here
    )
  )
controller_hpc_heavy <- 
  crew.cluster::crew_controller_slurm(
    name = "hpc_heavy",
    workers = 3, 
    seconds_idle = 1000, #  time until workers are shut down after idle
    tasks_max = 20, # make workers semi-persistent—launch new SLURM job after 20 targets
    garbage_collection = TRUE, # run garbage collection between tasks
    launch_max = 5L, # number of unproductive launched workers until error
    slurm_partition = "standard",
    slurm_time_minutes = 120, #wall time for each worker
    slurm_log_output = "logs/crew_log_%A.out",
    slurm_log_error = "logs/crew_log_%A.err",
    slurm_memory_gigabytes_per_cpu = 5,
    slurm_cpus_per_task = 5, 
    script_lines = c(
      "#SBATCH --account theresam",
      "export LD_PRELOAD=/opt/ohpc/pub/libs/gnu8/openblas/0.3.7/lib/libopenblas.so",
      "module load gdal/3.8.5 R/4.4 eigen/3.4.0"
      #add additional lines to the SLURM job script as necessary here
    )
  )

controller_local <-
  crew::crew_controller_local(
    name = "local",
    local_log_directory = "logs/",
    workers = 3, 
    seconds_idle = 60
  )

if (isTRUE(hpc)) { #when on HPC, do ALL the thresholds
  threshold <- seq(50, 2500, by = 50)
} else { # only do select thresholds
  threshold <- c(50, 1250, 2500)
}

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks.
  packages = c("fs", "terra", "stringr", "lubridate", "colorspace", "purrr",
               "ggplot2", "tidyterra", "glue", "car", "httr2", "readr", "sf", "maps", "tidyr"),
  controller = crew::crew_controller_group(controller_hpc_heavy, controller_hpc_light, controller_local),
  resources = tar_resources(
    crew = tar_resources_crew(controller = ifelse(hpc, "hpc_light", "local"))
  ),
  #assume workers have access to the data as well
  storage = "worker",
  retrieval = "worker"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

main <- tar_plan(
  years = 1981:2023,
  tar_target(
    name = prism_tmean,
    command = get_prism_tmean(years),
    pattern = map(years),
    deployment = "main", #prevent downloads from running in parallel
    format = "file_fast" #just check last modified date when deciding whether to re-run
  ),
  tar_terra_vect(roi, make_roi(), deployment = "main"),
  tar_map(
    values = list(threshold = threshold),
    tar_terra_rast(
      gdd_doy,
      calc_gdd_doy(rast_dir = prism_tmean, roi = roi, gdd_threshold = threshold),
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
      plot_doy(gdd_doy_stack, threshold = threshold, width = 15, height = 8),
      resources = tar_resources(
        crew = tar_resources_crew(controller = "hpc_heavy")
      ),
      format = "file"
    ),
    tar_terra_rast(
      doy_trend,
      get_lm_slope(gdd_doy_stack),
      resources = tar_resources(
        crew = tar_resources_crew(controller = "hpc_heavy")
      )
    ),
    tar_target(
      doy_trend_tif,
      write_tiff(doy_trend, filename = paste0("doy_trend_", threshold, ".tif")),
      format = "file"
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
      write_tiff(normals_summary[["mean"]],
                 filename = paste0("normals_mean_", threshold, ".tiff")),
      format = "file"
    ),
    tar_target(
      normals_sd_gtiff,
      write_tiff(normals_summary[["sd"]], 
                 filename = paste0("normals_sd_", threshold, ".tiff")),
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

gams <- tar_plan(
  tar_map(
    values = tidyr::expand_grid(
      resolution = c(50000, 25000, 10000, 5000),
      # resolution = c(50000),
      k = c(50, 100, 200, 400, 800)
      # k = c(50, 100)
    ),
    tar_target(
      gam_df,
      make_gam_df(gdd_doy_stack_50, res = resolution),
      format = "qs"
    ),
    tar_target(
      gam,
      fit_bam(gam_df, k_spatial = k),
      format = "qs",
      resources = tar_resources(
        crew = tar_resources_crew(controller = ifelse(hpc, "hpc_heavy", "local"))
      )
    ),
    tar_file(
      smooth_est,
      draw_smooth_estimates(gam, roi),
      description = "Save smooth estimates plots"
    ),
    tar_target(
      k_check,
      check_k(gam),
      packages = c("mgcv", "dplyr")
    )
  ),
  tar_target(
    k_check_df,
    bind_rows(!!!rlang::syms(
      tidyr::expand_grid(
        resolution = c(50000, 25000, 10000, 5000),
        # resolution = c(50000),
        k = c(50, 100, 200, 400, 800)
        # k = c(50, 100)
      ) |> 
        glue::glue_data("k_check_{resolution}_{k}")
    )),
    tidy_eval = TRUE
  ),
  tar_file(
    k_check_df_csv,
    tar_write_csv(k_check_df, "k_check.csv")
  ),
  tar_target(
    slope_newdata,
    make_slope_newdata(gdd_doy_stack_50, res_m = 50000)
  ),
  tar_map(
    values = list(
      gam = rlang::syms(c("gam_50000_50", "gam_50000_400", "gam_25000_400", "gam_25000_800", "gam_10000_800"))
    ),
    tar_target(
      slopes,
      calc_avg_slopes(gam, slope_newdata),
      packages = c("marginaleffects", "mgcv")
    ),
    tar_file(
      slopes_plot,
      plot_avg_slopes(slopes),
      packages = c("ggpattern", "ggplot2", "terra", "tidyterra")
    )
  )
)


tar_plan(
  main,
  get_results,
  combine_results,
  gams
)
