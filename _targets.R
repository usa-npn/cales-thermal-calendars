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
library(qs2) #for format = "qs"
library(nanoparquet) #for format = tar_format_nanoparquet()

# Detect whether you're on HPC & not with an Open On Demand session (which cannot submit SLURM jobs).
slurm_host <- Sys.getenv("SLURM_SUBMIT_HOST")
hpc <- grepl("hpc\\.arizona\\.edu", slurm_host) & !grepl("ood", slurm_host)

controller_hpc <- 
  crew.cluster::crew_controller_slurm(
    name = "hpc",
    workers = 5, 
    # make workers semi-persistent: 
    tasks_max = 40, # shut down SLURM job after completing 40 targets
    seconds_idle = 300, # or when idle for some time
    slurm_partition = "standard",
    slurm_time_minutes = 60, #wall time for each worker
    slurm_log_output = "logs/crew_log_%A.out",
    slurm_log_error = "logs/crew_log_%A.err",
    slurm_memory_gigabytes_per_cpu = 5,
    slurm_cpus_per_task = 3, #use 3 cpus per worker
    script_lines = c(
      "#SBATCH --account theresam",
      #use optimized openBLAS for linear algebra
      "export LD_PRELOAD=/opt/ohpc/pub/libs/gnu8/openblas/0.3.7/lib/libopenblas.so",
      "module load gdal/3.8.5 R/4.4 eigen/3.4.0"
    )
  )

controller_local <-
  crew::crew_controller_local(
    name = "local",
    workers = 3, 
    seconds_idle = 60,
    options_local = crew::crew_options_local(
      log_directory = "logs/"
    )
  )

if (isTRUE(hpc)) { #when on HPC, do ALL the thresholds
  threshold <- seq(50, 2500, by = 50)
} else { # only do select thresholds
  threshold <- c(50, 1250, 2500)
}

# Set target options:
tar_option_set(
  trust_timestamps = TRUE, #just check last modified date when deciding whether to re-run
  # Packages that your targets need for their tasks.
  packages = c(
    "fs",
    "terra",
    "stringr",
    "lubridate",
    "colorspace",
    "purrr",
    "ggplot2",
    "tidyterra",
    "glue",
    "car",
    "httr2",
    "readr",
    "sf",
    "maps",
    "tidyr",
    "dplyr",
    "broom",
    "forcats",
    "mgcv"
  ), 
  controller = crew::crew_controller_group(controller_hpc, controller_local),
  resources = tar_resources(
    crew = tar_resources_crew(controller = ifelse(isTRUE(hpc), "hpc", "local"))
  ),
  #assume workers have access to the _targets/ data store
  storage = "worker",
  retrieval = "worker",
  memory = "auto",
  #allows use of `tar_workspace()` to load dependencies of an errored target for interactive debugging.
  workspace_on_error = TRUE 
)

# `source()` the R scripts in the R/ folder with your custom functions:
tar_source()


tar_plan(
  years = 1981:2023,
  tar_target(
    name = prism_tmean,
    command = get_prism_tmean(years),
    pattern = map(years),
    deployment = "main", #prevent downloads from running in parallel on distributed workers
    format = "file", 
    description = "download PRISM data"
  ),
  tar_terra_vect(
    roi,
    make_roi(),
    deployment = "main",
    description = "vector for North East"
  ),
  tar_map( # for each threshold...
    values = list(threshold = threshold),
    tar_terra_rast(
      gdd_doy,
      calc_gdd_doy(rast_dir = prism_tmean, roi = roi, gdd_threshold = threshold, gdd_base = 10),
      pattern = map(prism_tmean),
      iteration = "list",
      description = "calc DOY to reach threshold GDD"
    ),
    
    # This converts the output of the dynamic branching to be SpatRasters with
    # multiple layers instead of lists of SpatRasters. Would love to not have to
    # have this target, but there is no way to customize how iteration works.
    tar_terra_rast(
      gdd_doy_stack,
      terra::rast(unname(gdd_doy))
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
  )
)
