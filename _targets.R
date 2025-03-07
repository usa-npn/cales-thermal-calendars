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
    options_cluster = crew_options_slurm(
      script_lines = c(
        "#SBATCH --account theresam",
        #use optimized openBLAS for linear algebra
        "export LD_PRELOAD=/opt/ohpc/pub/libs/gnu13/openblas/0.3.21/lib/libopenblas.so",
        "module load gdal/3.8.5 R/4.4 eigen/3.4.0"
      ),
      log_output = "logs/crew_log_%A.out",
      log_error = "logs/crew_log_%A.err",
      memory_gigabytes_per_cpu = 5,
      cpus_per_task = 3, #use 3 cpus per worker
      time_minutes = 60, #wall time for each worker
      partition = "standard"
    )
  )

controller_local <-
  crew::crew_controller_local(
    name = "local",
    workers = 5, 
    seconds_idle = 60,
    options_local = crew::crew_options_local(
      log_directory = "logs/"
    )
  )

js2 <- Sys.info()[["sysname"]] == "Linux" #TODO: this is crude.  Find a better indicator JS2

if (isTRUE(hpc) | isTRUE(js2)) { #when on HPC or Jetstream2, do ALL the thresholds
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
    "purrr",
    "ggplot2",
    "tidyterra",
    "glue",
    "httr2",
    "sf",
    "maps",
    "tidyr",
    "dplyr"
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
    name = prism_tmin,
    command = get_prism(years, "tmin"),
    pattern = map(years),
    deployment = "main",
    format = "file",
    description = "download PRISM tmin"
  ),
  tar_target(
    name = prism_tmax,
    command = get_prism(years, "tmax"),
    pattern = map(years),
    deployment = "main",
    format = "file",
    description = "download PRISM tmax"
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
      calc_gdd_be_doy(
        tmin_dir = prism_tmin, #ºC, but gets converted to ºF
        tmax_dir = prism_tmax, #ºC, but gets converted to ºF
        roi = roi, 
        gdd_threshold = threshold, 
        gdd_base = 50 #ºF
      ),
      pattern = map(prism_tmin, prism_tmax),
      iteration = "list",
      description = "calc DOY to reach threshold GDD"
    ),
    tar_terra_rast(
      gdd_doy_stack,
      terra::rast(unname(gdd_doy)),
      description = "Stack list of SpatRasters into layers of single SpatRaster"
    ),
    tar_terra_rast(
      normals_summary,
      summarize_normals(gdd_doy_stack),
      deployment = "main"
    ),
    #these layers are written out as separate files because that is what was requested
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
      normals_count_gtiff,
      write_tiff(normals_summary[["count"]], 
                 filename = paste0("normals_count_", threshold, ".tiff")),
      format = "file",
      description = "Number of years the GDD threshold is reached"
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
