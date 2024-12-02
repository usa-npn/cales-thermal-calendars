# Phenology in the Northeastern US

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

The goal of this project is to calculate spatial and temporal patterns in phenology in [Northeast CASC](https://www.usgs.gov/programs/climate-adaptation-science-centers/northeast-casc) member states using [PRISM](https://prism.oregonstate.edu/) temperature data.
This analytical pipeline downloads daily data, calculates growing degree days (GDD) for each day, and then finds the day of year (DOY) that certain threshold GDD are reached for this region.
Products include rasters of mean and standard deviation of the DOY a range of GDD thresholds are reached over the normals period (1991-2020). 
Some examples can be found in `output/figs/`, although not all outputs are in this repository.

## Reproducibility

### `renv`

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package management.
When opening this repo as an RStudio Project for the first time, `renv` should automatically install itself and prompt you to run `renv::restore()` to install all package dependencies.
If this gets annoying (which it sometimes does) you can turn off `renv` with `renv::deactivate()` and install the packages in `_targets_packages.R` manually.
There are no guarantees that the pipeline will work with updated versions of packages, however.

### `targets`

This project uses the [`targets` package](https://docs.ropensci.org/targets/) for workflow management.
The pipeline is set up to run on the UA-HPC cluster and can be submitted as a job there with `sbatch run.sh`.  However, it should also run locally (e.g. on a laptop), just slower and with fewer workers.
To start the pipeline locally, open the directory in RStudio and run `targets::tar_make()`.

------------------------------------------------------------------------

Developed in collaboration with the University of Arizona [CCT Data Science](https://datascience.cct.arizona.edu/) group.
