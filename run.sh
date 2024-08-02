#!/bin/bash
#SBATCH --job-name=targets_main
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5gb          
#SBATCH --time=48:00:00
#SBATCH --partition=standard
#SBATCH --account=theresam
#SBATCH -o logs/%x_%j.out

# use OpenBLAS
export LD_PRELOAD=/opt/ohpc/pub/libs/gnu8/openblas/0.3.7/lib/libopenblas.so
module load gdal/3.8.5 R/4.4 eigen/3.4.0

R -e 'targets::tar_make()'
#dump tar_meta for benchmarking
R -e 'readr::write_csv(targets::tar_meta(), "tar_meta_hpc.csv")'
