#!/bin/bash
#SBATCH --job-name=targets_main
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=5gb          
#SBATCH --time=24:00:00
#SBATCH --partition=standard
#SBATCH --account=theresam
#SBATCH -o logs/%x_%j.out

module load gdal/3.8.5 R/4.3 eigen/3.4.0

R -e 'targets::tar_make()'
