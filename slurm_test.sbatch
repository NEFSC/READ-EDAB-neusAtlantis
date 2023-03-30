#!/bin/bash

#SBATCH --job-name=neus_test_1
#SBATCH --clusters=compute1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1  

Rscript slurm_test.R
