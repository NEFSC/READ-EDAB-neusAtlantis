#!/bin/bash

#SBATCH --job-name=neus_test_1
#SBATCH --partition=compute1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1

Rscript slurm_test_2.R
