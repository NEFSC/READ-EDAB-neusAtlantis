#!/bin/bash

#SBATCH --job-name=neus_test_1
#SBATCH --partition=compute1
#SBATCH --nodes=1

Rscript slurm_test.R
