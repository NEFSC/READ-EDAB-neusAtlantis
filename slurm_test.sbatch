#!/bin/bash

#SBATCH --job-name=neus_test_1
#SBATCH --account=josephcaracappa-azclusternoaav2base-00020-1-0001 
#SBATCH --partition=compute1
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1

Rscript slurm_test.R
