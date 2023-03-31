#!/bin/bash

#SBATCH --job-name=neus_test_1
#SBATCH --partition=compute1
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1  

#Rscript slurm_test.R
sudo mkdir /contrib/Joseph.Caracappa/out1
sudo podman run --rm  -d --name scenario1 --mount "type=bind,src=/usr/local/neus-atlantis/currentVersion/,dst=/app/model" --mount "type=bind,src=/contrib/Joseph.Caracappa/out1,dst=/app/model/output/" atlantis:6536

