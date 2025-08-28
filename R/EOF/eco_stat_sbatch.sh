#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=101
#SBATCH --cpus-per-task=1
#SBATCH --array=1-101 # Replace N with the length of your run.dirs vector
#SBATCH --job-name=atlantis_processing
#SBATCH --output=/model/Joseph.Caracappa/output/atlantis_processing_output.log
#SBATCH --error=/model/Joseph.Caracappa/output/atlantis_processing_error.log


# Load R and any other necessary modules
module load gnu
module load intel/2023.2.0
module load R/4.3.3/intel_2023.2.0

# Run the R script for the current task ID
Rscript process_run.R