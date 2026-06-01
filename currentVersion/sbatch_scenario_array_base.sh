#!/bin/bash
#SBATCH --array=1-660
#SBATCH --partition=compute
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1

mkdir -p /atlantisdisk/Joseph.Caracappa/slurm_test/out$SLURM_ARRAY_TASK_ID

export APPTAINERENV_HDF5_USE_FILE_LOCKING=FALSE

singularity exec --bind /model/Joseph.Caracappa/READ-EDAB-neusAtlantis/currentVersion:/app/model,/atlantisdisk/Joseph.Caracappa/slurm_test/out$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/RunAtlantis_cloud.sh