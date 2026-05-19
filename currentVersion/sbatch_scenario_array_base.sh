#!/bin/bash
#SBATCH --nodes=1
#SBATCH --array=1-5
#SBATCH --partition=compute

mkdir -p /atlantisdisk/Joseph.Caracappa/slurm_test/out$SLURM_ARRAY_TASK_ID

export APPTAINERENV_HDF5_USE_FILE_LOCKING=FALSE

#sudo singularity exec --bind /model/Joseph.Caracappa/READ-EDAB-neusAtlantis/currentVersion:/app/model,/atlantistemp/$USER/slurm_test/out$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/RunAtlantis$SLURM_ARRAY_TASK_ID.sh
singularity exec --bind /model/Joseph.Caracappa/READ-EDAB-neusAtlantis/currentVersion:/app/model,/atlantisdisk/Joseph.Caracappa/slurm_test/out$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/RunAtlantis_cloud.sh