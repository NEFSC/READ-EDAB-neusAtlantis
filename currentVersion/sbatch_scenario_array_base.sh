#!/bin/bash
#SBATCH --array=1-660
#SBATCH --partition=compute
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1

mkdir -p /robstorageatlantismain/out_$SLURM_ARRAY_TASK_ID

export APPTAINERENV_HDF5_USE_FILE_LOCKING=FALSE

singularity exec --/model/Robert.Gamble/READ-EDAB-neusAtlantis/currentVersion:/app/model,/robstorageatlantismain/test_setup_$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/RunAtlantis_$SLURM_ARRAY_TASK_ID.sh