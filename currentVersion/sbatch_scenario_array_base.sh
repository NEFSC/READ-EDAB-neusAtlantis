#!/bin/bash
#SBATCH --nodes=1
#SBATCH --array=1-5

sudo mkdir -p /atlantisdisk/slurm_array2/out$SLURM_ARRAY_TASK_ID

sudo singularity exec --bind /model/Joseph.Caracappa/READ-EDAB-neusAtlantis/currentVersion:/app/model,/atlantisdisk/slurm_array2/out$SLURM_ARRAY_TASK_ID:/app/model/output /model/atlantisCode/atlantis6681.sif /app/model/RunAtlantis$SLURM_ARRAY_TASK_ID.sh