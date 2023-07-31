#!/bin/bash
#SBATCH --nodes=1
#SBATCH --array=1-5

sudo mkdir -p /contrib/$USER/slurm_array2/out$SLURM_ARRAY_TASK_ID

sudo singularity exec --bind /contrib/neus-atlantis/currentVersion:/app/model,/contrib/$USER/slurm_array2/out$SLURM_ARRAY_TASK_ID:/app/model/output /contrib/atlantisCode/atlantis6536.sif /app/model/RunAtlantis$SLURM_ARRAY_TASK_ID.sh