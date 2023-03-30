#SBATCH --job-name=neus_test_1
#SBATCH --clusters=compute1
#SBATCH --n-tasks-per-node=1

Rscript slurm_test.R
