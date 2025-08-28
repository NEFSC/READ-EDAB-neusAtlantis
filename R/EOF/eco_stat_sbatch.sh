#!/bin/bash
# eco_stat_sbatch.sh
# This script submits an sbatch array job to process Atlantis runs.

# --- SBATCH Directives ---
# These lines provide instructions to the Slurm scheduler.

# Job name (will appear in squeue)
#SBATCH --job-name=atlantis_eof_array

# Output file for standard output (STDOUT) - %A for Job ID, %a for Array Task ID
# This will create files like atlantis_eof_array_12345_1.out, atlantis_eof_array_12345_2.out, etc.
#SBATCH --output=/atlantisdisk/catch_thresholds_eof_3/analysis/slurm_output/atlantis_eof_array_%A_%a.out

# Error file for standard error (STDERR) - %A for Job ID, %a for Array Task ID
#SBATCH --error=/atlantisdisk/catch_thresholds_eof_3/analysis/slurm_error/atlantis_eof_array_%A_%a.err

# Wall-clock time limit. Adjust based on your R script's expected runtime.
#SBATCH --time=02:00:00

# Memory per CPU. Each task will request 4GB. Adjust as needed for R.


# Explicitly request 1 CPU per task.
#SBATCH --cpus-per-task=1
#SBATCH --nodes=1

# Number of tasks in the array.
NUM_RUNS=101

# Define the array range. Slurm array tasks are 1-indexed.
# This will create tasks from 1 to NUM_RUNS.
#SBATCH --array=1-${NUM_RUNS}

# --- REMOVED: #SBATCH --nodes=1 ---
# This allows Slurm to distribute the 101 tasks across multiple nodes
# if a single node with 101+ cores isn't immediately available.
# Keep this line if your tasks MUST run on the same node.

# Specify the partition. Ensure 'compute' is the correct and available partition.
#SBATCH --partition=compute
#SBATCH --nodelist=josephcaracappa-cloudcalibrationv3-00013-1-0001

# --- Environment Setup ---
# Load R module. Confirm 'R/3.6.1' is the correct and available module name.
module load R/3.6.1

# --- Renv Activation ---
# Define the root directory of your R project where the 'renv.lock' file is located.
# This is crucial for renv to know which project to activate.
# Based on your Rscript path, it seems your project root is:
export RENV_PROJECT="/model/Joseph.Caracappa/READ-EDAB-neusAtlantis"

# Ensure 'renv' package itself is installed. If not, install it into a temporary location
# or a user-specific library that's always in R's path.
# This check prevents errors if renv isn't globally available with R/3.6.1.
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"

# Activate the renv project and restore packages from renv.lock.
# This makes sure all packages defined in your renv.lock are available
# and the R session uses the project's isolated library.
echo "Activating renv project: ${RENV_PROJECT}"
Rscript -e "options(renv.consent = TRUE); setwd('${RENV_PROJECT}'); renv::restore()"
echo "Renv restore complete."


# --- Create necessary output directories before running the jobs ---
EXPERIMENT_ID="catch_thresholds_eof_3"
MAIN_ANALYSIS_DIR="/atlantisarchive/Joseph.Caracappa/${EXPERIMENT_ID}/analysis"
SLURM_OUTPUT_DIR="${MAIN_ANALYSIS_DIR}/slurm_output"
SLURM_ERROR_DIR="${MAIN_ANALYSIS_DIR}/slurm_error"

mkdir -p "${MAIN_ANALYSIS_DIR}"
mkdir -m 770 "${SLURM_OUTPUT_DIR}"
mkdir -m 770 "${SLURM_ERROR_DIR}"

# --- REMOVED: chmod command ---
# It's best practice to let `mkdir -p` handle default permissions.
# Rely on proper user/group ownership for access to /atlantisarchive.

# --- Execute the R script for the current array task ---
echo "Starting R script for task ID: ${SLURM_ARRAY_TASK_ID}"
# The Rscript command should now execute within the renv-managed environment.
Rscript "${RENV_PROJECT}/R/EOF/call_process_eco_state.R" "${SLURM_ARRAY_TASK_ID}"

echo "Finished R script for task ID: ${SLURM_ARRAY_TASK_ID}"
