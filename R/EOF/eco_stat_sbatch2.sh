#!/bin/bash
# eco_stat_sbatch_final.sh
# This script submits an sbatch array job to process Atlantis runs.

# --- SBATCH Directives ---
# Job name
#SBATCH --job-name=atlantis_eof_array
# Output file
#SBATCH --output=/atlantisdisk/catch_thresholds_eof_3/analysis/slurm_output/atlantis_eof_array_%A_%a.out
# Error file
#SBATCH --error=/atlantisdisk/catch_thresholds_eof_3/analysis/slurm_error/atlantis_eof_array_%A_%a.err
# Number of tasks in the array
#SBATCH --array=1-101
# Specify the partition
#SBATCH --partition=compute

# --- Environment Setup ---
# Load R module.
module load R/3.6.1

export RENV_PROJECT="/model/Joseph.Caracappa/READ-EDAB-neusAtlantis"

# --- Renv Activation ---
# Define the root directory of your R project where the 'renv.lock' file is located.

# --- CRITICAL: Pre-populate renv cache on a head node WITH INTERNET ACCESS ---
# The 'error code 22' indicates compute nodes cannot download packages.
# Before submitting this sbatch script, you MUST log into your cluster's head node (interactive session)
# and run these R commands ONCE in your project directory:
#
# cd /model/Joseph.Caracappa/READ-EDAB-neusAtlantis
# module load R/3.6.1 # (if not already loaded)
# R
# > options(renv.consent = TRUE)
# > renv::restore() # This will download and install all packages into your shared cache
# > quit()
#
# Ensure your ~/.cache/R/renv/cache (or RENV_PATHS_ROOT if you explicitly set it)
# is on a shared filesystem accessible by compute nodes.

# Ensure 'renv' package itself is installed.
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"

# Activate the renv project and restore packages from renv.lock.
# This restore will now primarily link from the pre-populated, shared renv cache.
echo "Activating renv project: ${RENV_PROJECT}"
Rscript -e "options(renv.consent = TRUE); setwd('${RENV_PROJECT}'); renv::restore()"
echo "Renv restore complete. Packages should now be linked from cache."

# --- Create necessary output directories before running the jobs ---
EXPERIMENT_ID="catch_thresholds_eof_3"
MAIN_ANALYSIS_DIR="/atlantisarchive/Joseph.Caracappa/${EXPERIMENT_ID}/analysis"
SLURM_OUTPUT_DIR="${MAIN_ANALYSIS_DIR}/slurm_output"
SLURM_ERROR_DIR="${MAIN_ANALYSIS_DIR}/slurm_error"

mkdir -p "${MAIN_ANALYSIS_DIR}"
mkdir -m 770 "${SLURM_OUTPUT_DIR}"
mkdir -m 770 "${SLURM_ERROR_DIR}"

# --- Execute the R script for the current array task ---
echo "Starting R script for task ID: ${SLURM_ARRAY_TASK_ID}"
Rscript "${RENV_PROJECT}/R/EOF/call_process_eco_state.R" "${SLURM_ARRAY_TASK_ID}"
~/.cache/R/renv/cache/v5/R-3.6/x86_64-pc-linux-gnu
echo "Finished R script for task ID: ${SLURM_ARRAY_TASK_ID}"

# --- REMOVED: sudo singularity exec command ---
# The problematic 'sudo singularity exec' command has been removed to allow the R script to run.
