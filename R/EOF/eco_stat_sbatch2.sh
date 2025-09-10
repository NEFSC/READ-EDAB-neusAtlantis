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

# --- Renv Activation ---
# Define the root directory of your R project where the 'renv.lock' file is located.
export RENV_PROJECT="/model/Joseph.Caracappa/READ-EDAB-neusAtlantis"

# --- CRITICAL: Set RENV_PATHS_ROOT to your ACTUAL SHARED CACHE PATH. ---
# This path MUST be accessible by ALL compute nodes.
# YOU MUST REPLACE THE PLACEHOLDER BELOW with a REAL, ABSOLUTE, and WRITABLE path.
# Examples:
#   - If your home directory is shared: export RENV_PATHS_ROOT="/home/Joseph.Caracappa/renv_cache"
#   - If you have a scratch space: export RENV_PATHS_ROOT="/scratch/Joseph.Caracappa/renv_cache"
export RENV_PATHS_ROOT="/model/shared_R_libs/renv_cache" # <--- YOU MUST CHANGE THIS LINE
mkdir -p "${RENV_PATHS_ROOT}" # This mkdir will now succeed if the path is correct.
echo "RENV_PATHS_ROOT (set in shell): ${RENV_PATHS_ROOT}" # This confirms shell var is set

# --- CRITICAL: PRE-POPULATE RENV CACHE (MANDATORY!) ---
# The 'error code 22' confirms compute nodes cannot download packages.
# Before submitting this sbatch script, you MUST perform these steps ONCE on a HEAD NODE
# that has internet access. This will fill your shared renv cache.
#
# 1. Log into your cluster's head node (interactive session).
# 2. Navigate to your project directory: cd /model/Joseph.Caracappa/READ-EDAB-neusAtlantis
# 3. Load the R module: module load R/3.6.1
# 4. Set the RENV_PROJECT environment variable: export RENV_PROJECT="/model/Joseph.Caracappa/READ-EDAB-neusAtlantis"
# 5. Set the RENV_PATHS_ROOT environment variable (MUST MATCH THE REAL PATH ABOVE!):
#    export RENV_PATHS_ROOT="/model/shared_R_libs/renv_cache" # <--- USE THE SAME REAL PATH HERE
# 6. Ensure the shared cache directory exists: mkdir -p "${RENV_PATHS_ROOT}"
# 7. Launch R: R
# 8. Inside R, run these commands:
#    > options(renv.consent = TRUE)
#    > renv::restore() # This will download all packages into your specified shared cache.
#                      # WATCH FOR ANY ERRORS HERE ON THE HEAD NODE.
#    > renv::paths$cache() # Verify the cache path inside R
#    > quit()
#
# Once this is successfully done on the head node, the cache will be populated,
# and compute nodes will simply link from it.

# Ensure 'renv' package itself is installed.
Rscript -e "if (!requireNamespace('renv', quietly = TRUE)) install.packages('renv')"

# Activate the renv project and restore packages from renv.lock.
echo "Activating renv project: ${RENV_PROJECT}"
# --- NEW: Explicitly set renv.paths.root R option using direct shell substitution ---
# This guarantees renv sees the correct path as a literal string.
Rscript -e "options(renv.consent = TRUE, renv.paths.root = '${RENV_PATHS_ROOT}'); setwd('${RENV_PROJECT}'); renv::restore()"
echo "Renv restore complete. Packages should now be linked from cache."

# --- NEW: More robust verification of renv cache path from within R ---
# This directly checks the R option, bypassing the problematic renv::paths$cache() call.
Rscript -e "if (requireNamespace('renv', quietly = TRUE)) { cat('renv cache path (from R option): ', getOption('renv.paths.root'), '\n') } else { cat('renv package not loaded in R.\n') }"


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

echo "Finished R script for task ID: ${SLURM_ARRAY_TASK_ID}"
