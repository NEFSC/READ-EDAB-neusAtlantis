#!/bin/bash
#SBATCH --job-name=R_array_job
#SBATCH --array=59,60,63,65,67,69,71,76,79,84,99
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
##SBATCH -o "/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/out/R_job_%A_%a.out"
##SBATCH -e "/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/err/R_job_%A_%a.err"

# Define variables for clarity and reusability
EXPERIMENT_NAME="catch_thresholds_eof_3"
PROJECT_DIR='/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
OUTPUT_BASE_DIR="/atlantisarchive/$EXPERIMENT_NAME"

OUT_DIR="$OUTPUT_BASE_DIR/out"
ERR_DIR="$OUTPUT_BASE_DIR/err"

# Test 1: Check the parent directory of the output folders
echo "Checking existence and permissions of $OUTPUT_BASE_DIR..."
ls -ld "$OUTPUT_BASE_DIR"

# Test 2: Attempt to create the output directories
echo "Attempting to create directories..."
mkdir -p "$OUT_DIR"
mkdir -p "$ERR_DIR"
echo "Directories created successfully." # This will only print if mkdir succeeds

# Test 3: Continue with the rest of the script
echo "Continuing with rest of script."

# Wait for the necessary input file to be created.
RUN_DIR_TO_CHECK="/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/catch_thresholds_eof_3_${SLURM_ARRAY_TASK_ID}"
# echo "Waiting for directory: $RUN_DIR_TO_CHECK"

# while [ ! -d "$RUN_DIR_TO_CHECK" ]; do
#     echo "Directory not found. Retrying in 5 seconds..."
#     sleep 5
# done

# echo "Directory found. Listing its contents."
# ls -l "$RUN_DIR_TO_CHECK"

cd "$PROJECT_DIR"
Rscript R/EOF/call_process_eco_state.R "$SLURM_ARRAY_TASK_ID"