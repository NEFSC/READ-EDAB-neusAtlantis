#!/bin/bash
#SBATCH --job-name=R_array_job
#SBATCH --array=1-660
#SBATCH --ntasks=1
##SBATCH --cpus-per-task=1
#SBATCH --partition=computelow
##SBATCH -o "/atlantisarchive/Joseph.Caracappa/eof_targeting_4/out/R_job_%A_%a.out"
##SBATCH -e "/atlantisarchive/Joseph.Caracappa/eof_targeting_4/err/R_job_%A_%a.err"

# Define variables for clarity and reusability
EXPERIMENT_NAME="eof_targeting_4"
PROJECT_DIR='/model/Joseph.Caracappa/READ-EDAB-neusAtlantis/'
OUTPUT_BASE_DIR="/atlantisarchive/Joseph.Caracappa/$EXPERIMENT_NAME"

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

cd "$PROJECT_DIR"
Rscript R/Cloud_Utilities/call_process_eco_state.R "$EXPERIMENT_NAME" "$PROJECT_DIR" "$OUTPUT_BASE_DIR" "$SLURM_ARRAY_TASK_ID" "$EXPERIMENT_NAME" "$RUN_DIR_TO_CHECK"