#!/bin/bash
#SBATCH --job-name=R_array_job_diag
#SBATCH --array=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -o "/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/out/R_job_%A_%a.out"
#SBATCH -e "/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/err/R_job_%A_%a.err"

# Exit on any error (-e) and print commands as they are executed (-x)
set -ex

# --- 1. Log Basic Information ---
echo "## DIAGNOSTIC START ##"
echo "Running on host: $(hostname)"
echo "Running as user: $(whoami)"
echo "Task ID: $SLURM_ARRAY_TASK_ID"
echo "Output file path: $SLURM_SUBMIT_DIR/R_job_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.out"
echo ""

# --- 2. Check Filesystem and Quotas ---
# Define the directory to check
TARGET_DIR="/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/"
echo "## CHECKING FILESYSTEM ##"
df -h "$TARGET_DIR"
echo ""
echo "## CHECKING QUOTAS ##"
quota -s
echo ""


# --- 3. Test a Direct Write Operation ---
echo "## TESTING DIRECT WRITE ##"
# Try creating a brand new file directly in the out directory
TEST_FILE="/atlantisarchive/Joseph.Caracappa/catch_thresholds_eof_3/out/direct_write_test_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.txt"
echo "Attempting to write to: $TEST_FILE"

# This command attempts the write and checks its exit code
echo "This is a direct write test." > "$TEST_FILE"
if [ $? -eq 0 ]; then
    echo "Direct write SUCCESSFUL."
else
    echo "Direct write FAILED with exit code $?."
fi
echo ""

# --- 4. Final test of standard output ---
echo "## FINAL STDOUT TEST ##"
echo "This is the final line sent to standard output."

echo "## DIAGNOSTIC END ##"

# A sleep is still a good idea, just in case.
sleep 1