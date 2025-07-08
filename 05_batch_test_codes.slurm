#!/bin/sh
#SBATCH --job-name=batch_ncbi_submit
#SBATCH --mail-type=ALL
#SBATCH --mail-user=owenvisser@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=100mb
#SBATCH --time=400:00:00
#SBATCH --output=/orange/somnath.datta/CAMDA2025/logs/batch_submit_%j.out

pwd; hostname; date

#  PATH SETUP 
MAINDIR="/orange/somnath.datta/CAMDA2025" 
DATASET_DIR="${MAINDIR}/Datasets"
LOOP_SCRIPT="${MAINDIR}/05_loop_script_for_codes.slurm"
TESTLOG="${MAINDIR}/rancodes_test.txt"

MAX_JOBS=5

dos2unix "${MAINDIR}/genus_species.txt"

# Ensure rancodes.txt exists
touch "$TESTLOG"

# OUTER LOOP
while IFS=$'\t' read -r genus species || [ -n "$genus" ]; do
    organism="${genus}_${species}"
    echo "Processing: $organism"

    test_file="${DATASET_DIR}/test_codes/test_codes_${organism}.txt"
    dos2unix "$test_file"

    # INNER LOOP: TESTING
    while IFS= read -r code || [ -n "$code" ]; do

		# avoid duplicate codes
		if grep -Fxq "$code" "$TESTLOG"; then
            echo "Skipping $code (already in rancodes.txt)"
            continue
        fi
		
		# check if map files exist for both refseq and resfinder
		RESDIR="${MAINDIR}/Results/${code}"		
        refseq_check=$(find "${RESDIR}/refseq_mapping" -name "*.mapstat" 2>/dev/null | wc -l)
        resfinder_check=$(find "${RESDIR}/resfinder_mapping" -name "*.mapstat" 2>/dev/null | wc -l)
        if [ "$refseq_check" -gt 0 ] && [ "$resfinder_check" -gt 0 ]; then
            echo "Skipping $code (mapstats exist)"
            echo "$code" >> "$TESTLOG"
            continue
        fi

        # check if we are running too many jobs - if so, wait
        while true; do
			active_jobs=$(squeue -u $USER | tail -n +2 | wc -l)
			if [ "$active_jobs" -lt "$MAX_JOBS" ]; then 
				break
			fi
			echo "$(date): Waiting — jobs: $active_jobs" 
			sleep 60
		done
		
		sbatch --job-name="${organism}_${code}" \
				--output="${MAINDIR}/logs/output_${organism}_${code}_%j.out" \
				"$LOOP_SCRIPT" "$code" "$organism"
			   
		echo "$code" >> "$TESTLOG"
		sleep 1
		
    done < "$test_file"

done < "${MAINDIR}/genus_species.txt"

date
