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
RANLOG="${MAINDIR}/rancodes.txt"

MAX_JOBS=6

dos2unix "${MAINDIR}/genus_species.txt"

# Ensure rancodes.txt exists
touch "$RANLOG"

#  OUTER LOOP
while IFS=$'\t' read -r genus species; do
    organism="${genus}_${species}"
    echo "Processing: $organism"

    train_file="${DATASET_DIR}/train_codes/train_codes_${organism}.txt"
    test_file="${DATASET_DIR}/test_codes/test_codes_${organism}.txt"

    dos2unix "$train_file"
    dos2unix "$test_file"

    # INNER LOOP 1: TRAINING
    while IFS= read -r code || [ -n "$code" ]; do

        RESDIR="${MAINDIR}/Results/${code}"
        refseq_check=$(find "${RESDIR}/refseq_mapping" -name "*.mapstat" 2>/dev/null | wc -l)
        resfinder_check=$(find "${RESDIR}/resfinder_mapping" -name "*.mapstat" 2>/dev/null | wc -l)

        if grep -Fxq "$code" "$RANLOG"; then
            echo "Skipping $code (already in rancodes.txt)"
            continue
        fi

        if [ "$refseq_check" -gt 0 ] && [ "$resfinder_check" -gt 0 ]; then
            echo "Skipping $code (mapstats exist)"
            echo "$code" >> "$RANLOG"
            continue
        fi

        # Wait while job count is too high OR personal storage usage exceeds 400 GB
        while true; do
			active_jobs=$(squeue -u $USER | tail -n +2 | wc -l)

			# Extract your current orange storage usage (user-level only)
			#usage=$(orange_quota | awk '/Storage quotas for user/,/^$/ { if ($1 == "orange") { gsub("G","",$2); print int($2) } }')

			#limit=400

			if [ "$active_jobs" -lt "$MAX_JOBS" ]; then #&& [ "$usage" -lt "$limit" ]; then
				break
			fi

			echo "$(date): Waiting — jobs: $active_jobs" #, usage: ${usage}G / ${limit}G"
			sleep 60
		done
		
        sbatch --job-name="${organism}_${code}" \
               --output="${MAINDIR}/logs/output_${organism}_${code}_%j.out" \
               "$LOOP_SCRIPT" "$code" "$organism"

        echo "$code" >> "$RANLOG"
        sleep 5
    done < "$train_file"

    # INNER LOOP 2: TESTING
    while IFS= read -r code || [ -n "$code" ]; do

        RESDIR="${MAINDIR}/Results/${code}"
        refseq_check=$(find "${RESDIR}/refseq_mapping" -name "*.mapstat" 2>/dev/null | wc -l)
        resfinder_check=$(find "${RESDIR}/resfinder_mapping" -name "*.mapstat" 2>/dev/null | wc -l)

        if grep -Fxq "$code" "$RANLOG"; then
            echo "Skipping $code (already in rancodes.txt)"
            continue
        fi

        if [ "$refseq_check" -gt 0 ] && [ "$resfinder_check" -gt 0 ]; then
            echo "Skipping $code (mapstats exist)"
            echo "$code" >> "$RANLOG"
            continue
        fi

        while true; do
			active_jobs=$(squeue -u $USER | tail -n +2 | wc -l)

			# Extract your current orange storage usage (user-level only)
			#usage=$(orange_quota | awk '/Storage quotas for user/,/^$/ { if ($1 == "orange") { gsub("G","",$2); print int($2) } }')

			#limit=400

			if [ "$active_jobs" -lt "$MAX_JOBS" ]; then #&& [ "$usage" -lt "$limit" ]; then
				break
			fi

			echo "$(date): Waiting — jobs: $active_jobs" #, usage: ${usage}G / ${limit}G"
			sleep 60
		done

        sbatch --job-name="${organism}_${code}" \
               --output="${MAINDIR}/logs/output_${organism}_${code}_%j.out" \
               "$LOOP_SCRIPT" "$code" "$organism"

        echo "$code" >> "$RANLOG"
        sleep 5
    done < "$test_file"

done < "${MAINDIR}/genus_species.txt"

date
