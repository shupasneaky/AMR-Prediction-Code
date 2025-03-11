#!/bin/sh
#SBATCH --job-name=batch_ncbi_submit             # Job name
#SBATCH --mail-type=ALL                          # Mail events
#SBATCH --mail-user=owenvisser@ufl.edu          # Email notifications
#SBATCH --ntasks=1                               # Run on a single node
#SBATCH --cpus-per-task=1                        # Only need 1 CPU per submission
#SBATCH --mem=500mb                              # Minimal memory per submission
#SBATCH --time=34:00:00                          # Short run-time per job submission
#SBATCH --output=/home/owenvisser/CAMDA2025/logs/batch_submit_%j.out            # Output and error log

pwd; hostname; date

dos2unix /home/owenvisser/CAMDA2025/train_fna.txt

# Loop through each accession code in train_fna.txt and submit a job for each
while IFS= read -r first_accession || [ -n "$first_accession" ]; do
	sbatch --job-name="$first_accession" --output="/home/owenvisser/CAMDA2025/logs/output_${first_accession}_%j.out" fna_loop_script.slurm "$first_accession"
    sleep 20
done </home/owenvisser/CAMDA2025/train_fna.txt
