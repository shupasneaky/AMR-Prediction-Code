#!/bin/bash
#SBATCH --job-name=all_counts   # Job name
#SBATCH --mail-type=ALL                # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=owenvisser@ufl.edu # Where to send mail	
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=19
#SBATCH --mem-per-cpu=1gb
#SBATCH --time=02:00:00
#SBATCH --output=counts_%j.log        # Standard output and error log
pwd; hostname; date

module load R

echo "Running counts on all train codes"

R CMD BATCH 07_counts_test.R ## the file to run

date