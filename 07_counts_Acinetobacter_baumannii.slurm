#!/bin/bash
#SBATCH --job-name=all_counts   # Job name
#SBATCH --mail-type=ALL                # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=owenvisser@ufl.edu # Where to send mail	
#SBATCH --ntasks=1                     # Run on a single CPU
#SBATCH --mem=1gb                     # Job memory request
#SBATCH --time=0:30:00                # Time limit hrs:min:sec
#SBATCH --output=counts_%j.log        # Standard output and error log
pwd; hostname; date

module load R

echo "Running counts on all Acinetobacter baumannii"

R CMD BATCH 07_counts_Acinetobacter_baumannii.R ## the file to run

date