#!/bin/bash
#SBATCH --job-name=ensemble   # Job name
#SBATCH --mail-type=ALL                # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=owenvisser@ufl.edu # Where to send mail	
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem-per-cpu=1gb
#SBATCH --time=20:00:00
#SBATCH --output=ensemble_%j.log        # Standard output and error log

pwd; hostname; date

module load R

echo "Running ensemble on all bact"

R CMD BATCH 10_Ensemble.R ## the file to run

date
