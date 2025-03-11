#!/bin/sh
#SBATCH --mail-type=ALL                        # Mail events
#SBATCH --mail-user=owenvisser@ufl.edu         # Where to send email
#SBATCH --ntasks=1                             # Run on a single machine (node)
#SBATCH --mem-per-cpu=1gb                      # Memory limit
#SBATCH --time=01:00:00                        # Time: hrs:min:sec

pwd; hostname; date 

first_accession="$1"

## Load required modules for downloading accession codes
module load ncbi_cli/15.3.1
module load sra/3.0.8

## Download the accession
attempts=10
for ((i=1; i<=attempts; i++)); do
    datasets download genome accession "$first_accession" --include genome,protein,cds,gff3,gbff,rna --filename "/home/owenvisser/CAMDA2025/fna_data/${first_accession}.zip" && break
    echo "Download failed. Retrying ($i/$attempts)..."
    sleep 30  # Wait before retrying
done

## Extract the fna file out of the ZIP file
unzip -o "/home/owenvisser/CAMDA2025/fna_data/${first_accession}.zip" \
    "ncbi_dataset/data/${first_accession}/*" -d "/home/owenvisser/CAMDA2025/fna_data/${first_accession}_extracted/"

## Move the fna file into the output folder
mv "/home/owenvisser/CAMDA2025/fna_data/${first_accession}_extracted/ncbi_dataset/data/${first_accession}" "/home/owenvisser/CAMDA2025/fna_data/"


## Remove pointless files
rm "/home/owenvisser/CAMDA2025/fna_data/${first_accession}/${first_accession}"*".fna"
rm -rf "/home/owenvisser/CAMDA2025/fna_data/${first_accession}.zip" "/home/owenvisser/CAMDA2025/fna_data/${first_accession}_extracted"
