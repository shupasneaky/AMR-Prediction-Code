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
module load prokka/1.14.6

## Download the accession
attempts=5
for ((i=1; i<=attempts; i++)); do
    datasets download genome accession "$first_accession" --filename "/home/owenvisser/CAMDA2025/fna_data/${first_accession}.zip" && break
    echo "Download failed. Retrying ($i/$attempts)..."
    sleep 30  # Wait before retrying
done

## Extract the fna file out of the ZIP file
unzip -o "/home/owenvisser/CAMDA2025/fna_data/${first_accession}.zip" \
    "ncbi_dataset/data/${first_accession}/*.fna" -d "/home/owenvisser/CAMDA2025/fna_data/${first_accession}_extracted/"

## Move the fna file into the output folder
mv "/home/owenvisser/CAMDA2025/fna_data/${first_accession}_extracted/ncbi_dataset/data/${first_accession}" "/home/owenvisser/CAMDA2025/fna_data/"

## Label the name so we can reference in future functions
fna_file=$(find "/home/owenvisser/CAMDA2025/fna_data/${first_accession}/" -name "*.fna")

## Convert to unix
dos2unix "$fna_file"

## Run Prokka for annotation, keeping only tbl and tsv
prokka --outdir "/home/owenvisser/CAMDA2025/fna_data/${first_accession}" \
       --prefix "${first_accession}" \
       --force \
       --usegenus \
       --metagenome \
       "$fna_file"

## Remove all files except tbl, tsv, and fna
find "/home/owenvisser/CAMDA2025/fna_data/${first_accession}" \! -name "${first_accession}.tbl" \! -name "${first_accession}.tsv" \! -name "*.fna" -type f -delete

rm -f "$fna_file"
rm -rf "/home/owenvisser/CAMDA2025/fna_data/${first_accession}.zip" "/home/owenvisser/CAMDA2025/fna_data/${first_accession}_extracted"
