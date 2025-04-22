#!/bin/sh
#SBATCH --job-name=batch_ncbi_submit
#SBATCH --mail-type=ALL
#SBATCH --mail-user=owenvisser@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=500mb
#SBATCH --time=1:00:00
#SBATCH --output=/orange/somnath.datta/CAMDA2025/logs/batch_submit_%j.out

module load ncbi_cli/15.3.1

GENOME_DIR="/orange/somnath.datta/CAMDA2025/RefGenomes"
mkdir -p "$GENOME_DIR"

while IFS=$'\t' read -r genus species; do
    organism="${genus}_${species}"
    echo "Downloading reference genome + GFF for: $organism"

    output_zip="${GENOME_DIR}/${organism}.zip"

    attempts=5
    for ((i=1; i<=attempts; i++)); do
		datasets download genome taxon "$genus $species" \
			--assembly-level complete \
			--assembly-source RefSeq \
			--reference \
			--include genome,gff3 \
			--filename "$output_zip" && break
        echo "Download failed for $organism. Retrying ($i/$attempts)..."
        sleep 20
    done

    if [[ -f "$output_zip" ]]; then
        unzip -o -d "${GENOME_DIR}/${organism}" "$output_zip"
        rm "$output_zip"

        # Merge genome FASTAs
        find "${GENOME_DIR}/${organism}" -name "*.fna" -exec cat {} + > "${GENOME_DIR}/${organism}/${organism}_genome.fna"
        find "${GENOME_DIR}/${organism}" -name "*.fna" ! -name "${organism}_genome.fna" -delete

        # Merge GFF files
        find "${GENOME_DIR}/${organism}" -name "*.gff" -exec cat {} + > "${GENOME_DIR}/${organism}/${organism}_genes.gff"
        find "${GENOME_DIR}/${organism}" -name "*.gff" ! -name "${organism}_genes.gff" -delete
    else
        echo "No reference genome found for $organism"
    fi

done < /orange/somnath.datta/CAMDA2025/genus_species.txt

echo "Finished downloading and merging reference genome FASTAs + GFFs."
date
