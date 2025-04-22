#!/bin/bash
#SBATCH --job-name=Index_RefSeq
#SBATCH --mail-type=ALL
#SBATCH --mail-user=owenvisser@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=2
#SBATCH --mem=1gb
#SBATCH --time=01:00:00
#SBATCH --output=/orange/somnath.datta/CAMDA2025/logs/refseq_index_%j.out

# ----------------------
# Load KMA
# ----------------------
module load kma/20230106

# ----------------------
# Define paths
# ----------------------
MAINPATH="/orange/somnath.datta/CAMDA2025"
REFSEQ_FASTA="${MAINPATH}/RefGenomes"
INDEX_OUT="${MAINPATH}/RefSeq/kma_index"

mkdir -p "$INDEX_OUT"

# ----------------------
# Loop through organisms
# ----------------------
while IFS=$'\t' read -r genus species; do
    organism="${genus}_${species}"
    genome_fna="${REFSEQ_FASTA}/${organism}/${organism}_genome.fna"
    index_prefix="${INDEX_OUT}/${organism}_genome"

    if [[ -f "$genome_fna" ]]; then
        echo "Indexing $organism..."
        kma index -i "$genome_fna" -o "$index_prefix"
    else
        echo "Genome FASTA not found for $organism"
    fi
done < "${MAINPATH}/genus_species.txt"

echo "All available RefSeq genome indexing complete."
date
