#!/bin/bash
#SBATCH --job-name=Download_ResFinder
#SBATCH --mail-type=ALL
#SBATCH --mail-user=owenvisser@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=1gb
#SBATCH --time=01:00:00
#SBATCH --output=/orange/somnath.datta/CAMDA2025/logs/resfinder_download_%j.out

# Load modules
module load kma/20230106
module load git/2.30.1

# Define paths
WORKDIR="/orange/somnath.datta/CAMDA2025/ResFinder"
REFDIR="${WORKDIR}/raw_fsa"
INDEXDIR="${WORKDIR}/kma_index"

mkdir -p "$REFDIR" "$INDEXDIR"
cd "$WORKDIR"

# Clone or update the ResFinder gene database
if [ -d "resfinder_db" ]; then
    echo "Updating existing ResFinder DB..."
    cd resfinder_db
    git pull
    cd ..
else
    echo "Cloning ResFinder DB..."
    git clone https://bitbucket.org/genomicepidemiology/resfinder_db.git
fi

# Copy all .fsa files to the raw_fsa directory
cp resfinder_db/*.fsa "$REFDIR"

# Index each .fsa file using KMA
cd "$REFDIR"
echo "Indexing ResFinder .fsa files with KMA..."
for file in *.fsa; do
    base=$(basename "$file" .fsa)
    echo "Indexing $base..."
    kma index -i "$file" -o "${INDEXDIR}/${base}"
done

echo "ResFinder download and KMA indexing complete."
date
