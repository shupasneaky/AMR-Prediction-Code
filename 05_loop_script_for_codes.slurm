#!/bin/sh
#SBATCH --job-name=loopscrip
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=owenvisser@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=3
#SBATCH --mem-per-cpu=4gb
#SBATCH --time=02:00:00
#SBATCH --output=/orange/somnath.datta/CAMDA2025/logs/loop_submit_%j.out

pwd; hostname; date

max_cpus=4

# FOR TESTING:
# sbatch 05_loop_script_for_codes.slurm SRR3137575 Acinetobacter_baumannii

# Get inputs from outer loop
organism="$2"
code="$1"

# ----------------------
# File Path Definitions
# ----------------------

# Main location path
MAINDIR="/orange/somnath.datta/CAMDA2025" 
# Where the results will go for this code
RESDIR="${MAINDIR}/Results/${code}"  
    
# ----------------------
# DOWNLOAD FILES CHECKS
# ----------------------

# Load required modules
module load ncbi_cli/15.3.1
module load sra/3.0.8

# Get HTML from NCBI SRA page
page=$(curl -s "https://www.ncbi.nlm.nih.gov/sra/?term=${code}")

# Extract size (e.g., "253.8Mb" or "1.2Gb")
size=$(echo "$page" | grep -oP '<td align="right">\K[0-9\.]+[MG]b(?=</td>)' | head -n 1)
size_num=$(echo "$size" | grep -oE '[0-9\.]+')
unit=$(echo "$size" | grep -oE '[MG]b')

# Convert size to GB
if [ "$unit" = "Mb" ]; then
    size_gb=$(awk "BEGIN {print $size_num / 1024}")
else
    size_gb="$size_num"
fi

echo "Detected size: $size_gb GB"

# Exit if size > 2 GB
if (( $(echo "$size_gb > 2" | bc -l) )); then
    echo "ERROR: File is larger than 2 GB. Exiting. Will handle later"
    exit 1
fi

# Extract layout (e.g., "PAIRED" or "SINGLE")
layout=$(echo "$page" | grep -oP 'Layout: <span>\K[^<]+')

if [ "$layout" = "SINGLE" ]; then
	echo echo "Single Layout, will handle later"
	exit 1
fi

echo "Detected layout: $layout"

# ----------------------
# DOWNLOAD (FASTERQ)
# ----------------------

# Directory for downloads
RAWDIR="${RESDIR}/rawfastq"
mkdir -p "$RAWDIR/$code"

# Load required modules
module load sra/3.0.8

# Attempt fasterq-dump up to 10 times
echo "Downloading and extracting with fasterq-dump..."
attempts=10
for ((i=1; i<=attempts; i++)); do
	fasterq-dump "$code" \
	  --split-files \
	  --outdir "$RAWDIR/$code" \
	  --progress && break
    echo "fasterq-dump failed. Retrying ($i/$attempts)..."
    sleep 10
done

# Determine file references based on layout
fq_dir="${RAWDIR}/${code}"

if [ "$layout" = "PAIRED" ]; then
    file1="${fq_dir}/${code}_1.fastq"
    file2="${fq_dir}/${code}_2.fastq"
elif [ "$layout" = "SINGLE" ]; then
	echo "Combining single-end FASTQ files..."
	combined="${fq_dir}/${code}_combined.fastq"
	cat "${fq_dir}/${code}"*.fastq > "$combined"
	find "$fq_dir" -maxdepth 1 -name "${code}*.fastq" ! -name "${code}_combined.fastq" -delete
	file1="$combined"
	file2=""
else
    echo "ERROR: Unrecognized layout type '$layout'. Exiting."
    exit 1
fi


# ----------------------
# FASTQC FOR FILE QUALITY
# ----------------------

# fastqc plots will go here
QCDIR="${RESDIR}/qc_logs"
mkdir -p "$QCDIR"

# Load required modules
module load fastqc/0.12.1

# Run fastqc for appropriate layout
echo "Running FastQC..."
if [ "$layout" = "PAIRED" ]; then
    fastqc -o "$QCDIR" "$file1" "$file2"
elif [ "$layout" = "SINGLE" ]; then
    fastqc -o "$QCDIR" "$file1"
fi

# Drop the zips
rm -f "$QCDIR/"*.zip

# ----------------------
# REMOVE HUMAN PARTS
# ----------------------

# kneaddata results will go here
KNEADDIR="${RESDIR}/kneaddata"
mkdir -p "$KNEADDIR"

# Remove modules we aren't using anymore and get the preprocessing mods
module purge
module load trimmomatic/0.39
module load bowtie2/2.5.4
module load kneaddata/0.12.0

# Run the preprocessing
echo "Running kneaddata..."

if [ "$layout" = "PAIRED" ]; then
  kneaddata -i1 "$file1" -i2 "$file2" -o "$KNEADDIR" \
    -db /home/owenvisser/HumanReference \
    --trimmomatic ~/trimmomatic \
    --bowtie2 /apps/kneaddata/0.12.0/bin/ \
    -t ${max_cpus} \
    --trimmomatic-options "ILLUMINACLIP:~/trimmomatic/adapters/NexteraPE-PE.fa:2:30:10:8:keepBothReads SLIDINGWINDOW:4:30 MINLEN:60" \
    --bowtie2-options "--very-sensitive --dovetail"

elif [ "$layout" = "SINGLE" ]; then
  kneaddata -un "$file1" -o "$KNEADDIR" \
    -db /home/owenvisser/HumanReference \
    --trimmomatic ~/trimmomatic \
    --bowtie2 /apps/kneaddata/0.12.0/bin/ \
    -t ${max_cpus} \
	--trimmomatic-options "SLIDINGWINDOW:4:30 MINLEN:60"
else
  echo "ERROR: Invalid layout '$layout' for kneaddata. Exiting."
  exit 1
fi

# Check if kneaddata produced any FASTQ files
if ! ls "${KNEADDIR}"/*.fastq 1> /dev/null 2>&1; then
  echo "ERROR: kneaddata did not produce any FASTQ files. Exiting."
  exit 1
fi

echo "Finished kneaddata."

# Remove earlier downloaded fastq files, they aren't useful anymore
rm -rf "$RAWDIR"

# Save kneaddata log with the html from fastqc
mv "${KNEADDIR}/${code}"*_kneaddata.log "${QCDIR}/${code}.log"

# ----------------------
# REPAIR FASTQ
# ----------------------

# this is where we will store the repaired fastq files
REPAIRED_DIR="${RESDIR}/repaired_fastq"
mkdir -p "$REPAIRED_DIR"

# Load required modules
module purge
module load bbmap/39.06
module load kma/20230106

export _JAVA_OPTIONS="-Xmx4g"

echo "Repairing kneaded fastq..."
base="${KNEADDIR}/${code}_1_kneaddata_paired"
repair.sh -Xmx4g in1="${base}_1.fastq" in2="${base}_2.fastq" \
  out1="${REPAIRED_DIR}/${code}_ORDERED_1.fastq" \
  out2="${REPAIRED_DIR}/${code}_ORDERED_2.fastq" \
  outs="${REPAIRED_DIR}/${code}_ORDERED_singleton.fastq" repair
echo "Repaired"

# remove singleton file and kneaddata files, no longer kneaded -GET IT??-
rm -f "${REPAIRED_DIR}"/*_singleton.fastq
rm -rf "${KNEADDIR}"

# ----------------------
# FASTQC FOR FILE QUALITY
# ----------------------

# fastqc plots will go here
QCDIR2="${RESDIR}/qc_logs_repaired"
mkdir -p "$QCDIR2"

# Load required modules
module purge
module load fastqc/0.12.1

# Run fastqc for some plots and put in QCDIR
echo "Running FastQC..."
fastqc -o "$QCDIR2" "${REPAIRED_DIR}/${code}_ORDERED_1.fastq" "${REPAIRED_DIR}/${code}_ORDERED_2.fastq"

# Drop the zips
rm -f "$QCDIR2/"*.zip

# ----------------------
# REFSEQ MAPPING
# ----------------------

module purge
module load bbmap/39.06
module load kma/20230106

# Locate the reference genome file for this organism
ref_path=$(find "$MAINDIR/RefGenomes/$organism" -name "*.fna" | head -n 1)

# RefSeq index path (e.g., /CAMDA2025/RefSeq/kma_index/Escherichia_coli_genome)
res_path=$(find "$MAINDIR/RefSeq/kma_index" -type f -name "${organism}_genome.length.b" | sed 's/\.length\.b$//' | head -n 1)

# Folder for storing RefSeq mapping results
REFSEQDIR="${RESDIR}/refseq_mapping"
mkdir -p "$REFSEQDIR"

# Run KMA alignment to RefSeq
refseq_out="${REFSEQDIR}/${code}_${organism}_refseq"

echo "Running KMA alignment to RefSeq..."
kma -ipe "${REPAIRED_DIR}/${code}_ORDERED_1.fastq" "${REPAIRED_DIR}/${code}_ORDERED_2.fastq" \
    -t_db "$res_path" \
    -t ${max_cpus} \
    -o "$refseq_out" \
    -1t1 -ef -verbose

# Clean .mapstat header
sed -i '7s/^..//' "${refseq_out}.mapstat"

# ----------------------
# RESFINDER MAPPING
# ----------------------

RESFINDERDIR="${RESDIR}/resfinder_mapping"
mkdir -p "$RESFINDERDIR"

echo "Running ResFinder mapping for $code..."

for dbID in $(ls "$MAINDIR/ResFinder/kma_index"/*.length.b | sed -e 's/.*\///' -e 's/.length.b//' | sort -u); do
    res_out="${RESFINDERDIR}/${code}_${dbID}"
    echo "Mapping $code to AMR gene class: $dbID"

    kma -ipe "${REPAIRED_DIR}/${code}_ORDERED_1.fastq" "${REPAIRED_DIR}/${code}_ORDERED_2.fastq" \
        -t_db "$MAINDIR/ResFinder/kma_index/${dbID}" \
        -t ${max_cpus} \
        -o "$res_out" \
        -1t1 -ef -verbose

    rm -f "${res_out}.aln" "${res_out}.res" "${res_out}.fsa" "${res_out}.frag.gz"
    sed -i '7s/^..//' "${res_out}.mapstat"
done

echo "Completed ResFinder mapping."

# ----------------------
# REMOVE CLEANED FASTQ AND EXTRA KMA FILES
# ----------------------

echo "Cleaning unnecessary KMA files from mapping folders..."

find "$REFSEQDIR" -type f ! -name "*.mapstat" -delete
find "$RESFINDERDIR" -type f ! -name "*.mapstat" -delete

echo "Removing cleaned FASTQ files"
rm -rf "${REPAIRED_DIR}"

echo "Done with ${code}"

date
