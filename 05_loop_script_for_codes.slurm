#!/bin/sh
#SBATCH --job-name=loopscrip
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=owenvisser@ufl.edu
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=9
#SBATCH --mem-per-cpu=6gb
#SBATCH --time=02:00:00
#SBATCH --output=/orange/somnath.datta/CAMDA2025/logs/loop_submit_%j.out

pwd; hostname; date

max_cpus=9

# FOR TESTING:
# sbatch 05_loop_script_for_codes.slurm "SRR3666035" "Acinetobacter_baumannii"

# Get inputs from outer loop
code="$1"
organism="$2"

# --------------------------------------------------------------------------
# File Path Definitions
# --------------------------------------------------------------------------

MAINDIR="/orange/somnath.datta/CAMDA2025" 
RESDIR="${MAINDIR}/Results/${code}"  

# Load required modules
module load ncbi_cli/15.3.1

# --------------------------------------------------------------------------
# DOWNLOAD (FASTERQ)
# --------------------------------------------------------------------------

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

# Determine layout and set file references
fq_dir="${RAWDIR}/${code}"
fastq_files=("$fq_dir"/*.fastq)

if [[ ${#fastq_files[@]} -eq 1 ]]; then
    layout="SINGLE"
    file1="${fastq_files[0]}"
    file2=""
elif [[ ${#fastq_files[@]} -eq 2 ]]; then
    layout="PAIRED"
    file1="${fastq_files[0]}"
    file2="${fastq_files[1]}"
fi

echo "Detected layout: $layout"

# --------------------------------------------------------------------------
# FASTQC FOR FILE QUALITY
# --------------------------------------------------------------------------

# fastqc plots will go here
QCDIR="${RESDIR}/qc_logs"
mkdir -p "$QCDIR"

# Load required modules
module load fastqc/0.12.1

# Run fastqc for appropriate layout
echo "Running FastQC..."
if [[ "$layout" == "SINGLE" ]]; then
    fastqc --extract -o "$QCDIR" "$file1"
else
    fastqc --extract -o "$QCDIR" "$file1" "$file2"
fi

# Define path to fastqc_data.txt
fastqc_data="${QCDIR}/$(basename "$file1" .fastq)_fastqc/fastqc_data.txt"

# Compute MINLEN from FastQC stats
if [[ -f "$fastqc_data" ]]; then
    total_seqs=$(awk '/Total Sequences/ {print $3}' "$fastqc_data")
    bases_val=$(awk '/Total Bases/ {print $3}' "$fastqc_data")
    unit=$(awk '/Total Bases/ {print $4}' "$fastqc_data")
    unit_upper=$(echo "$unit" | tr '[:lower:]' '[:upper:]')

	case "$unit_upper" in
		GBP) factor=1e9 ;;
		MBP) factor=1e6 ;;
		KBP) factor=1e3 ;;
		*) echo "Unknown unit: $unit"; exit 1 ;;
	esac

    bases_num=$(awk -v v="$bases_val" -v f="$factor" 'BEGIN { print v * f }')
    avg_len=$(awk -v b="$bases_num" -v s="$total_seqs" 'BEGIN { printf "%.0f", b / s }')
    MINLEN=$(awk -v a="$avg_len" 'BEGIN { print int(a * 0.6) }')

    echo "[$code] MINLEN: $MINLEN (avg read length: $avg_len)"
else
    echo "[$code] FastQC file not found: $fastqc_data"
    exit 1
fi

if [[ "$MINLEN" -lt 25 ]]; then
    MINLEN=25
fi

# Clean up zips and HTML reports
rm -f "$QCDIR"/*.zip
rm -f "$QCDIR"/*.html


# --------------------------------------------------------------------------
# REMOVE HUMAN PARTS
# --------------------------------------------------------------------------
KNEADDIR="${RESDIR}/kneaddata"
mkdir -p "$KNEADDIR"

# Remove modules we aren't using anymore and get the preprocessing mods
module purge
module load trimmomatic/0.39
module load bowtie2/2.5.4
module load kneaddata/0.12.0

# Run the preprocessing
echo "Running kneaddata..."

# Run fastqc for appropriate layout
if [[ "$layout" == "SINGLE" ]]; then
	 kneaddata -un "$file1" -o "$KNEADDIR" \
	   -db /home/owenvisser/HumanReference \
	   --trimmomatic ~/trimmomatic \
	   --bowtie2 /apps/kneaddata/0.12.0/bin/ \
	   -t ${max_cpus} \
	   -p 1 \
	   --max-memory 54000m \
	   --trimmomatic-options "ILLUMINACLIP:~/trimmomatic/adapters/TruSeq3-SE:2:30:10 SLIDINGWINDOW:4:20 MINLEN:${MINLEN}"
else
	 kneaddata -i1 "$file1" -i2 "$file2" -o "$KNEADDIR" \
	   -db /home/owenvisser/HumanReference \
	   --trimmomatic ~/trimmomatic \
	   --bowtie2 /apps/kneaddata/0.12.0/bin/ \
	   -t ${max_cpus} \
	   -p 1 \
	   --max-memory 54000m \
	   --trimmomatic-options "ILLUMINACLIP:~/trimmomatic/adapters/NexteraPE-PE.fa:2:30:10:8:keepBothReads SLIDINGWINDOW:4:20 MINLEN:${MINLEN}" \
	   --bowtie2-options "--very-sensitive --dovetail"
fi

echo "Finished kneaddata."

# Remove earlier downloaded fastq files, they aren't useful anymore
rm -rf "$RAWDIR"

# Save kneaddata log with the html from fastqc
mv "${KNEADDIR}/${code}"*_kneaddata.log "${QCDIR}/${code}.log"

# --------------------------------------------------------------------------
# REPAIR
# --------------------------------------------------------------------------
REPAIRED_DIR="${RESDIR}/repaired_fastq"
mkdir -p "$REPAIRED_DIR"

# Load required modules
module purge
module load bbmap/39.06
module load kma/20230106

export _JAVA_OPTIONS="-Xmx4g"

# Run fastqc for appropriate layout
echo "Repairing kneaded fastq..."
if [[ "$layout" == "SINGLE" ]]; then
	cp "${KNEADDIR}/${code}_kneaddata.fastq" "${REPAIRED_DIR}/${code}_ORDERED.fastq"
else

	base="${KNEADDIR}/${code}_1_kneaddata_paired"
	repair.sh -Xmx4g in1="${base}_1.fastq" in2="${base}_2.fastq" \
	  out1="${REPAIRED_DIR}/${code}_ORDERED_1.fastq" \
	  out2="${REPAIRED_DIR}/${code}_ORDERED_2.fastq" \
	  outs="${REPAIRED_DIR}/${code}_ORDERED_singleton.fastq" repair
	
	rm -f "${REPAIRED_DIR}"/*_singleton.fastq
fi
echo "Repaired"

# remove singleton file and kneaddata files
rm -rf "${KNEADDIR}"

# --------------------------------------------------------------------------
# FASTQC FOR FILE QUALITY
# --------------------------------------------------------------------------
QCDIR2="${RESDIR}/qc_logs_repaired"
mkdir -p "$QCDIR2"

# Load required modules
module purge
module load fastqc/0.12.1


# Run fastqc for appropriate layout
echo "Running FastQC..."
if [[ "$layout" == "SINGLE" ]]; then
    fastqc --extract -o "$QCDIR2" "${REPAIRED_DIR}/${code}_ORDERED.fastq"
else
    fastqc --extract -o "$QCDIR2" "${REPAIRED_DIR}/${code}_ORDERED_1.fastq" "${REPAIRED_DIR}/${code}_ORDERED_2.fastq"
fi

# Drop the zips
rm -f "$QCDIR2/"*.zip
rm -f "$QCDIR2/"*.html

# --------------------------------------------------------------------------
# REFSEQ MAPPING
# --------------------------------------------------------------------------
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


# Run fastqc for appropriate layout
echo "Running KMA alignment to RefSeq..."
if [[ "$layout" == "SINGLE" ]]; then
	kma -i "${REPAIRED_DIR}/${code}_ORDERED.fastq" \
		-t_db "$res_path" \
		-t ${max_cpus} \
		-o "$refseq_out" \
		-mem_mode \
		-1t1 -ef -verbose
else
	kma -ipe "${REPAIRED_DIR}/${code}_ORDERED_1.fastq" "${REPAIRED_DIR}/${code}_ORDERED_2.fastq" \
		-t_db "$res_path" \
		-t ${max_cpus} \
		-o "$refseq_out" \
		-mem_mode \
		-1t1 -ef -verbose
fi

# Clean .mapstat header
sed -i '7s/^..//' "${refseq_out}.mapstat"

# --------------------------------------------------------------------------
# RESFINDER MAPPING
# --------------------------------------------------------------------------
RESFINDERDIR="${RESDIR}/resfinder_mapping"
mkdir -p "$RESFINDERDIR"

echo "Running ResFinder mapping for $code..."
for dbID in $(ls "$MAINDIR/ResFinder/kma_index"/*.length.b | sed -e 's/.*\///' -e 's/.length.b//' | sort -u); do
    res_out="${RESFINDERDIR}/${code}_${dbID}"
    echo "Mapping $code to AMR gene class: $dbID"

	if [[ "$layout" == "SINGLE" ]]; then
		    kma -i "${REPAIRED_DIR}/${code}_ORDERED.fastq" \
			-t_db "$MAINDIR/ResFinder/kma_index/${dbID}" \
			-t ${max_cpus} \
			-o "$res_out" \
			-mem_mode \
			-1t1 -ef -verbose
	else
		kma -ipe "${REPAIRED_DIR}/${code}_ORDERED_1.fastq" "${REPAIRED_DIR}/${code}_ORDERED_2.fastq" \
			-t_db "$MAINDIR/ResFinder/kma_index/${dbID}" \
			-t ${max_cpus} \
			-o "$res_out" \
			-mem_mode \
			-1t1 -ef -verbose
	fi
    rm -f "${res_out}.aln" "${res_out}.res" "${res_out}.fsa" "${res_out}.frag.gz"
    sed -i '7s/^..//' "${res_out}.mapstat"
done
echo "Completed ResFinder mapping."

# --------------------------------------------------------------------------
# REMOVE CLEANED FASTQ AND EXTRA KMA FILES
# --------------------------------------------------------------------------

echo "Cleaning unnecessary KMA files from mapping folders..."

find "$REFSEQDIR" -type f ! -name "*.mapstat" -delete
find "$RESFINDERDIR" -type f ! -name "*.mapstat" -delete

echo "Removing cleaned FASTQ files"
rm -rf "${REPAIRED_DIR}"

echo "Done with ${code}"

date
