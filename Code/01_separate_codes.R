#===================================================================================================
# This code is used to get the accession codes out of the csv files from the CAMDA database
#   and to separate them into the fna files codes and the fastq file codes. Then, save them 
#   as csv files so I can reference them in HiperGator.
#===================================================================================================

# set paths
setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance")
data_path <- paste0(getwd(),'/Datasets/')

# import CSV files
train <- read.csv(paste0(data_path, 'TrainingDataset.csv'), header=TRUE)
test <- read.csv(paste0(data_path, 'TestingDataset.csv'), header=TRUE)

# training data accession codes
fastq_files <- grep(pattern = "SRR|ERR", x = train$accession, value = TRUE)
cat(fastq_files, file = paste0(data_path,"train_fastq.txt"), sep = '\n')
cat(setdiff(train$accession, fastq_files), file =  paste0(data_path,"train_fna.txt"), sep = '\n')

# testing data accession codes
fastq_files <- grep(pattern = "SRR|ERR", x = test$accession, value = TRUE)
cat(fastq_files, file =  paste0(data_path,"test_fastq.txt"), sep = '\n')
cat(setdiff(test$accession, fastq_files), file =  paste0(data_path,"test_fna.txt"), sep = '\n')

# check the files are there
list.files(data_path, pattern = ".txt")

