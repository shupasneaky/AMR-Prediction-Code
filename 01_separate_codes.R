#===================================================================================================
# This code is used to get the accession codes out of the csv files from the CAMDA database
#   and to separate them into the fna files codes and the fastq file codes. Then, save them 
#   as csv files so I can reference them in HiperGator.
#===================================================================================================


# set paths
setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance")
data_path <- paste0(getwd(),'/Datasets/')


# import CSV files
train <- read.csv(paste0(data_path, 'training_dataset.csv'), header=TRUE)
test <- read.delim(paste0(data_path, 'testing_dataset_reduced.csv'), header=TRUE)


# training data accession codes
train_file = data.frame(bact_type = as.factor(stringr::str_c(train$genus, train$species, sep=',')),
                        codes = as.character(train$accession))

for( level in levels(train_file$bact_type)){
  cat(train_file$codes[train_file$bact_type == level], file =  paste0(data_path,"train_codes_",level, ".txt") , sep = '\n')
}

# testing data accession codes
test_file = data.frame(bact_type = as.factor(stringr::str_c(test$genus, test$species, sep=',')),
                        codes = as.character(test$accession))

for( level in levels(test_file$bact_type)){
  cat(test_file$codes[test_file$bact_type == level], file =  paste0(data_path,"test_codes_",level, ".txt") , sep = '\n')
}

# check the files are there
list.files(data_path, pattern = ".txt")



train_file = data.frame(bact_type = as.factor(stringr::str_c(train$genus, train$species, sep='\t')),
                        codes = as.character(train$accession))

cat(levels(train_file$bact_type), file = "genus_species.txt", sep='\n')


# are they the same?
levels(train_file$bact_type) == levels(test_file$bact_type)

