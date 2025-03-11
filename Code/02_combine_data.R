#setwd(/home/owenvisser/CAMDA2025/fna_data/)


fna_codes = unlist(read.delim("Datasets/train_fna.txt", sep='\n', header=F))


folder_path = paste0(getwd(),"/Datasets/fna_data/",fna_codes[1],"/")
tsv_file <- read.delim(paste0(folder_path,fna_codes[1],".tsv"), sep = "\t")
