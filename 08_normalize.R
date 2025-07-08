rm(list = ls())

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ##
## Create the abundance table
## merge results from individual mapstat files
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ##
library(tidyverse)
library(data.table)
library(rentrez)
#BiocManager::install("metagenomeSeq")
library(metagenomeSeq)
library(readxl)

# # Fixed function to fetch strain name
# get_strain_name <- function(accession) {
#   out <- tryCatch({
#     sumres <- entrez_summary(db = "nuccore", id = accession)
#     title <- sumres$title
#     if (is.null(title) || length(title) == 0) return(NA)
#     return(title)
#   }, error = function(e) return(NA))
#   
#   return(out)
# }
# 
# # Function to clean strain name
# clean_strain_name <- function(name) {
#   name <- gsub("^Acinetobacter baumannii strain ", "", name)
#   name <- gsub("^Acinetobacter baumannii ", "", name)
#   name <- gsub(",? ?(complete sequence|complete genome|chromosome.*|plasmid.*)", "", name)
#   name <- trimws(name)
#   return(name)
# }


setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/Results")
gs <- read.table(file = "C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/genus_species.txt") %>%
  mutate("bact" = paste0(V1, '_', V2)) %>%
  select(bact) %>%
  unlist()

for(i in gs){
  
  files <- list.files(pattern = i)
  
  amr_count <- read.csv(files[str_detect(files, "AMR_counts_train")]) %>%
    column_to_rownames("sample")
  
  otu_count <- read.csv(files[str_detect(files, "OTU_counts_train")]) %>%
    column_to_rownames("sample")
  
  # strain_code <- sub("^([^.]+\\.[^.]+)\\..*$", "\\1", colnames(otu_count))
  # 
  # # Apply to your cleaned codes
  # strain_names <- sapply(strain_code, get_strain_name)
  # 
  # # Apply to the Strain_Name column
  # colnames(otu_count) <- sapply(strain_names, clean_strain_name)
  # 
  # Filter OTUs (columns) with low total counts
  otu_count <- otu_count[, colSums(otu_count) >= 10]
  
  # Filter OTUs not present in enough samples (1% of all samples)
  otu_count <- otu_count[, colSums(otu_count > 0) >= nrow(otu_count) * 0.01]
  
  # Filter samples (rows) with ≤ 1 OTU present
  otu_count <- otu_count[rowSums(otu_count > 0) > 1, ]
  
  # Transpose: metagenomeSeq needs features as rows
  otu_mat <- t(otu_count)
  
  # Normalize with metagenomeSeq
  otu_mrexp <- newMRexperiment(otu_mat)
  p_otu <- cumNormStatFast(otu_mrexp)
  otu_norm <- cumNorm(otu_mrexp, p = p_otu)
  otu_norm_log <- MRcounts(otu_norm, norm = TRUE, log = TRUE) %>% t()
  
  # AMR Normalization with metagenomeSeq
  # Filter samples (rows) with ≤ 1 AMR present
  amr_count <- amr_count[rowSums(amr_count > 0) > 1, ]
  
  amr_mat <- t(amr_count)
  amr_mrexp <- newMRexperiment(amr_mat)
  p_amr <- cumNormStatFast(amr_mrexp)
  amr_norm <- cumNorm(amr_mrexp, p = p_amr)
  amr_norm_log <- MRcounts(amr_norm, norm = TRUE, log = TRUE) %>% t()
  
  write.csv(amr_norm_log, file = paste0("amr_norm_train_", i,".csv"))
  write.csv(otu_norm_log, file = paste0("otu_norm_train_", i,".csv"))
}

