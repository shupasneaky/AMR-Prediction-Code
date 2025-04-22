rm(list = ls())
library(tidyverse)
library(data.table)
library(metagenomeSeq)

# Define root path
count_path <- "/orange/somnath.datta/CAMDA2025/"
root_path <- "/orange/somnath.datta/CAMDA2025/Results"
#root_path <- "C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/Code/Results"
code_list <- readLines("/orange/somnath.datta/CAMDA2025/Datasets/train_codes/train_codes_Acinetobacter_baumannii.txt")


# List all .mapstat files
all_mapstats <- list.files(path = root_path, pattern = "\\.mapstat$", recursive = TRUE, full.names = TRUE)

# Filter only files that match the codes in your list
mapstat_filtered <- all_mapstats[
  str_extract(basename(all_mapstats), "^SRR[0-9]+") %in% code_list
]

# Separate OTU (refseq) and AMR (resfinder)
refseq_files <- grep("refseq_mapping", mapstat_filtered, value = TRUE)
resfinder_files <- grep("resfinder_mapping", mapstat_filtered, value = TRUE)

# ------------------ OTU PROCESSING ------------------ #

otu_all <- list()

for (f in refseq_files) {
  sample_name <- str_extract(basename(f), "^SRR[0-9]+")
  df <- tryCatch(read.delim(f, sep = "\t", skip = 6, header = TRUE, quote = ""), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) next
  
  message(sample_name)
  
  accs <- str_extract(df$refSequence, "^[A-Za-z0-9_\\.-]+")
  accs <- str_remove(accs, "\\.[0-9]+$")  # removes trailing .1, .2, etc.
  
  message(accs)
  
  df <- df %>%
    mutate(accs = accs) %>%
    group_by(accs) %>%
    summarise(!!sample_name := sum(readCount), .groups = "drop") %>%
    filter(!is.na(accs))
  
  message(df)
  
  otu_all[[sample_name]] <- df
}


# Merge all OTU by accs
otu_table <- reduce(otu_all, full_join, by = "accs") %>%  mutate(across(-accs, ~replace_na(.x, 0)))
otu_table <- as.data.frame(otu_table)
rownames(otu_table) <- otu_table$accs
otu_table$accs <- NULL

otu_mat <- t(otu_table)  # transpose so samples = rows
otu_df <- data.frame(sample = rownames(otu_mat), otu_mat)

# Save preprocessed OTU
otu_out_path <- paste0(count_path, "OTU_counts_preproc.csv")
write.csv(otu_df, otu_out_path, row.names = FALSE)



# ------------------ AMR PROCESSING ------------------ #

amr_all <- list()

for (f in resfinder_files) {
  df <- tryCatch(read.delim(f, sep = "\t", skip = 6, header = TRUE, quote = ""), error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) next
  
  sample_name <- str_extract(basename(f), "^SRR[0-9]+")
  header <- tryCatch(readLines(f, 3)[3], error = function(e) "")
  amr_class <- str_split(header, "\t")[[1]][2]
  if (amr_class == "all") next
  
  tmp <- df %>%
    summarise(readCount = sum(readCount)) %>%
    mutate(sample = sample_name, class = amr_class)
  
  amr_all[[length(amr_all) + 1]] <- tmp
}

amr_df <- bind_rows(amr_all) %>%  pivot_wider(names_from = class, values_from = readCount, values_fill = 0)
amr_mat <- as.data.frame(amr_df)

# Save preprocessed AMR OTU
amr_out_path <- paste0(count_path, "AMR_counts_preproc.csv")
write.csv(amr_mat, amr_out_path, row.names = FALSE)
