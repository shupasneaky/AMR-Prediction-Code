rm(list = ls())

suppressPackageStartupMessages({
  library(tidyverse)
  library(data.table)
  library(metagenomeSeq)
})

# Define paths
base_path <- "/orange/somnath.datta/CAMDA2025"
code_path <- file.path(base_path, "Datasets/test_codes")
result_path <- file.path(base_path, "Results")
count_path <- base_path

# Get all test code files
code_files <- list.files(code_path, pattern = "\\.txt$", full.names = TRUE)

# Loop over each code_file (species)
for (code_file in code_files) {
  
  species_codes <- unique(readLines(code_file))
  species_name <- str_extract(basename(code_file), "(?<=test_codes_).+(?=\\.txt)")
  message("Processing: ", species_name)
  
  # Preallocate per-species OTU and AMR lists
  otu_list <- vector("list", length(species_codes))
  amr_list <- vector("list", length(species_codes) * 30)  # assume max 20 AMR classes per code
  otu_idx <- 1
  amr_idx <- 1
  
  for (code in species_codes) {
    message(which(species_codes == code), "/", length(species_codes), " ", species_name, "  Code: ", code)
    sample_name <- code
    base_dir <- file.path(result_path, code)
    
    if (otu_idx %% 50 == 0) gc()
    
    # Get mapping directories
    refseq_dir <- file.path(base_dir, "refseq_mapping")
    resfinder_dir <- file.path(base_dir, "resfinder_mapping")
    
    # OTU MAPSTAT FILE
    if (dir.exists(refseq_dir)) {
      otu_file <- list.files(refseq_dir, pattern = "\\.mapstat$", full.names = TRUE)
      dat <- tryCatch(fread(otu_file, skip = 6, quote = ""), error = function(e) NULL)
      if (is.null(dat) || nrow(dat) == 0) next
      
      accs <- sub("\\.[0-9]+$", "", sub("^([^\\s]+).*", "\\1", dat$refSequence))
      
      dat <- dat %>%
        mutate(accs = accs) %>%
        group_by(accs) %>%
        summarise(!!sample_name := sum(readCount), .groups = "drop") %>%
        filter(!is.na(accs))

      otu_list[[otu_idx]] <- dat
      otu_idx <- otu_idx + 1
    }
    
    # AMR MAPSTAT FILES
    if (dir.exists(resfinder_dir)) {
      amr_files <- list.files(resfinder_dir, pattern = "\\.mapstat$", full.names = TRUE)
      amr_files <- amr_files[!grepl("_all\\.mapstat$", amr_files)]
      
      amr_entries <- lapply(amr_files, function(f) {
        dat <- tryCatch(fread(f, skip = 6, quote = ""), error = function(e) return(NULL))
        if (is.null(dat) || nrow(dat) == 0 || !"readCount" %in% colnames(dat)) return(NULL)
        
        class_name <- str_extract(basename(f), "(?<=_)[^_]+(?=\\.mapstat$)")
        if (is.na(class_name) || class_name == "all") return(NULL)
        
        dat %>%
          summarise(readCount = sum(readCount, na.rm = TRUE)) %>%
          mutate(sample = sample_name, class = class_name)
      })
      
      valid_amr <- amr_entries[!sapply(amr_entries, is.null)]
      for (entry in valid_amr) {
        amr_list[[amr_idx]] <- entry
        amr_idx <- amr_idx + 1
      }
    }
  }
  
  otu_list <- otu_list[1:(otu_idx - 1)]
  amr_list <- amr_list[1:(amr_idx - 1)]
  
  # Write per-species OTU
  if (length(otu_list) > 0) {
    otu_merged <- reduce(otu_list, full_join, by = "accs") %>%
      mutate(across(-accs, ~replace_na(.x, 0)))
    otu_mat <- t(column_to_rownames(otu_merged, "accs"))
    otu_dat <- data.frame(sample = rownames(otu_mat), otu_mat)
    
    write.csv(otu_dat, file.path(count_path, paste0("OTU_counts_test_", species_name, ".csv")), row.names = FALSE)
  }
  
  # Write per-species AMR
  if (length(amr_list) > 0) {
    amr_dat <- bind_rows(amr_list) %>%
      pivot_wider(names_from = class, values_from = readCount, values_fill = list(readCount = 0))
    
    write.csv(amr_dat, file.path(count_path, paste0("AMR_counts_test_", species_name, ".csv")), row.names = FALSE)
  }
}
