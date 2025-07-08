rm(list = ls())
library(dplyr)

# set paths
setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance")
data_path <- paste0(getwd(),'/Datasets')
res_path <- paste0(getwd(), '/Results')

# import CSV files
test <- read.csv(paste0(data_path, '/testing_template.csv'), header=TRUE)

# testing data accession codes
test_df <- data.frame(
  genus = as.factor(test$genus),
  species = as.factor(test$species),
  accession = test$accession,
  phenotype = test$phenotype,
  antibiotic = as.factor(test$antibiotic),
  measurement_value = NA
) %>% as.tibble()

gs <- read.table(file = "C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/genus_species.txt") %>%
  mutate("bact" = paste0(V1, '_', V2)) %>%
  select(bact) %>%
  unlist()


for(i in 1:length(gs)){
  amr_counts <- read.csv(file = paste0(res_path,'/',list.files(res_path, pattern = paste0("amr_norm_test_",gs[i]))))
  amr_codes <- amr_counts$X
  
  otu_counts <- read.csv(file = paste0(res_path,'/',list.files(res_path, pattern = paste0("otu_norm_test_",gs[i]))))
  otu_codes <- otu_counts$X
  
  # Retain only otu codes
  df_hold <- test_df %>% filter(accession %in% otu_codes)
  df_hold <- df_hold[match(otu_codes, df_hold$accession), ]
  
  # Align and fill AMR
  amr_aligned <- amr_counts %>%
    right_join(data.frame(X = otu_codes), by = "X") %>%
    arrange(match(X, otu_codes)) %>%
    replace(is.na(.), 0)
  
  # Clean rownames
  rownames(amr_aligned) <- amr_aligned$X
  rownames(otu_counts) <- otu_counts$X
  amr_aligned$X <- NULL
  otu_counts$X <- NULL
  
  # === S3 object ===
  x <- list(
    info = df_hold %>% select(genus, species, accession, phenotype, antibiotic, measurement_value),
    otu = otu_counts,
    amr = amr_aligned)
  
  # Save the object by its name
  save(x, file = paste0(res_path,'/', gs[i], "_test.RData"))
}

