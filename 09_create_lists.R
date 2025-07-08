rm(list = ls())
library(dplyr)

# set paths
setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance")
data_path <- paste0(getwd(),'/Datasets')
res_path <- paste0(getwd(), '/Results')

# import CSV files
train <- read.csv(paste0(data_path, '/training_dataset.csv'), header=TRUE)

# training data accession codes
train_df <- data.frame(
  bact_type = as.factor(stringr::str_c(train$genus, train$species, sep = '_')),
  code = train$accession,
  antibiotic = as.factor(train$antibiotic),
  pheno = as.factor(train$phenotype),
  date = as.factor(train$collection_date),
  wound = as.factor(train$isolation_source),
  country = as.factor(train$isolation_country)
) %>%
  as_tibble() %>%
  group_by(code) %>%
  summarise(
    bact_type = na.omit(bact_type)[1],
    antibiotic = na.omit(antibiotic)[1],
    pheno = na.omit(pheno)[1],
    date = na.omit(date)[1],
    wound = na.omit(wound)[1],
    country = na.omit(country)[1],
    .groups = "drop"
  )

gs <- read.table(file = "C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/genus_species.txt") %>%
  mutate("bact" = paste0(V1, '_', V2)) %>%
  select(bact) %>%
  unlist()


for(i in 1:length(gs)){
  amr_counts <- read.csv(file = paste0(res_path,'/',list.files(res_path, pattern = paste0("amr_norm_train_",gs[i]))))
  amr_codes <- amr_counts$X
  
  otu_counts <- read.csv(file = paste0(res_path,'/',list.files(res_path, pattern = paste0("otu_norm_train_",gs[i]))))
  otu_codes <- otu_counts$X
  
  # Retain only otu codes
  df_hold <- train_df %>% filter(code %in% otu_codes)
  df_hold <- df_hold[match(otu_codes, df_hold$code), ]
  
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
    info = df_hold %>% select(bact_type, antibiotic, pheno),
    meta = df_hold %>% select(date, wound, country),
    otu = otu_counts,
    amr = amr_aligned)
  
  fix.meta <- x$meta %>%
    mutate(across(
      everything(),
      ~ as.factor(
        ifelse(
          is.na(.) | trimws(.) == "" | tolower(as.character(.)) %in% c("na", "wound"),
          "no info",
          tolower(as.character(.))
        )
      )
    )) %>%
    as.data.frame()
  rownames(fix.meta) <- rownames(x$otu)
  x$meta <- fix.meta
  
  # Save the object by its name
  save(x, file = paste0(res_path,'/', gs[i], "_train.RData"))
}

