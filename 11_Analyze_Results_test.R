rm(list = ls())

library(dplyr)

gs <- read.table(file = "C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/genus_species.txt") %>%
  mutate("bact" = paste0(V1, '_', V2)) %>%
  dplyr::select(bact) %>%
  unlist()

setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance")
data_path <- paste0(getwd(), '/Datasets')
res_path <- paste0(getwd(), '/Results')

test <- read.csv(paste0(data_path, '/testing_template.csv'), header = TRUE, stringsAsFactors = FALSE)

for(i in gs){
  
  load(paste0(res_path, '/', i, "_test_predictions.RData")) 
  x <- as.data.frame(x)
  
  x_to_join <- x %>%
    mutate(
      genus = as.character(genus),
      species = as.character(species),
      accession = as.character(accession),
      phenotype_from_x = as.character(phenotype)
    ) %>%
    dplyr::select(genus, species, accession, phenotype_from_x)
  
  # Join predicted phenotypes
  test <- test %>%
    left_join(x_to_join, by = c("genus", "species", "accession")) %>%
    mutate(
      phenotype = ifelse(!is.na(phenotype_from_x), phenotype_from_x, phenotype)
    ) %>%
    dplyr::select(-phenotype_from_x)
  
  # # Fill remaining NAs using majority phenotype in x
  # majority_pheno <- names(sort(table(x$phenotype), decreasing = TRUE))[1]
  # 
  # test <- test %>%
  #   mutate(
  #     phenotype = ifelse(
  #       !(phenotype %in% c("Resistant", "Susceptible")) & genus == unique(x$genus) & species == unique(x$species),
  #       majority_pheno,
  #       phenotype
  #     )
  #   )
  test$measurement_value <- ifelse(test$phenotype == "Resistant", 1, 0)
}


apply(test, 2, table)
write.csv(test, paste0(res_path, '/testing_template_updated.csv'), row.names = FALSE)

