rm(list = ls())

# Load packages
library(dplyr)

gs <- read.table(file = "C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/genus_species.txt") %>%
  mutate("bact" = paste0(V1, '_', V2)) %>%
  dplyr::select(bact) %>%
  unlist()

# load in data
setwd("C:/Users/owvis/Desktop/CAMDA 2025/Anti Microbial Resistance/Results")

model_performance <- matrix(0, nrow = 9, ncol = 4, dimnames= list(c(rep("",9)), c("LLR", "RF", "XGB", "ENS")))
variable_importance <- matrix(0, nrow = 9, ncol = 1, dimnames= list( c(rep("",9)), "Important Variables" ))
data_composition <- matrix(0, nrow = 10, ncol = 4, dimnames= list( c(rep("",10)), c("train", "train", "test", "test") ))
data_composition[1,] <- c("resistant", "susceptible", "resistant", "susceptible")

for(i in 1:length(gs)){
    
  load(list.files(pattern=paste0(gs[i],"_full_info")))
  load(list.files(pattern=paste0(gs[i],"_train")))
  
  rownames(model_performance)[i] <- rownames(variable_importance)[i] <- rownames(data_composition)[i+1] <- gs[i]
  
  #Data composition
  data_composition[i+1,] <- c(
    sum(x$info$pheno == "Resistant") - sum(full_info$phenotype$Actual == 1),
    sum(x$info$pheno == "Susceptible") - sum(full_info$phenotype$Actual == 0),
    sum(full_info$phenotype$Actual == 1),
    sum(full_info$phenotype$Actual == 0))
  
  #Model Performance
  model_performance[i,] <- c(full_info$accuracy$LLR, full_info$accuracy$RF, full_info$accuracy$XGB, full_info$accuracy$ENS)
  model_performance[i,] <- round(model_performance[i,], 3)*100
  
  #Variable Importance
  variable_importance[i,] <- paste(full_info$variable_importance$Variable[1:15], collapse=", ")
  
}

