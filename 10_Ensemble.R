rm(list = ls())

# Define paths
setwd("/orange/somnath.datta/CAMDA2025/Ensemble_test")

# Load packages
library(dplyr)
source("10_XGBoostModel.R")
source("10_PenalizedLogisticModel.R")
source("10_RandomForestModel.R")

gs <- read.table(file = "genus_species.txt") %>%
  mutate("bact" = paste0(V1, '_', V2)) %>%
  dplyr::select(bact) %>%
  unlist()

for(i in gs) {
    
  # load in data
  load(paste0(i,"_test.RData"))
  test <- x
  
  load(paste0(i, "_train.RData"))
  train <- x
  
  # Filter out intermediate cases
  filter_idx <- train$info$pheno %in% c("Susceptible", "Resistant")
  train <- lapply(train, function(y) y[filter_idx,])
  
  train_x <- cbind(train$amr, train$otu)
  test_x <- cbind(test$amr, test$otu)
  
  common_cols <- intersect(colnames(train_x), colnames(test_x))
  
  train_x <- train_x[, common_cols, drop = FALSE]
  test_x  <- test_x[, common_cols, drop = FALSE]
  
  
  train_y <- factor(ifelse(train$info$pheno == "Resistant", 1, 0))
  
  
  # # set up 70/30 split for data
  # set.seed(1234)
  # n <- length(responses)
  # train_idx <- sample(seq_len(n), size = floor(0.7 * n))
  # test_idx  <- setdiff(seq_len(n), train_idx)
  # 
  # train_x <- predictors[train_idx,]
  # train_y <- factor(ifelse(responses[train_idx] == "Resistant", 1, 0))
  # 
  # test_x <- predictors[test_idx,]
  # test_y <-  factor(ifelse(responses[test_idx] == "Resistant", 1, 0))
  # 
  # now we train all the models:
  
  message("Training models for ", i, "...")
  llr_train <- Train_Lasso_LogReg(train_x, train_y)
  rf_train <- Train_RF_Model(train_x, train_y)
  xgb_train <- Train_XGB_Model(train_x, train_y)
  
  # get prediction vector for ensemble 
  llr_res <- Lasso_LogReg_Model(train_x, train_y, test_x, llr_train)
  rf_res <- RF_Model(train_x, train_y, test_x, rf_train)
  xgb_res <- XGB_Model(train_x, train_y, test_x, xgb_train)
  
  pred_mat <- cbind(LLR = llr_res, RF = rf_res, XGB = xgb_res)

  # Majority vote (ENS)
  ensemble_preds <- apply(pred_mat, 1, function(row) as.integer(mean(row) >= 0.5))
  test$info$phenotype <- factor(ifelse(ensemble_preds == 1, "Resistant", "Susceptible"))
  #ensemble_labels <- factor(ifelse(ensemble_preds == 1, "Resistant", "Susceptible"))
  x <- test$info
  save(x, file = paste0(i, "_test_predictions.RData"))
  
}