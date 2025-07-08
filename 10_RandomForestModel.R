library(caret)

.libPaths("/orange/somnath.datta/CAMDA2025/Rpackages")
library(ranger)

Train_RF_Model <- function(train_x, train_y) {
  set.seed(1)
  train_mat <- data.frame(train_y = train_y, train_x)
  q <- floor(sqrt(ncol(train_mat) - 1))
  
  tune_grid <- expand.grid(
    mtry = c(q - 2, q - 1, q, q + 1, q + 2),
    splitrule = "gini",
    min.node.size = 1:5
  )
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE
  )
  
  rf_tuned <- train(
    train_y ~ .,
    data = train_mat,
    method = "ranger",
    trControl = ctrl,
    tuneGrid = tune_grid,
    num.trees = 500,
    importance = 'none',
    classification = TRUE
  )
  
  return(rf_tuned$bestTune)
}


RF_Model <- function(train_x, train_y, test_x, best_params) {
  set.seed(1)
  train_mat <- data.frame(train_y = train_y, train_x)
  test_mat <- data.frame(test_x)
  
  rf_model <- ranger(
    formula = train_y ~ .,
    data = train_mat,
    num.trees = 500,
    mtry = best_params$mtry,
    min.node.size = best_params$min.node.size,
    splitrule = best_params$splitrule,
    importance = 'none',
    classification = TRUE
  )
  
  pred_labels <- predict(rf_model, data = test_mat)$predictions
  pred_labels <- ifelse(pred_labels == "1", 1, 0)
  return(pred_labels)
}


