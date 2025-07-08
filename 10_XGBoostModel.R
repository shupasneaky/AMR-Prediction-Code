library(caret)

.libPaths("/orange/somnath.datta/CAMDA2025/Rpackages")
library(xgboost)

Train_XGB_Model <- function(train_x, train_y) {
  set.seed(1)
  train_df <- data.frame(train_y = train_y, train_x)
  train_df$train_y <- factor(ifelse(train_df$train_y == 1, "Yes", "No"))
  
  xgb_grid <- expand.grid(
    nrounds = c(300, 500),
    max_depth = c(3, 4, 5),
    eta = c(0.03, 0.05, 0.07),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    verboseIter = TRUE,
    classProbs = TRUE,
    summaryFunction = twoClassSummary
  )
  
  
  xgb_tuned <- suppressWarnings(
    train(
      train_y ~ .,
      data = train_df,
      method = "xgbTree",
      trControl = ctrl,
      tuneGrid = xgb_grid,
      metric = "ROC"
    )
  )
  
  return(xgb_tuned$bestTune)
}


XGB_Model <- function(train_x, train_y, test_x, best_params) {
  set.seed(1)
  
  dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = as.vector(train_y))
  dtest <- xgb.DMatrix(data = as.matrix(test_x))
  
  final_model <- xgboost(
    data = dtrain,
    nrounds = best_params$nrounds,
    max_depth = best_params$max_depth,
    eta = best_params$eta,
    gamma = best_params$gamma,
    colsample_bytree = best_params$colsample_bytree,
    min_child_weight = best_params$min_child_weight,
    subsample = best_params$subsample,
    objective = "binary:logistic",
    eval_metric = "logloss",
    verbose = 0
  )
  
  pred_probs <- predict(final_model, dtest)
  pred_y <- ifelse(pred_probs > 0.5, 1, 0)
  return(as.vector(pred_y))
}
