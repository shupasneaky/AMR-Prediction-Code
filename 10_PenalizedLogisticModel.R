library(glmnet)

Train_Lasso_LogReg <- function(train_x, train_y) {
  set.seed(1)
  
  # 10-fold CV for lambda selection
  cv_fit <- cv.glmnet(as.matrix(train_x), train_y, family = "binomial", alpha = 1, nfolds = 10)
  
  return(cv_fit$lambda.min)
}


Lasso_LogReg_Model <- function(train_x, train_y, test_x, best_lambda) {
  set.seed(1)
  
  # Fit final model with best lambda
  lasso_model <- glmnet(as.matrix(train_x), train_y, family = "binomial", alpha = 1, lambda = best_lambda)
  
  # Predict on test set
  pred_probs <- predict(lasso_model, newx = as.matrix(test_x), type = "response")
  pred_labels <- ifelse(pred_probs > 0.5, 1, 0)
  
  return(as.vector(pred_labels))
}
