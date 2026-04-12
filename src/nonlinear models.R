
library(xgboost)
#run ytain-test firsttt
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)


params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 0
)
xgb_pred <- predict(xgb_model, dtest)
xgb_pred_train <- predict(xgb_model, dtrain)
# Train R²
xgb_sse_train <- sum((y_train - xgb_pred_train)^2)
xgb_sst_train <- sum((y_train - mean(y_train))^2)

xgb_train_r2 <- 1 - (xgb_sse_train / xgb_sst_train)
xgb_mae <- mean(abs(y_test - xgb_pred))
xgb_sse <- sum((y_test - xgb_pred)^2)
xgb_sst <- sum((y_test - mean(y_test))^2)
xgb_r2 <- 1 - (xgb_sse / xgb_sst)
xgb_mse <- xgb_sse / nrow(X_test)
xgb_rmse <- sqrt(xgb_mse)

xgb_train_r2
xgb_r2
xgb_mae
xgb_mse
xgb_rmse

# ALLL INN=====
plot_data_xgb <- data.frame(
  Actual = y_test,
  Predicted = xgb_pred
)

ggplot(plot_data_xgb, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#1DB954") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 1.3) +
  theme_minimal() +
  labs(
    title = "XGBoost (All Features): Predicted vs Actual",
    subtitle = paste("Test R²:", round(xgb_r2, 3))
  )
importance_matrix <- xgb.importance(
  feature_names = colnames(dtrain),
  model = xgb_model)
importance_frame <- as.data.frame(importance_matrix)

ggplot(importance_frame, 
       aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "#1DB954") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "XGBoost Feature Importance (All Features)",
    x = "Features",
    y = "Importance (Gain)"
  )

importance_frame
#using LASSO FEATURES1====

selected_features

# Extract selected feature names (remove intercept)
lasso_features <- rownames(selected_features)
lasso_features <- lasso_features[lasso_features != "(Intercept)"]

# Match columns in X_train
X_train_lasso <- X_train[, colnames(X_train) %in% lasso_features]
X_test_lasso  <- X_test[, colnames(X_test) %in% lasso_features]

# DMatrix
dtrain_lasso <- xgb.DMatrix(data = X_train_lasso, label = y_train)
dtest_lasso  <- xgb.DMatrix(data = X_test_lasso,  label = y_test)

# Train model
xgb_model_lasso <- xgb.train(
  params = params,
  data = dtrain_lasso,
  nrounds = 100,
  verbose = 0
)

# Predictions
xgb_pred_lasso <- predict(xgb_model_lasso, dtest_lasso)
# Train R² (Lasso-selected XGBoost)
xgb_pred_train_lasso <- predict(xgb_model_lasso, dtrain_lasso)

xgb_lasso_sse_train <- sum((y_train - xgb_pred_train_lasso)^2)
xgb_lasso_sst_train <- sum((y_train - mean(y_train))^2)

xgb_lasso_train_r2 <- 1 - (xgb_lasso_sse_train / xgb_lasso_sst_train)
xgb_lasso_mae <- mean(abs(y_test - xgb_pred_lasso))
xgb_lasso_sse <- sum((y_test - xgb_pred_lasso)^2)
xgb_lasso_sst <- sum((y_test - mean(y_test))^2)

xgb_lasso_r2 <- 1 - (xgb_lasso_sse / xgb_lasso_sst)
xgb_lasso_mse <- xgb_lasso_sse / nrow(X_test_lasso)
xgb_lasso_rmse <- sqrt(xgb_lasso_mse)

xgb_lasso_train_r2
xgb_lasso_r2
xgb_lasso_mae
xgb_lasso_mse
xgb_lasso_rmse

#using LASSO FEATURES2========

plot_data_xgb_lasso <- data.frame(
  Actual = y_test,
  Predicted = xgb_pred_lasso
)

ggplot(plot_data_xgb_lasso, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#191414") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = 'green', linewidth = 1.3) +
  theme_minimal() +
  labs(
    title = "XGBoost (Lasso Features): Predicted vs Actual",
    subtitle = paste("Test R²:", round(xgb_lasso_r2, 3))
  )

importance_matrix_lasso <- xgb.importance(model = xgb_model_lasso)

importance_xg_lasso <- as.data.frame(importance_matrix_lasso)

ggplot(importance_xg_lasso, 
       aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "#2E8B57") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "XGBoost Feature Importance (Lasso-Selected Features)",
    x = "Features",
    y = "Importance (Gain)"
  )
importance_xg_lasso
