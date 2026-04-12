library(tidyverse)
library(glmnet)

# ================================================
# 1. DATA CLEANING (ONLY ONCE)==========
# =========================================
spotify_clean <- read_csv("spotifydataset.csv") |>
  arrange(desc(popularity)) |>
  distinct(track_id, .keep_all = TRUE) |>
  drop_na()

spotify_processed <- spotify_clean |>
  mutate(explicit=ifelse(explicit==TRUE, 1, 0))

# ============================================================
# 2. TYPE CONVERSION + FEATURE SELECTION===========
# =========================================
spotify <- spotify_processed |>
  mutate(
    time_signature = as.factor(time_signature),
    mode = as.factor(mode),
    key = as.factor(key),
    explicit = as.factor(explicit)
  )

target_features <- c("valence", "popularity", "duration_ms", "danceability", 
                     "energy", "key", "loudness", "mode", "speechiness", 
                     "acousticness", "instrumentalness", "liveness", 
                     "tempo", "time_signature", "explicit")

spotify <- spotify |> select(all_of(target_features))

# ================================================================
# 3. TRAIN-TEST SPLIT (SHARED BY ALL MODELS)=========
# =========================================
set.seed(123)
sample_size <- floor(0.8 * nrow(spotify))
train_indices <- sample(seq_len(nrow(spotify)), size = sample_size)

train_subset <- spotify[train_indices, ]
test_subset  <- spotify[-train_indices, ]

# Fix factor levels
train_subset <- droplevels(train_subset)
test_subset <- test_subset |>
  mutate(across(where(is.factor),
                ~ factor(.x, levels = levels(train_subset[[cur_column()]]))))

# =========================================
# 4. SCALING (TRAIN-BASED ONLY)==========
# =========================================
numeric_cols <- c("popularity", "duration_ms", "danceability", "energy", 
                  "loudness", "speechiness", "acousticness", 
                  "instrumentalness", "liveness", "tempo")

train_center <- sapply(train_subset[, numeric_cols], mean)
train_scale  <- sapply(train_subset[, numeric_cols], sd)

train_subset[, numeric_cols] <- scale(train_subset[, numeric_cols],
                                      center = train_center,
                                      scale = train_scale)

test_subset[, numeric_cols] <- scale(test_subset[, numeric_cols],
                                     center = train_center,
                                     scale = train_scale)

# =========================================
# 5. OLS MODEL==================
# =========================================
model_ols <- lm(valence ~ ., data = train_subset)

train_r2 <- summary(model_ols)$r.squared

predictions <- predict(model_ols, newdata = test_subset)

test_mae <- mean(abs(test_subset$valence - predictions))
sse <- sum((test_subset$valence - predictions)^2)
sst <- sum((test_subset$valence - mean(test_subset$valence))^2)

test_r2 <- 1 - (sse / sst)
test_mse <- sse / nrow(test_subset)
test_rmse <- sqrt(test_mse)

summary(model_ols)
model_ols
train_r2
test_r2 
test_mae
test_mse
test_rmse

# =========================================
# 6. LASSO MODEL
# =========================================

# Design matrices
X_train <- model.matrix(valence ~ ., data = train_subset)[, -1]
y_train <- train_subset$valence

X_test  <- model.matrix(valence ~ ., data = test_subset)[, -1]
y_test  <- test_subset$valence

# Cross-validated Lasso which uses alpha=1
lasso_cv <- cv.glmnet(X_train, y_train, alpha =1, nfolds = 10)
best_lambda <- lasso_cv$lambda.1se

lasso_final <- glmnet(X_train, y_train, alpha =1, lambda = best_lambda)

# Coefficients
coefs <- as.matrix(coef(lasso_final))
selected_features <- coefs[coefs != 0, , drop = FALSE]

# Metrics
lasso_train_r2 <- lasso_final$dev.ratio

lasso_predict <- predict(lasso_final, s = best_lambda, newx = X_test)

lasso_test_mae <- mean(abs(y_test - lasso_predict))
lasso_sse <- sum((y_test - lasso_predict)^2)
lasso_sst <- sum((y_test - mean(y_test))^2)

lasso_test_r2 <- 1 - (lasso_sse / lasso_sst)
lasso_test_mse <- lasso_sse / nrow(X_test)
lasso_test_rmse <- sqrt(lasso_test_mse)

coefs
best_lambda
lasso_train_r2
lasso_test_r2
lasso_test_mae
lasso_test_mse
lasso_test_rmse

# =========================================
# 7. LASSO VISUALIZATION==============
# =========================================
coef_dframe_lasso <- data.frame(
  Feature = rownames(coefs),
  Coefficient = as.numeric(coefs)
) |>
  filter(Feature != "(Intercept)") |>
  mutate(
    Direction = case_when(
      Coefficient > 0 ~ "a. Positive Impact",
      Coefficient < 0 ~ "b. Negative Impact",
      TRUE ~ "No Impact (Zeroed)"
    ),
    Coefficient = ifelse(is.na(Coefficient), 0, Coefficient)
  )

ggplot(coef_dframe_lasso, aes(x = reorder(Feature, Coefficient), 
                              y = Coefficient, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.1) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey") +
  scale_fill_manual(values = c(
    "a. Positive Impact" = "#2E8B57", 
    "b. Negative Impact" = "#191414")) +
  theme_minimal()+
  labs(
    title = "Lasso Regression Coefficients",
    x = "Musical Features",
    y = "Coefficient Value",)
    
# Predicted vs Actual
plot_data <- data.frame(
  Actual = y_test,
  Predicted = as.vector(lasso_predict)
)

ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.4, color = "#191414") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color='green', linewidth = 1.3) +
  theme_minimal() +
  labs(
    title = "Lasso Regression: Predicted vs. Actual Valence",
    subtitle = paste("Test R-squared:", round(lasso_test_r2, 3)),
    x = "Actual Valence",
    y = "Predicted Valence")


# =========================================
# ELASTIC NET==============
# =========================================

# Elastic Net uses same X_train, y_train, X_test, y_test

# alpha between(0 = Ridge, 1 = Lasso)
# commonly 0.5 as balanced mix
elastic_cv <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 10)

best_lambda_enet <- elastic_cv$lambda.1se

elastic_final <- glmnet(X_train, y_train, alpha = 0.5, lambda = best_lambda_enet)

# Coefficients
coefs_enet <- as.matrix(coef(elastic_final))
selected_features_enet <- coefs_enet[coefs_enet != 0, , drop = FALSE]

elastic_train_r2 <- elastic_final$dev.ratio
elastic_predict <- predict(elastic_final, s = best_lambda_enet, newx = X_test)

elastic_test_mae <- mean(abs(y_test - elastic_predict))
elastic_sse <- sum((y_test - elastic_predict)^2)
elastic_sst <- sum((y_test - mean(y_test))^2)

elastic_test_r2 <- 1 - (elastic_sse / elastic_sst)
elastic_test_mse <- elastic_sse / nrow(X_test)
elastic_test_rmse <- sqrt(elastic_test_mse)

coefs_enet
best_lambda_enet
elastic_train_r2
elastic_test_r2
elastic_test_mae
elastic_test_mse
elastic_test_rmse

# =========================================
# 9. ELASTIC NET VISUALIZATION========
# =========================================
coef_dframe_enet <- data.frame(
  Feature = rownames(coefs_enet),
  Coefficient = as.numeric(coefs_enet)
) |>
  filter(Feature != "(Intercept)") |>
  mutate(
    Direction = case_when(
      Coefficient > 0 ~ "a. Positive Impact",
      Coefficient < 0 ~ "b. Negative Impact",
      TRUE ~ "No Impact (Zeroed)"
    ),
    Coefficient = ifelse(is.na(Coefficient), 0, Coefficient)
  )

ggplot(coef_dframe_enet, aes(x = reorder(Feature, Coefficient), 
                             y = Coefficient, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.1) +
  coord_flip() +
  geom_hline(yintercept = 0, color = "grey") +
  scale_fill_manual(values = c(
    "a. Positive Impact" = "#2E8B57", 
    "b. Negative Impact" = "#191414"
  )) + theme_minimal()+
  labs(
  title = "Elastic Net Coefficients",
  x = "Musical Features",
  y = "Coefficient Value")

# Predicted vs Actual
plot_data_enet <- data.frame(
  Actual = y_test,
  Predicted = as.vector(elastic_predict)
)

ggplot(plot_data_enet, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5, color = "#2E8B57") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth=1.3, color='black') +
  theme_minimal() +
  labs(
    title = "Elastic Net: Predicted vs. Actual Valence",
    subtitle = paste("Test R-squared:", round(elastic_test_r2, 3)),
    x = "Actual Valence",
    y = "Predicted Valence")


# =========================================
# 9. XGBOOST========
# =========================================
library(xgboost)

# Convert to matrix (already numeric from model.matrix)
dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)


# Parameters (simple baseline)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1,
  max_depth = 6
)

# Train model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 0
)

# Predictions
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

# =========================================
# 9. XGBOOST VISUALIZATION========
# =========================================
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
# ===============================================
# 9. XGBOOST using only LASSO FEATURES========
# =========================================

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

# ========================================================
# XGBOOST using only LASSO FEATURES VISUALIZATION========
# =========================================

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
