library(glmnet)

#RUN TRAIN_TEST FIRST (WON'T WORK IF NOT)

#OLS======
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

#LASSO====

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

# PredActual
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


# ELASTIC NET===========

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

