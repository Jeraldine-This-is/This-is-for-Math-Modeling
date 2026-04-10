# This is Data Cleaning
```
library(tidyverse)
library(glmnet)

# ================================================
# 1. DATA CLEANING (only once, kindly dont repeat)==========
# =========================================
spotify_clean <- read_csv("spotifydataset.csv") |>
  arrange(desc(popularity)) |>
  distinct(track_id, .keep_all = TRUE) |>
  drop_na()

spotify_processed <- spotify_clean |>
  mutate(explicit=ifelse(explicit==TRUE, 1, 0))
```
# This is for Test-Train Split

```

# data splitting - consistent 123 as starting point
set.seed(123) 
sample_size <- floor(0.8 * nrow(spotify_val_processed))
train_indices <- sample(seq_len(nrow(spotify_val_processed)), size = sample_size)

target_features <- c( "valence", "popularity", "duration_ms", "danceability", 
                     "energy", "key", "loudness", "mode", "speechiness", 
                     "acousticness", "instrumentalness", "liveness", "tempo","time_signature", "explicit")
train_subset <- spotify_val_processed[train_indices, ] |> select(all_of(target_features))
test_subset  <- spotify_val_processed[-train_indices, ] |> select(all_of(target_features))

#to fill the Model 0 Training r-squared cell
# dot is all else except DV_valence
model_zero <- lm(valence ~ ., data = train_subset)
#train r-squared output
train_r2 <-summary(model_zero)$r.squared
train_r2
#prediction of values for test set using the trained one
predictions <- predict(model_zero, newdata = test_subset)

#Test R-squared output 
#(Manually, since lm summary doesn't do this for new data)
#calculating sum of squares
test_mae <- mean(abs(test_subset$valence-predictions))
sse <- sum((test_subset$valence-predictions)^2)
sst <- sum((test_subset$valence-mean(test_subset$valence))^2)
test_r2 <- 1-(sse / sst)
test_mse <- sse / nrow(test_subset)
test_rmse <- sqrt(test_mse)

#test results
test_r2 
test_mae
test_mse
test_rmse
```
