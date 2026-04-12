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

