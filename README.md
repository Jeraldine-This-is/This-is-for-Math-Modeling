# This is Data Cleaning
```
library(tidyverse)
spotify_val_processed <- read_csv("spotify_processed_fr_valence.csv")

#types of variable 
spotify_val_processed <- spotify_val_processed |>
  mutate(
    time_signature = as.factor(time_signature),
    mode = as.factor(mode),
    key = as.factor(key),
    explicit =as.factor(explicit))

#full model equation
full_model <- lm(valence ~ popularity + duration_ms + danceability + 
                   energy + key + loudness + mode + speechiness + time_signature+
                   acousticness + instrumentalness + liveness + tempo + explicit, 
                 data = spotify_val_processed)
summary(full_model)
```
