library(tidyverse)

spotify_clean <- read_csv("spotifydataset.csv") |>
  arrange(desc(popularity)) |>
  distinct(track_id, .keep_all = TRUE) |>
  drop_na()

spotify_processed <- spotify_clean |>
  mutate(explicit=ifelse(explicit==TRUE, 1, 0))

write_csv(spotify_processed, "spotify_processed_fr_valence.csv")