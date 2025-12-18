# Guiding Question 
# How has weather impacted UFO sightings


# -------- Packages ---------------
library(readr)
library(dplyr)


# -------- Reading data files and investigating -----------
songs <- read.csv("/Users/prestonyoshino/DataScience/final/original_data/spotify_songs.csv")
history <- read.csv("/Users/prestonyoshino/DataScience/final/original_data/spotify_history.csv")

# ----- Inspect Data for Null values --------------------
nrow(songs %>% filter(is.na(track_name)))
nrow(history %>% filter(is.na(track_name))) # none

# ---------- Remove Null Values ----------------
songs <- songs %>% filter(!is.na(track_name))

# ---------- Remove Where Song is not listened to --------------
history <- history %>% filter(ms_played != 0)

# ----------- Remove Non-Relevant Features -------------
history_select <- history %>% select(-ts, -platform, -reason_end, -shuffle)
songs_select <- songs %>% 
  select(-track_id, -track_album_id, -track_album_name, -playlist_id)

# ----------- Calculate Group Stats -----------
history_select <- history %>% select(-ts, -platform, -reason_end, -shuffle)
history_summarized <- history_select %>% group_by(track_name) %>% summarize(
  mean_ms_played_percentage = mean(ms_played, na.rm = TRUE),
  skip_percentage = mean(skipped, na.rm = TRUE),
  intentional_percentage = mean(reason_start == "clickrow", na.rm = TRUE),
  unintentional_percantage = mean(reason_start != "clickrow", na.rm = TRUE),
  listens = n(),
)

# -------- Filter Small Sample Sizes --------------
history_size <- history_summarized %>% filter(listens >= 20)

# ----------- Inner Join ------------------
combined <- inner_join(songs_select, history_size, by = "track_name")

# ------- Remove Duplicate Songs -----------
combined_no_rep <- combined %>% 
  distinct(track_name, .keep_all = TRUE)

# ----------- Compute mean_ms_played over duration ------------
combined_time <- combined_no_rep %>% mutate(playing_time_percentage = mean_ms_played_percentage / duration_ms)
combined_time_rm <- combined_time %>% select(-mean_ms_played_percentage)

# ------------ Further Refinement ----------------------------
refine <- combined_time_rm %>% select(-playlist_name)

write.csv(refine, file="data.csv", row.names = FALSE)
