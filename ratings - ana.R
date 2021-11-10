library(tidyverse)
# load packages

cbbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# load packages

# IST pre-process ----
ist_path = paste(getwd(), "/ist/", sep = "")
filenames = list.files(ist_path)

ist = lapply(paste(ist_path, filenames, sep = ""), read.csv) 
ist = do.call(plyr::rbind.fill, ist)

# preprocess
ist_scored = ist %>% 
  rename(subject_nr = participant) %>%
  group_by(subject_nr, ï..scenarioNo) %>%
  slice(c(-1)) %>%
  mutate(stand_score = ifelse(Lefttype == "I", 100-rating.response, rating.response)) %>%
  summarize(avg_ist = mean(stand_score))
# waytz
waytz_path = paste(getwd(), "/waytz/", sep = "")
waytzFiles = list.files(waytz_path)
waytz = lapply(paste(waytz_path, waytzFiles, sep = ""), read.csv) 
waytz = do.call(plyr::rbind.fill, waytz)

# preprocess
all_ratings = waytz %>% 
  group_by(subject_nr) %>%
  mutate(Rating_response2 = as.numeric(Rating_response)) %>%
  summarize(waytz_score = mean(Rating_response2, na.rm = T)) %>%
  left_join(ist_scored, by = "subject_nr")

write.csv(all_ratings, "aggregated_ratings.csv")

