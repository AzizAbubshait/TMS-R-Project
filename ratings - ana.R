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
  group_by(participant, ï..scenarioNo) %>%
  slice(c(-1)) %>%
  summarize(avg_ist = mean(rating.response),
         rt_ist = mean(rating.rt))
# waytz
waytz_path = paste(getwd(), "/waytz/", sep = "")
waytzFiles = list.files(waytz_path)
waytz = lapply(paste(waytz_path, waytzFiles, sep = ""), read.csv) 
waytz = do.call(plyr::rbind.fill, waytz)

# preprocess
all_ratings = waytz %>% 
  rename(participant = subject_nr) %>%
  group_by(participant) %>%
  summarize(waytz_score = mean(Rating_response)) %>%
  left_join(ist_scored, by = "participant")

write.csv(all_ratings, "aggregated_ratings.csv")
