library(tidyverse)
library(factoextra)
library(gridExtra)

rm(list = ls())

# K means analysis ----
# gc pre-process ----
gc_path = paste(getwd(), "/raw gc/", sep = "")
filenames = list.files(gc_path)

gc = lapply(paste(gc_path, filenames, sep = ""), read.csv) 
gc = do.call(plyr::rbind.fill, gc)

gc_dat = gc %>% 
  group_by(
    subject_nr, Site
  ) %>%
  mutate(
    validity = recode(validity, valid="invalid", invalid = "valid"),
    accu_percent = sum(correct == 1)/length(correct)
  ) %>%
  filter(
    correct == 1,
    response_time > 150,
    response_time < 1500,
    TRIAL_VALID == 1
  ) %>%
  group_by(
    subject_nr, gazeCond, Site, validity
  ) %>% 
  mutate(
    rm_trial = case_when(response_time > abs(mean(response_time)+2.5*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-2.5*sd(response_time)) ~ 1)
  ) %>%
  filter(
    is.na(rm_trial) == T,
    subject_nr != 0
  ) %>%
  group_by(subject_nr) %>%
  summarize(rt = mean(response_time), 
            accu = first(accu_percent)) %>%
  left_join(read.csv("aggregated_ratings.csv"), by = "subject_nr") %>%
  select(-X)

# the idea is to see which observations are alike?
df_cluster = as.data.frame(gc_dat)
row.names(df_cluster) = df_cluster$subject_nr
df_cluster = df_cluster %>% select(rt, accu, waytz_score, avg_ist)
df_cluster = scale(df_cluster)

distance = get_dist(df_cluster)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 = kmeans(df_cluster, centers = 2, nstart = 25)
str(k2)

fviz_cluster(k2, data = df_cluster)

k3 = kmeans(df_cluster, centers = 3, nstart = 25)
k4 = kmeans(df_cluster, centers = 4, nstart = 25)
k5 = kmeans(df_cluster, centers = 5, nstart = 25)

# plots to compare
p1 = fviz_cluster(k2, geom = "point", data = df_cluster) + ggtitle("k = 2")+theme_bw()
p2 = fviz_cluster(k3, geom = "point",  data = df_cluster) + ggtitle("k = 3")+theme_bw()

p3 = fviz_cluster(k4, geom = "point",  data = df_cluster) + ggtitle("k = 4")+theme_bw()
p4 = fviz_cluster(k5, geom = "point",  data = df_cluster) + ggtitle("k = 5")+theme_bw()

grid.arrange(p1, p2, p3, p4, nrow = 2)

set.seed(123)
fviz_nbclust(df_cluster, kmeans, method = "wss")
fviz_nbclust(df_cluster, kmeans, method = "silhouette")

# gap statistic to get SEs
gap_stat = clusGap(df_cluster, FUN = kmeans, nstart = 25,
                   K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

final = kmeans(df_cluster, 2, nstart = 25)
final2 = kmeans(df_cluster, 2, nstart = 25)

print(final)
fviz_cluster(final, data = df_cluster)

df_cluster = as.data.frame(df_cluster)

(df_cluster %>%
    mutate(Cluster = final$cluster) %>%
    group_by(Cluster) %>%
    summarise_all("mean"))

df_cluster_2 = df_cluster %>%
  mutate(Cluster = final$cluster,
         Cluster2 = final2$cluster) 

gc_cluster_data = df_cluster_2 %>%
  tibble::rownames_to_column("subject_nr") %>%
  select(subject_nr, Cluster) 

write.csv(gc_cluster_data, "cluster_data.csv")
