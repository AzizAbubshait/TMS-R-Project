library(tidyverse)
library(PupillometryR)
# load packages

cbbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# load packages

# gc pre-process ----
gc_path = paste(getwd(), "/raw gc/", sep = "")
filenames = list.files(gc_path)

gc = lapply(paste(gc_path, filenames, sep = ""), read.csv) 
gc = do.call(plyr::rbind.fill, gc)

# preprocess
gc_dat = gc %>% 
  group_by(
    subject_nr
    ) %>%
  mutate(
    validity = recode(validity, valid="invalid", invalid = "valid"),
    accu_percent = sum(correct == 1)/length(correct)
  ) %>%
  filter(
    correct == 1,
    response_time > 100,
    response_time < 1500,
    TRIAL_VALID == 1
  ) %>%
  group_by(
    subject_nr, gazeCond, validity
           ) %>% # get correct trials only
  mutate(
    rm_trial = case_when(response_time > abs(mean(response_time)+2*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-2*sd(response_time)) ~ 1)) %>%
  filter(
    is.na(rm_trial) == T,
    subject_nr != 0
    )

gc %>% group_by(subject_nr, Site) %>%
  filter(TRIAL_VALID == 0) %>%
  summarize(n_trials = n())

gc_dat %>%
  group_by(subject_nr) %>%
  summarize(accu = first(accu_percent)) %>%
  print()

gc_dat %>% group_by(subject_nr, Site) %>%
  summarize(n_trials = n())

gc_dat %>% group_by(gazeCond, validity, Site) %>%
    summarize(rt = mean(response_time)) %>% print()

ggplot(gc_dat, aes(gazeCond, response_time, color = validity, fill = validity))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Site, scales = "free_y")

avg_gc = gc_dat %>%
  group_by(subject_nr, Site, validity, gazeCond, Session) %>%
  summarize(rt = mean(response_time))

ggplot(avg_gc, aes(gazeCond, rt, color = validity, fill = validity))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Site, scales = "free_y")

avg_gc_long = avg_gc %>%
  pivot_wider(names_from = validity, values_from = rt) %>%
  mutate(gc = invalid-valid)

ggplot(avg_gc_long, aes(Site, gc, color = gazeCond, fill = gazeCond))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

# ggplot(avg_gc_long, aes(Site, gc, color = gazeCond, fill = gazeCond, group = subject_nr))+
#   geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
#   scale_fill_manual(values = cbbPalette)+
#   scale_color_manual(values = cbbPalette)+
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
#                position = position_dodge(.5))+
#   stat_summary(fun.data = mean_se, geom = "point", size = 5,
#                position = position_dodge(.5))+
#   geom_point() + 
#   geom_line() + 
#   theme_bw()
# 

# tms with ratings ----
rat_dat = read.csv("aggregated_ratings.csv")

avg_gc_rat = avg_gc_long %>%
  left_join(rat_dat, by = "subject_nr")

ggplot(avg_gc_rat, aes(avg_ist, gc, color = gazeCond))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(~Site)

ggplot(avg_gc_rat, aes(waytz_score, gc, color = gazeCond))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(~Site)

ggplot(avg_gc_rat, aes(gc, avg_ist))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(avg_gc_rat, aes(gc, waytz_score))+
  geom_point()+
  geom_smooth(method = "lm")
