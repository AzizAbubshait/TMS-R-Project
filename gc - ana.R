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

gc %>% group_by(Site) %>%
  filter(subject_nr != 0) %>%
  summarize(n = n_distinct(subject_nr))

# check how many were removed based on SD ----

gc %>% 
  group_by(
    subject_nr
  ) %>%
  mutate(
    validity = recode(validity, valid="invalid", invalid = "valid"),
    accu_percent = sum(correct == 1)/length(correct)
  ) %>%
  group_by(
    subject_nr#, gazeCond, validity
  ) %>% # get correct trials only
  mutate(
    fast_resp = case_when(response_time <= 100 ~ 1,
                          response_time >= 100 ~ 0),
    slow_resp = case_when(response_time >= 1500 ~ 1,
                          response_time <= 1500 ~ 0),
    sd_out = case_when(response_time > abs(mean(response_time)+2*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-2*sd(response_time)) ~ 1)) %>% 
  summarize(fast_resp = sum(fast_resp == 1, na.rm = T),
            slow_resp = sum(slow_resp == 1, na.rm = T),
            TRIAL_VALID = sum(TRIAL_VALID == 0, na.rm = T),
            sd_out = sum(sd_out == 1, na.rm = T),
            err = sum(correct == 0, na.rm = T)) %>%
  filter(subject_nr != 0) %>%
  print()

gc_dat %>%
  group_by(subject_nr) %>%
  summarize(accu = first(accu_percent)) %>%
  print()

gc_dat %>% group_by(subject_nr, Site) %>%
  summarize(n_trials = n())

gc_dat %>% group_by(gazeCond, validity, Site) %>%
    summarize(rt = mean(response_time)) %>% print()

ggplot(gc_dat, aes(response_time, fill = validity))+
  geom_density(alpha = .3)+theme_bw()+
  scale_fill_manual(values = cbbPalette)

avg_gc = gc_dat %>%
  group_by(subject_nr, Site, validity, gazeCond, Session) %>%
  summarize(rt = mean(response_time))

ggplot(avg_gc, aes(validity, y = rt))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

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

ggplot(avg_gc, aes(gazeCond, rt, color = validity))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Site, scales = "free_y")+theme_bw()

avg_gc_long = avg_gc %>%
  pivot_wider(names_from = validity, values_from = rt) %>%
  mutate(gc = invalid-valid) 
avg_gc_long$gc_sd = scale(avg_gc_long$gc, center = T, scale = T)

avg_gc_long %>%
  ggplot(aes(gc_sd, fill = Site))+
  geom_histogram(bins = 30, position = position_dodge())+
  geom_vline(xintercept = 2.5, linetype = "dotted")+
  geom_vline(xintercept = -2.5, linetype = "dotted")+
  geom_vline(xintercept = 3, linetype = "dashed")+
  geom_vline(xintercept = -3, linetype = "dashed")+
  theme_bw()

avg_gc_long %>%
  ggplot(aes(gc, fill = Site))+
  geom_histogram(bins = 30, position = position_dodge())+
  theme_bw()

ggplot(avg_gc_long, aes(gazeCond, gc))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

ggplot(avg_gc_long, aes(Site, gc, color = gazeCond, fill = gazeCond))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))
avg_gc_long %>%
  #filter(gc_sd < 2.5) %>%
  ggplot(aes(Site, gc, color = gazeCond))+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+theme_bw()

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

ggplot(avg_gc_long, aes(Site, gc, color = gazeCond))+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1,
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  geom_point() +
  geom_line(aes(group = interaction(subject_nr, gazeCond))) +
  theme_bw()


# tms with ratings ----
rat_dat = read.csv("aggregated_ratings.csv")

avg_gc_rat = avg_gc_long %>%
  left_join(rat_dat, by = "subject_nr")

ggplot(avg_gc_rat, aes(avg_ist, gc, color = gazeCond))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  facet_wrap(~Site)

ggplot(avg_gc_rat, aes(waytz_score, avg_ist))+
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
