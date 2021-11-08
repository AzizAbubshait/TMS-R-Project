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
  group_by(subject_nr) %>%
  mutate(
    validity = recode(validity, valid="invalid", invalid = "valid")
  ) %>%
  filter(
    correct == 1,
    response_time > 100,
    response_time < 1500,
    TRIAL_VALID == 1
  ) %>%# remove incorrect responses
  group_by(subject_nr, gazeCond, validity) %>% # get correct trials only
  mutate(rm_trial = case_when(response_time > abs(mean(response_time)+2*sd(response_time)) ~ 1,
                              response_time < abs(mean(response_time)-2*sd(response_time)) ~ 1)) %>%
  filter(is.na(rm_trial) == T)
(gc_dat %>% group_by(gazeCond, validity) %>%
    summarize(rt = mean(response_time)))

(gc_dat %>% group_by(gazeCond, validity) %>%
  summarize(rt = mean(response_time)))

ggplot(gc_dat, aes(gazeCond, response_time, color = validity, fill = validity))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Site)

avg_gc = gc_dat %>%
  group_by(subject_nr, Site, validity, gazeCond) %>%
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
  facet_wrap(~Site)
