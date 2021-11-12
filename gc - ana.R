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
# this is a manual process
rand_seq_dat = data.frame(
  subject_nr = c(1:20),
  ran_seq = c(3,2,5,1,6,4,1,2,3,4,5,6,1,2,3,4,5,6,1,2)
)
# preprocess
gc_dat = gc %>% 
  group_by(
    subject_nr
    ) %>%
  mutate(
    validity = recode(validity, valid="invalid", invalid = "valid"),
    accu_percent = sum(correct == 1)/length(correct)
    # rand_seq = case_when(Site == "TPJ" & "Session" == > abs(mean(response_time)+3*sd(response_time)) ~ 1, which sequence?
    #                      response_time < abs(mean(response_time)-3*sd(response_time)) ~ 1)
    ) %>%
  filter(
    correct == 1,
    response_time > 250,
    response_time < 1200,
    TRIAL_VALID == 1
  ) %>%
  group_by(
    subject_nr, gazeCond, validity, Session
           ) %>% # get correct trials only
  mutate(
    rm_trial = case_when(response_time > abs(mean(response_time)+3*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-3*sd(response_time)) ~ 1)) %>%
  filter(
    is.na(rm_trial) == T,
    subject_nr != 0
    ) %>%
  left_join(
    rand_seq_dat, by = "subject_nr"
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
    subject_nr, Session#, gazeCond, validity
  ) %>% # get correct trials only
  mutate(
    fast_resp = case_when(response_time <= 250 ~ 1,
                          response_time >= 250 ~ 0),
    slow_resp = case_when(response_time >= 1200 ~ 1,
                          response_time <= 1200 ~ 0),
    sd_out = case_when(response_time > abs(mean(response_time)+3*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-3*sd(response_time)) ~ 1)) %>% 
  summarize(fast_resp = sum(fast_resp == 1, na.rm = T),
            slow_resp = sum(slow_resp == 1, na.rm = T),
            TRIAL_VALID = sum(TRIAL_VALID == 0, na.rm = T),
            sd_out = sum(sd_out == 1, na.rm = T),
            err = sum(correct == 0, na.rm = T)) %>%
  filter(subject_nr != 0) %>%
  mutate(total = fast_resp+slow_resp+TRIAL_VALID+sd_out+err) %>%
  mutate(percent_rm = total/128*100) %>%
  print()

gc_dat %>%
  group_by(subject_nr) %>%
  summarize(accu = first(accu_percent)) %>%
  print()

gc_dat %>% group_by(subject_nr, Site, Session) %>%
  summarize(n_trials = n())
gc %>% group_by(subject_nr, Site, Session, validity) %>%
  summarize(n_trials = n()) %>% print()

gc_dat %>% group_by(gazeCond, validity, Site, Session) %>%
    summarize(rt = mean(response_time)) %>% print()

avg_gc = gc_dat %>%
  group_by(subject_nr, validity, gazeCond, Session, Site) %>%
  mutate(Session = as.factor(Session)) %>%
  summarize(rt = mean(response_time))

ggplot(avg_gc, aes(rt, fill = validity))+
  geom_density(alpha = .3)+theme_bw()+
  scale_fill_manual(values = cbbPalette)+
  facet_grid(Site~gazeCond, scales = "free_y")

ggplot(avg_gc, aes(rt, fill = validity))+
  geom_histogram(position = position_dodge())+theme_bw()+
  scale_fill_manual(values = cbbPalette)

ggplot(avg_gc, aes(rt, fill = validity))+
  geom_histogram(color = "black")+theme_bw()+
  scale_fill_manual(values = cbbPalette)+
  scale_x_continuous(breaks = c(seq(200,1300,50)))

ggplot(avg_gc, aes(gazeCond, rt, color = validity, fill = validity))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_grid(Session~Site, scales = "free")

ggplot(avg_gc, aes(gazeCond, rt, color = validity, fill = validity))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Session)

ggplot(avg_gc, aes(gazeCond, rt, color = validity))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Session)+theme_bw()

ggplot(avg_gc, aes(gazeCond, rt, color = validity))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Site, scales = "free_y")+theme_bw()

#avg_gc$rt_scale = scale(avg_gc$rt)
avg_gc_long = avg_gc %>%
  #select(-rt) %>%
  pivot_wider(names_from = validity, values_from = rt) %>%
  mutate(gc = invalid-valid,
         diseng = valid+invalid) %>% 
  mutate(diff = diseng - gc) %>% print(n = Inf)
avg_gc_long$gc_sd = scale(avg_gc_long$gc, center = T, scale = T)

avg_gc_long %>%
  ggplot(aes(gc_sd, fill = Site))+
  geom_histogram(bins = 30, position = position_dodge())+
  geom_vline(xintercept = 2.5, linetype = "dotted", size = 1)+
  geom_vline(xintercept = -2.5, linetype = "dotted", size = 1)+
  geom_vline(xintercept = 3, linetype = "dashed", size = 1)+
  geom_vline(xintercept = -3, linetype = "dashed", size = 1)+
  theme_bw()

avg_gc_long %>%
  ggplot(aes(gc, fill = Site))+
  geom_histogram(bins = 30, position = position_dodge())+
  geom_vline(xintercept = quantile(avg_gc_long$gc)[2], linetype = "dotted", size = 1)+
  geom_vline(xintercept = quantile(avg_gc_long$gc)[4], linetype = "dotted", size = 1)+
  geom_vline(xintercept = quantile(avg_gc_long$gc)[1], linetype = "dashed", size = 1)+
  geom_vline(xintercept = quantile(avg_gc_long$gc)[5], linetype = "dashed", size = 1)+
  theme_bw()

bad_pees = c(12)
avg_gc_long %>%
filter(!subject_nr %in% bad_pees) %>%
  ggplot(aes(Session, gc))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

avg_gc_long %>%
  filter(!subject_nr %in% bad_pees) %>%
  ggplot(aes(Session, gc, color = gazeCond))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

# ggplot(avg_gc_long, aes(gazeCond, diseng))+
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
#                position = position_dodge(.5), color = "black")+
#   stat_summary(fun.data = mean_se, geom = "point", size = 5,
#                position = position_dodge(.5))
# 
# ggplot(avg_gc_long, aes(gazeCond, diff))+
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
#                position = position_dodge(.5), color = "black")+
#   stat_summary(fun.data = mean_se, geom = "point", size = 5,
#                position = position_dodge(.5))

avg_gc_long %>%
  filter(!subject_nr %in% bad_pees) %>%
  ggplot(aes(Site, gc, color = gazeCond, fill = gazeCond))+
  geom_flat_violin(position = position_nudge(.2), alpha = .4, lwd = .5, color = "black")+theme_bw()+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_fill_manual(values = cbbPalette)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

avg_gc_long %>%
  filter(!subject_nr %in% bad_pees) %>%
  ggplot(aes(Site, gc, color = gazeCond))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+theme_bw()+
  geom_hline(yintercept = 0, linetype = "dashed")

# avg_gc_long %>%
#   filter(!subject_nr %in% bad_pees) %>%
#   ggplot(aes(Site, diseng, color = gazeCond))+
#   scale_color_manual(values = cbbPalette)+
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
#                position = position_dodge(.5))+
#   stat_summary(fun.data = mean_se, geom = "point", size = 5,
#                position = position_dodge(.5))+theme_bw()
# 
# avg_gc_long %>%
#   filter(!subject_nr %in% bad_pees) %>%
#   ggplot(aes(Site, diff, color = gazeCond))+
#   scale_color_manual(values = cbbPalette)+
#   stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
#                position = position_dodge(.5))+
#   stat_summary(fun.data = mean_se, geom = "point", size = 5,
#                position = position_dodge(.5))+theme_bw()

avg_gc_long %>%
  ggplot(aes(Site, gc, color = gazeCond))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+theme_bw()+
  facet_wrap(~subject_nr, scales = "free_y")+
  geom_hline(yintercept = 0, linetype = "dashed")

ggplot(avg_gc_long, aes(Site, gc, color = gazeCond))+
  #geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1,
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  geom_point() +
  geom_line(aes(group = interaction(subject_nr, gazeCond))) +
  theme_bw()

ggplot(avg_gc_long, aes(Site, gc, color = gazeCond))+
  geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1,
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  theme_bw()

# maybe do mutual - avoiding 
avg_gc_long %>%
  filter(!subject_nr %in% bad_pees) %>%
  select(subject_nr, gazeCond, Session, Site, gc) %>% 
  pivot_wider(names_from = gazeCond, values_from = gc) %>%
  mutate(gc_diff = mutual- avoiding) %>%
  ggplot(aes(Site, gc_diff, color = Session))+
  #geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1,
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  geom_point() +
  geom_line(aes(group = interaction(subject_nr))) +
  theme_bw()

# tms with ratings ----
rat_dat = read.csv("aggregated_ratings.csv")

avg_gc_rat = avg_gc_long %>%
  left_join(rat_dat, by = "subject_nr") %>%
  filter(!subject_nr %in% bad_pees) 

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

ggplot(avg_gc_rat, aes(waytz_score, avg_ist))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()

ggplot(avg_gc_rat, aes(gc, avg_ist))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(avg_gc_rat, aes(gc, waytz_score))+
  geom_point()+
  geom_smooth(method = "lm")

