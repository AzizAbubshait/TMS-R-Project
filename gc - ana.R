library(tidyverse)
library(PupillometryR)
# load packages

cbbPalette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# gc pre-process ----
gc_path = paste(getwd(), "/raw gc/", sep = "")
filenames = list.files(gc_path)

gc = lapply(paste(gc_path, filenames, sep = ""), read.csv) 
gc = do.call(plyr::rbind.fill, gc)
# this is a manual process
rand_seq_dat = data.frame(
  subject_nr = c(1:24),
  ran_seq = c(3,2,5,1,6,4,1:6,1:6,1:6)
)

# ppt sequence

# First, we preprocess the data ----
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
    response_time > 250,
    response_time < 1200,
    TRIAL_VALID == 1
  ) %>%
  mutate(
    rm_trial = case_when(response_time > abs(mean(response_time)+2.5*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-2.5*sd(response_time)) ~ 1)
  ) %>%
  group_by(
    subject_nr, gazeCond, validity, Site
           ) %>% 
  # mutate(
  #   # rm_trial = case_when(response_time > abs(mean(response_time)+3*sd(response_time)) ~ 1,
  #   #                      response_time < abs(mean(response_time)-3*sd(response_time)) ~ 1)
  #   ) %>%
  filter(
    is.na(rm_trial) == T,
    subject_nr != 0
    ) %>%
  left_join(
    rand_seq_dat, by = "subject_nr"
  )

# lets see how many ppts we have in each group
gc %>% group_by(Site) %>%
  filter(subject_nr != 0) %>%
  summarize(n = n_distinct(subject_nr))

# Lets now check how many trials we removed from each participant ---- 
gc %>% 
  group_by(
    subject_nr, Session
  ) %>%
  mutate(
    validity = recode(validity, valid="invalid", invalid = "valid"),
    accu_percent = sum(correct == 1)/length(correct),
    fast_resp = case_when(response_time <= 150 ~ 1,
                          response_time >= 150 ~ 0),
    slow_resp = case_when(response_time >= 1500 ~ 1,
                          response_time <= 1500 ~ 0)) %>%
  group_by(
    subject_nr, gazeCond, validity, Session
    ) %>% 
    mutate(
      sd_out = case_when(response_time > abs(mean(response_time)+3*sd(response_time)) ~ 1,
                         response_time < abs(mean(response_time)-3*sd(response_time)) ~ 1)
      ) %>% 
  group_by(
    subject_nr, Session
  ) %>%
  summarize(fast_resp = sum(fast_resp == 1, na.rm = T),
            slow_resp = sum(slow_resp == 1, na.rm = T),
            TRIAL_VALID = sum(TRIAL_VALID == 0, na.rm = T),
            sd_out = sum(sd_out == 1, na.rm = T),
            err = sum(correct == 0, na.rm = T)) %>%
  filter(subject_nr != 0) %>%
  mutate(total = fast_resp+slow_resp+TRIAL_VALID+sd_out+err) %>%
  mutate(percent_rm = total/128*100) %>%
  print(n = Inf)

# what about ppts accuracy rate? ----
gc_dat %>%
  group_by(subject_nr, Session) %>%
  summarize(accu = first(accu_percent)) %>%
  print(n = Inf)
# number of trials remaining?
gc_dat %>% group_by(subject_nr, Site, Session) %>%
  summarize(n_trials = n()) %>% print(n = Inf)

# now we average to get RTs ----
avg_gc = gc_dat %>%
  group_by(subject_nr, validity, gazeCond, Session, Site, ran_seq) %>%
  mutate(Session = as.factor(Session),
         ran_seq = as.factor(ran_seq)) %>%
  summarize(rt = mean(response_time))

# now lets check some distributions ----
ggplot(avg_gc, aes(rt, fill = validity))+
  geom_density(alpha = .3)+theme_bw()+
  scale_fill_manual(values = cbbPalette)

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

# Now lets look at some plots ----

# Raincloud plot: Stim X Gaze X Validity
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

# Raincloud plot: Stim X Gaze X Sequence X validity
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

# Raincloud plot: Gaze X Sequence X validity
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

# Point-mean plot: Validity X Stim X Gaze
ggplot(avg_gc, aes(gazeCond, rt, color = validity))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Site)+theme_bw()

# Point-mean plot: Validity X Sequence X Gaze
ggplot(avg_gc, aes(gazeCond, rt, color = validity))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  facet_wrap(~Session)+theme_bw()

# now lets calculate GCE ----
#avg_gc$rt_scale = scale(avg_gc$rt)
avg_gc_long = avg_gc %>%
  #select(-rt) %>%
  pivot_wider(names_from = validity, values_from = rt) %>%
  mutate(gc = invalid-valid,
         diseng = valid+invalid) %>% 
  mutate(diff = diseng - gc) %>% print(n = Inf)
avg_gc_long$gc_sd = scale(avg_gc_long$gc, center = T, scale = T)
avg_gc_long$ran_seq = factor(avg_gc_long$ran_seq, levels = c(1:6))

# lets look at GCs in z-scores
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

bad_pees = c(12, 2, 3, 14)
good_pees = c(1, 6, 9, 10, 15, 16, 21)

avg_gc_long %>%
#  filter(!subject_nr %in% bad_pees) %>%
  ggplot(aes(ran_seq, gc, color = gazeCond))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

avg_gc_long %>%
  #filter(!subject_nr %in% bad_pees) %>%
  filter(subject_nr %in% good_pees) %>%
  ggplot(aes(Session, gc))+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1, 
               position = position_dodge(.5), color = "black")+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))

avg_gc_long %>%
  #filter(!subject_nr %in% bad_pees) %>%
  filter(subject_nr %in% good_pees) %>%
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
  #filter(!subject_nr %in% bad_pees) %>%
  filter(subject_nr %in% good_pees) %>%
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
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~ran_seq)

avg_gc_long %>%
  filter(
 #   !subject_nr %in% bad_pees,
#    !ran_seq %in% c("5","6")
         ) %>%
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
  filter(!subject_nr %in% bad_pees) %>%
  ggplot(aes(Site, gc, color = gazeCond))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "point", size = 5
               )+theme_bw()+
  facet_wrap(~subject_nr, scales = "free_y")+
  geom_line(aes(group = Site))+
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
  ggplot(aes(Site, gc_diff))+
  #geom_point(position = position_jitterdodge(jitter.width = .1,dodge.width = 0.5), alpha = .4)+
  #scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .1,
               position = position_dodge(.5))+
  stat_summary(fun.data = mean_se, geom = "point", size = 5,
               position = position_dodge(.5))+
  geom_point() +
  geom_line(aes(group = as.factor(subject_nr), color = as.factor(subject_nr))) +
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

# maybe plot Site by instance and Waytz scores
avg_gc_rat %>%
  mutate(
    ist_split = ifelse(avg_ist < mean(avg_gc_rat$avg_ist), "lo", ifelse(
    avg_ist >= mean(avg_gc_rat$avg_ist), "hi", NA))
  ) %>%
  ggplot(aes(Site, gc, color = gazeCond))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 5)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2), width = .1)+
  facet_wrap(~ist_split)+theme_bw()

avg_gc_rat %>%
  mutate(
    waytz_split = ifelse(waytz_score < mean(avg_gc_rat$waytz_score), "lo", ifelse(
      waytz_score >= mean(avg_gc_rat$waytz_score), "hi", NA))
  ) %>%
  ggplot(aes(Site, gc, color = gazeCond))+
  scale_color_manual(values = cbbPalette)+
  stat_summary(fun.data = mean_se, geom = "point", position = position_dodge(.2), size = 5)+
  stat_summary(fun.data = mean_se, geom = "errorbar", position = position_dodge(.2), width = .1)+
  facet_wrap(~waytz_split)+theme_bw()

