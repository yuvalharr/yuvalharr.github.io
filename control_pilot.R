library(data.table)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Downloads")
dt <- fread('brmscontrol-pilot.csv') 

dt <- dt[, rt:=as.double(as.character(rt))] # make rt column as double instead of string
dt <- dt[, acc:=as.numeric(as.character(acc))]
dt <- dt[, time_elapsed:=as.double(as.character(time_elapsed))]

dt <- dt[, bProblem:=as.double(as.character(bProblem))]
dt <- dt[, sProblem:=as.double(as.character(sProblem))]
dt <- dt[, trial:=as.numeric(as.character(trial))]
dt <- dt[, stimulus_alpha:=as.numeric(as.character(stimulus_alpha))]

dt <- dt[, id:=as.factor(id)] # make id column as factor instead of string
dt <- dt[, training:=as.logical(as.character(training))]
dt <- dt[, success:=as.logical(as.character(success))]
summary(dt)


only_brms <- dt[trial_type == 'bRMS']
only_brms <- only_brms[is.na(training),] # discard demo trials
summary(only_brms)

only_control <- dt[trial_type == 'control-bRMS']

# Clean brms data -------

# Keep only trials with good animation
only_brms <- only_brms[bProblem == 0 & sProblem < 5]

# Keep only correct trials
only_brms <- only_brms[acc == 1]
only_control <- only_control[(key_press == 'd' & stimulus_side == 0) | (key_press == 'k' & stimulus_side == 1)]

# Exclude short trials
only_brms <- only_brms[rt > 200]
only_control <- only_control[rt > 200]

# Exclude long trials
only_brms <- only_brms[rt < 28000]
only_control <- only_control[rt < 28000]

# Check number of correct trials
trialCount <- only_brms[, .(trials = .N), by = id]
control_trialCount <- only_control[, .(trials = .N), by = id]

# Divide control to different alphas
a_01 <- only_control[stimulus_alpha == 0.1]
a_02 <- only_control[stimulus_alpha == 0.2]
a_03 <- only_control[stimulus_alpha == 0.3]
a_04 <- only_control[stimulus_alpha == 0.4]
a_05 <- only_control[stimulus_alpha == 0.5]

# Exclude outlier trials per subject for each control alpha level
a_01[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
a_01 <- a_01[abs(zrt) < 3]
a_02[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
a_02 <- a_02[abs(zrt) < 3]
a_03[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
a_03 <- a_03[abs(zrt) < 3]
a_04[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
a_04 <- a_04[abs(zrt) < 3]
a_05[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
a_05 <- a_05[abs(zrt) < 3]


# Exclude outlier trials per subject
only_brms[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
only_brms <- only_brms[abs(zrt) < 3]

brms_summary <- only_brms[, mean_rt := mean(rt), by = id]
brms_summary <- only_brms[, .(brms_rt = mean(rt)), by = id]


control_summary <- only_control[, .(control_rt = mean(rt)), by = id]
a01_summary <- a_01[, .(a01_rt = mean(rt)), by = id]
a02_summary <- a_02[, .(a02_rt = mean(rt)), by = id]
a03_summary <- a_03[, .(a03_rt = mean(rt)), by = id]
a04_summary <- a_04[, .(a04_rt = mean(rt)), by = id]
a05_summary <- a_05[, .(a05_rt = mean(rt)), by = id]


all <- merge(control_summary, brms_summary) %>%
  merge(a01_summary) %>%
  merge(a02_summary) %>%
  merge(a03_summary) %>%
  merge(a04_summary) %>%
  merge(a05_summary)

colMeans(all[, 2:8])

ggplot(all, aes(x = brms_rt, y = a01_rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Correlation between brms and control")+
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(all, aes(x = brms_rt, y = a02_rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Correlation between brms and control")+
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )
ggplot(all, aes(x = brms_rt, y = a03_rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Correlation between brms and control")+
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )
ggplot(all, aes(x = brms_rt, y = a04_rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Correlation between brms and control")+
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )
ggplot(all, aes(x = brms_rt, y = a05_rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Correlation between brms and control")+
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )


