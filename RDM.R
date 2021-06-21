library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(dplyr)
library(Hmisc)
library(ggpubr)
library(ppcor)
library(tidyverse)
library(htmlTable)

# INITIAL DATA PREPERATION ----
setwd ("C:/Users/yuval/Downloads/Experiment 2 - RDM")
#setwd ("C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io")

# Get all file names in working directory above
files = list.files(pattern="*.csv")

# load all files into one datatable
dt = do.call(plyr::rbind.fill, lapply(files, fread))
dt <- data.table(dt)

dt <- dt[, .SD[run_id == min(run_id)], by = id] # keep only min run_id for each pp. Removes multiple runs for some pps.
finished_rdm <- unique(dt[trial_type == 'html-button-response', id]) # mark as finished if last trial exists

unfinished <- dt[!(id %in% finished_rdm)] # unfinished experiments are saved here before removal
dt <- dt[id %in% finished_rdm,] # keep only finished experiments

# make right type for each column
dt$Score <- as.numeric(dt$Score)
dt$RT <- as.numeric(dt$RT)
dt$ColCoh <- as.numeric(dt$ColCoh)
dt$Accuracy <- as.integer(as.logical(dt$Accuracy))

# Extract viewing distance
dt[, view_dist_mm:=as.double(view_dist_mm)] # make distance column as double instead of string
distance_rdm <- dt[!(is.na(view_dist_mm)),.(id, view_dist_mm_rdm = view_dist_mm)]

# take only main task rdm
only_rdm <- dt[TrialType == 'ColorTask']

# make summary of all participants
rdm_summary <- only_rdm[, list(accuracy=mean(Accuracy, na.rm = T), mean_rt=mean(RT, na.rm = T), score=max(Score)), by=id]
correct_rt <- only_rdm[Accuracy == 1, list(correct_rt=mean(RT, na.rm = T)), by=id]
wrong_rt <- only_rdm[Accuracy == 0, list(wrong_rt=mean(RT, na.rm = T)), by=id]

correct_rt_50 <-  only_rdm[Accuracy == 1 & ColCoh == 50, list(correct_rt_50=mean(RT, na.rm = T)), by=id]
correct_rt_56.4 <-  only_rdm[Accuracy == 1 & ColCoh == 56.4, list(correct_rt_56.4=mean(RT, na.rm = T)), by=id]
correct_rt_62.5 <-  only_rdm[Accuracy == 1 & ColCoh == 62.5, list(correct_rt_62.5=mean(RT, na.rm = T)), by=id]
correct_rt_73.6 <-  only_rdm[Accuracy == 1 & ColCoh == 73.6, list(correct_rt_73.6=mean(RT, na.rm = T)), by=id]

wrong_rt_50 <-  only_rdm[Accuracy == 0 & ColCoh == 50, list(wrong_rt_50=mean(RT, na.rm = T)), by=id]
wrong_rt_56.4 <-  only_rdm[Accuracy == 0 & ColCoh == 56.4, list(wrong_rt_56.4=mean(RT, na.rm = T)), by=id]
wrong_rt_62.5 <-  only_rdm[Accuracy == 0 & ColCoh == 62.5, list(wrong_rt_62.5=mean(RT, na.rm = T)), by=id]
wrong_rt_73.6 <-  only_rdm[Accuracy == 0 & ColCoh == 73.6, list(wrong_rt_73.6=mean(RT, na.rm = T)), by=id]

acc_50 <-  only_rdm[ColCoh == 50, list(acc_50=mean(Accuracy, na.rm = T)), by=id]
acc_56.4 <-  only_rdm[ColCoh == 56.4, list(acc_56.4=mean(Accuracy, na.rm = T)), by=id]
acc_62.5 <-  only_rdm[ColCoh == 62.5, list(acc_62.5=mean(Accuracy, na.rm = T)), by=id]
acc_73.6 <-  only_rdm[ColCoh == 73.6, list(acc_73.6=mean(Accuracy, na.rm = T)), by=id]

rt_50 <- only_rdm[ColCoh == 50, list(rt_50=mean(RT, na.rm = T)), by=id]
rt_56.4 <- only_rdm[ColCoh == 56.4, list(rt_56.4=mean(RT, na.rm = T)), by=id]
rt_62.5 <- only_rdm[ColCoh == 62.5, list(rt_62.5=mean(RT, na.rm = T)), by=id]
rt_73.6 <- only_rdm[ColCoh == 73.6, list(rt_73.6=mean(RT, na.rm = T)), by=id]

rt_all <- only_rdm[ColCoh != 50 & Accuracy == 1, list(rt_all=mean(RT, na.rm = T)), by=id]
acc_all <-  only_rdm[ColCoh != 50, list(acc_all=mean(Accuracy, na.rm = T)), by=id]



rdm_summary <- merge(rdm_summary, correct_rt, all = T) %>%
  merge(wrong_rt, all = T) %>%
  merge(rt_50, all = T) %>%
  merge(rt_56.4, all = T) %>%
  merge(rt_62.5, all = T) %>%
  merge(rt_73.6, all = T) %>%
  merge(correct_rt_50, all = T) %>%
  merge(correct_rt_56.4, all = T) %>%
  merge(correct_rt_62.5, all = T) %>%
  merge(correct_rt_73.6, all = T) %>%
  merge(wrong_rt_50, all = T) %>%
  merge(wrong_rt_56.4, all = T) %>%
  merge(wrong_rt_62.5, all = T) %>%
  merge(wrong_rt_73.6, all = T) %>%
  merge(acc_50, all = T) %>%
  merge(acc_56.4, all = T) %>%
  merge(acc_62.5, all = T) %>%
  merge(acc_73.6, all = T) %>%
  merge(distance_rdm, all = T) %>%
  merge(rt_all, all = T) %>%
  merge(acc_all, all = T)

rdm_summary <- rdm_summary[accuracy >= 0.6] # remove very bad participants

rdm_summary$acc_speed_50 <- (rdm_summary$acc_50/(rdm_summary$rt_50/1000))  # rt as seconds for nicer numbers
rdm_summary$acc_speed_56.4 <- (rdm_summary$acc_56.4/(rdm_summary$rt_56.4/1000))
rdm_summary$acc_speed_62.5 <- (rdm_summary$acc_62.5/(rdm_summary$rt_62.5/1000))
rdm_summary$acc_speed_73.6 <- (rdm_summary$acc_73.6/(rdm_summary$rt_73.6/1000))

rdm_summary$acc_speed_all <- (rdm_summary$acc_all/(rdm_summary$rt_all/1000))


# histogram of RDM accuracy
h = hist(rdm_summary$acc_all, breaks = c(seq(from = 0, to = 1, by = 0.05)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
p1 <- plot(h,freq=FALSE, main = "", xlab = "Average RT (ms)", ylab = "% of participants") + abline(v = mean(rdm_summary$acc_all, na.rm = T), col = 'red')

h = hist(rdm_summary$rt_all, breaks = c(seq(from = 0, to = 2000, by = 100)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
p2 <- plot(h,freq=FALSE, main = "", xlab = "Accuracy rate (%)", ylab = "% of participants") + abline(v = mean(rdm_summary$rt_all, na.rm = T), col = 'red')

h = hist(rdm_summary$acc_speed_all, breaks = c(seq(from = 0, to = 2, by = 0.1)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
p3 <- plot(h,freq=FALSE, main = "", xlab = "Accuracy/RT", ylab = "% of participants") + abline(v = mean(rdm_summary$acc_speed_all, na.rm = T), col = 'red')



library("cowplot")
plot_grid(p1,p2,ncol = 1, nrow = 2)

h1 <- ggplot(rdm_summary, aes(x = acc_all)) +
  geom_histogram(bins = 10, color = "black", fill = "white") +
  theme_gray((base_size = 14)) +
  geom_vline(aes(xintercept=mean(acc_all)),
             color="blue", linetype="dashed", size=1) +
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) +
  labs(title="a", x="Accuracy rate", y = "% of participants")

h2 <- ggplot(rdm_summary, aes(x = rt_all)) +
  geom_histogram(bins = 10, color = "black", fill = "white") +
  theme_gray((base_size = 14)) +
  geom_vline(aes(xintercept=mean(rt_all)),
             color="blue", linetype="dashed", size=1) +
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) +
  labs(title="b", x="Average RT (ms)", y = "% of participants")
  

h3 <- ggplot(rdm_summary, aes(x = acc_speed_all)) +
  geom_histogram(bins = 10, color = "black", fill = "white") +
  theme_gray((base_size = 14)) +
  geom_vline(aes(xintercept=mean(acc_speed_all)),
             color="blue", linetype="dashed", size=1) +
  aes(y=stat(count)/sum(stat(count))) + 
  scale_y_continuous(labels = scales::percent, limits = c(0,0.3)) +
  labs(title="c", x="Accuracy / RT", y = "% of participants") 
  
require(gridExtra)
require(ggpubr)
grid.arrange(h1, h2, h3, ncol=1)  




h = hist(rdm_summary$acc_speed_73.6, breaks = c(seq(from = 0, to = 2, by = 0.1)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "RDM acc/speed distribution - 73.6% coherence", xlab = "NPS (ms)", ylab = "% of participants")
abline(v = mean(rdm_summary$acc_speed_73.6, na.rm = T), col = 'red')

h = hist(rdm_summary$acc_all, breaks = c(seq(from = 0, to = 1, by = 0.05)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "RDM acc/speed distribution - 73.6% coherence", xlab = "NPS (ms)", ylab = "% of participants")
abline(v = mean(rdm_summary$acc_speed_73.6, na.rm = T), col = 'red')


ggplot(rdm_summary, aes(x = rt_all, y = acc_all)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )


# NOW BRING ALL_SUMMARY WITH ALL OTHER TASK RESULTS ----
setwd ("C:/Users/yuval/Downloads")
all_summary <- fread('all_summary.csv')

all_summary <- merge(all_summary, rdm_summary, by = 'id', all = T)


ggplot(all_summary, aes(x = view_dist_mm_rdm, y = view_dist_mm_2)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

cor.test(all_summary$view_dist_mm_rdm, all_summary$view_dist_mm_2)



all_summary[brms_quentile <= 2, group := 'best' ]
all_summary[brms_quentile >= 9, group := 'worst' ]

t.test(rt_56.4 ~ group, data = all_summary)


cor_matrix <- all_summary[,c('brms_rt_both','acc_all', 'rt_all', 'acc_speed_all')]

setnames(cor_matrix, 'rt_all', "Average RT")
setnames(cor_matrix, 'acc_all', "Accuracy rate")
setnames(cor_matrix, 'acc_speed_all', "IE")
#setnames(cor_matrix, 'control_rt_both', "Control RT both")
#setnames(cor_matrix, 'cb_rt_with_unfinished', "CB RT")

library(psych)
library(corrplot)
cor_results <- corr.test(cor_matrix, adjust = "none")
corrplot(cor_results$r, p.mat = cor_results$p, insig = "blank",type = "upper", method = "color",addCoef.col = "black", tl.col = "black")

# Make a table only for brms rt
p_for_correction <- cor_results$p["brms_rt_both",]
r_for_correction <- cor_results$r["brms_rt_both",]
bh_table <- data.table(cbind(r_for_correction, p_for_correction), keep.rownames = T)
bh_table <- bh_table[rn != "brms_rt_both"]

bh_table$corrected_p <- p.adjust(bh_table$p, method = "BH")
colnames(bh_table) <- c("Color RDM Measure","R", "p", "Corrected p")
bh_table[,2:4] <- round(bh_table[,2:4],2)
bh_table <- data.table(bh_table)
cols <- with(bh_table, ifelse(p <= 0.05, 'grey', 'white'))

bh_table <- htmlTable(as.matrix(bh_table), col.rgroup = cols)
bh_table

# FOR APPENDIX ----
cor_matrix <- all_summary[,c('brms_rt_both','acc_56.4', 'acc_62.5', 'acc_73.6','correct_rt_56.4','correct_rt_62.5','correct_rt_73.6', 'wrong_rt_56.4', 'wrong_rt_62.5', 'wrong_rt_73.6')] # for presentation only

cor_results <- corr.test(cor_matrix, adjust = "none")
corrplot(cor_results$r, p.mat = cor_results$p, insig = "blank",type = "upper", method = "color",addCoef.col = "black", tl.col = "black")

# Make a table only for brms rt
p_for_correction <- cor_results$p["brms_rt_both",]
r_for_correction <- cor_results$r["brms_rt_both",]
bh_table <- data.table(cbind(r_for_correction, p_for_correction), keep.rownames = T)
bh_table <- bh_table[rn != "brms_rt_both"]

bh_table$corrected_p <- p.adjust(bh_table$p, method = "BH")
colnames(bh_table) <- c("Measure","R", "p", "Corrected p")
bh_table[,2:4] <- round(bh_table[,2:4],2)
bh_table <- data.table(bh_table)
cols <- with(bh_table, ifelse(p <= 0.05, 'grey', 'white'))

bh_table <- htmlTable(as.matrix(bh_table), col.rgroup = cols)
bh_table


cor_matrix <- all_summary[,c('cb_acc','cb_rt_with_unfinished','alpha', 'acc_speed_all')]

setnames(cor_matrix, 'cb_acc', "60 s accuracy")
setnames(cor_matrix, 'cb_rt_with_unfinished', "CB RT")

cor_results <- corr.test(cor_matrix, adjust = "none")
corrplot(cor_results$r, p.mat = cor_results$p, insig = "blank",type = "full", method = "color",addCoef.col = "black", tl.col = "black")

# Make a table only for brms rt
p_for_correction <- cor_results$p["acc_speed_all",]
r_for_correction <- cor_results$r["acc_speed_all",]
bh_table <- data.table(cbind(r_for_correction, p_for_correction), keep.rownames = T)
bh_table <- bh_table[rn != "acc_speed_all"]

bh_table$corrected_p <- p.adjust(bh_table$p, method = "BH")
colnames(bh_table) <- c("CB measure","R", "p", "Corrected p")
bh_table[,2:4] <- round(bh_table[,2:4],2)
bh_table <- data.table(bh_table)
cols <- with(bh_table, ifelse(p < 0.01, 'grey', 'white'))

bh_table <- htmlTable(as.matrix(bh_table), col.rgroup = cols)
bh_table
