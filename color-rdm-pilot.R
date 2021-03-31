library(data.table)
library(ggplot2)

# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Downloads")
dt <- fread('color-rdm (2).csv')
dt$Score <- as.numeric(dt$Score)
dt$RT <- as.numeric(dt$RT)
dt$Accuracy <- as.integer(as.logical(dt$Accuracy))

dt[ , max(Score, na.rm = T), by = run_id]

dt[ , mean(Accuracy, na.rm = T), by = run_id]
dt[ , mean(RT, na.rm = T), by = run_id]
summary <- dt[, list(accuracy=mean(Accuracy, na.rm = T), mean_rt=mean(RT, na.rm = T)), by=run_id]


only_rdm <- dt[trial_type == 'wl-rdm'] # take only rdm trials
only_rdm <- dt[TrialType == "ColorTask"] # discard demo and other trials
dt[ , c(mean(Score, na.rm = T), mean(RT, na.rm = T)), by = run_id]

ggplot(summary, aes(x = accuracy, y = mean_rt)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(summary$accuracy, summary$mean_rt)
