library(data.table)
library(ggplot2)
library(dplyr)


# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Downloads/experiment-2-rdm")

# Get all file names in working directory above
files = list.files(pattern="*.csv")

# load all files into one datatable
dt = do.call(rbind, lapply(files, fread))

# make right type for each column
dt$Score <- as.numeric(dt$Score)
dt$RT <- as.numeric(dt$RT)
dt$Accuracy <- as.integer(as.logical(dt$Accuracy))

# take only main task rdm
only_rdm <- dt[TrialType == 'ColorTask']

# make summary of all participants
summary <- only_rdm[, list(accuracy=mean(Accuracy, na.rm = T), mean_rt=mean(RT, na.rm = T), score=max(Score)), by=id]

# make summary by id and coherence
groupColumns = c("id","ColCoh")
dataColumns = c("RT", "Accuracy")
#byCoherence = ddply(only_rdm, groupColumns, function(x) colMeans(x[dataColumns], na.rm = T))

# also try thinking maybe two rt columns is better. 1 for correct 1 for false.
res <- aggregate (only_rdm[,'RT'], 
                 by = list(id = only_rdm$id,  coh = only_rdm$ColCoh, acc = only_rdm$Accuracy), 
                 FUN = mean, na.rm = T)
res <- data.table(res)

ggplot(byCoherence, aes(x = ColCoh, y = Accuracy)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(res[acc == 1,], aes(x = coh, y = RT)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(only_rdm[Accuracy == 1,], aes(x = ColCoh, y = RT)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(only_rdm[Accuracy == 0,], aes(x = ColCoh, y = RT)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(res[acc == 0,], aes(x = coh, y = RT)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

cor.test(summary$accuracy, summary$mean_rt)
