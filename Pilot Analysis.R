library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(Hmisc)

# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io")
dt <- fread('pilot_10.csv')
dt <- dt[, rt:=as.double(rt)] # make rt column as double instead of string
dt <- dt[, acc:=as.double(acc)] # make rt column as double instead of string

only_brms <- dt[trial_type == 'bRMS']
trialCount <- only_brms[, .(trials = .N), by = run_id] 
trialCount <- trialCount[trials >= 48] # minimum of brms trials needed to finish exp
good_pps <- dt[run_id %in% trialCount$run_id] # keep only subjects who finished all brms trials

# make two dt's of brms and CB
only_animation <- good_pps[trial_type == 'animation'] # take only animation trials
only_animation <- only_animation[trial_index >15] # discard demo trials



only_brms <- good_pps[trial_type == 'bRMS'] # take only bRMS trials
only_brms <- only_brms[trial > 0] # discard demo trials

acc_column <- only_brms[,.(brms_acc = mean(acc)), by = id]
brms_summary <- only_brms[acc == 1, .(brms_rt = mean(rt), distance = mean(distance)), by = id]
brms_summary <- merge(brms_summary, acc_column)
cb_summary <- only_animation[success == 'true', .(cb_rt = mean(rt, na.rm = T)), by = id]
all_summary <- merge(brms_summary, cb_summary)
only_animation <- merge(all_summary[,.(brms_rt, id)], only_animation) # add mean brms score for each sbj


boxplot(rt~id, data= only_animation)  
boxplot(rt~id, data= only_brms)  
# MAKE CB CUMULATIVE % CORRECT GRAPH PER SUBJECT ----

only_animation[is.na(rt), success := NA] # make all 60 s trials unanswered instead of FALSE
only_animation <- only_animation[success == 'true' | is.na(success)]
only_animation[is.na(success), rt := 99999] # make all unanswered trials have infinate rt for asimptote in 60 s

only_animation <- only_animation[, brms_rt:=as.double(brms_rt)] # make rt column as double instead of string

mid <- mean(all_summary$brms_rt)
d.f <- arrange(only_animation,brms_rt,rt)

d.f.ecdf <- ddply(d.f, .(brms_rt), transform, ecdf=ecdf(rt)(rt) )
p <- ggplot( d.f.ecdf, aes(rt, ecdf, group = id, colour = brms_rt))
p + geom_line() + coord_cartesian(xlim = c(0, 60000), ylim = c(0, 1)) + scale_color_gradient2(midpoint = mid, low = "blue", mid = "white",
                                                                                              high = "red", space = "Lab" ) +
    ggtitle('Accumulated CB proportion correct at each time point per participant') +
  xlab ('Cut-off point (ms)') + ylab ('% Correct')

cor.test(all_summary$brms_rt, all_summary$cb_rt)
ggplot(all_summary, aes(x = brms_rt, y = cb_rt)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$brms_rt, all_summary$distance)
ggplot(all_summary, aes(x = brms_rt, y = distance)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$cb_rt, all_summary$distance)
ggplot(all_summary, aes(x = cb_rt, y = distance)) +
  geom_point() +
  geom_smooth(method='lm')

# from Yanivs analysis ----

cleanData <- function(x, excludeSubjects = NULL) {
  # Code a missing response as wrong response
  x$Acc[x$Acc != 1] <- 0
  
  # Exclude subjects by their debrief answers
  x <- subset(x, !(Subject %in% excludeSubjects))
  
  # Calculate mean accuracy per subject
  acc <- aggregate(x[c("Subject","Acc")], by = list(x$Subject), FUN = mean)
  print(nrow(acc)) # Print number of subjects prior to exclusion
  acc <- subset(acc, Acc >= .9) # select by acuuracy criterion
  print(nrow(acc)) # Print number of subjects after exclusion
  x <- subset(x, Subject %in% acc$Subject)
  
  # Exclude trials by accuracy
  pre <- nrow(x)
  x <- subset(x, Acc == 1)
  print('Excluded by accuracy')
  print(c(pre - nrow(x), (pre - nrow(x))/pre)) # Print trial nubmer before, after exlclusion
  
  # Exclude trials by minimum response time criterion
  tmp <- nrow(x)
  x <- subset(x, RT > .2)
  print("RT > .2")
  print(c(tmp - nrow(x), (tmp - nrow(x))/pre)) # Print trial nubmer before, after exlclusion
  
  # Standardize per subject
  x$ZRT <- ave(x$RT, x$Subject, FUN = scale)
  
  # Exclude by 3 SD exclusion criterion
  tmp <- nrow(x)
  x <- subset(x, abs(ZRT) < 3)
  x$ZRT <- NULL
  print("ZRT < 3")
  print(c(tmp - nrow(x), (tmp - nrow(x))/pre)) # Print trial nubmer before, after exlclusion
  
  # Standardize
  x$ZRT <- ave(x$RT, x$Subject, FUN = scale)
  
  return(x)
}



