library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(dplyr)
library(Hmisc)

# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io")
dt <- read.csv('10.1.csv')
dt <- as.data.table(dt)

dt <- dt[, run_id:=as.numeric(as.character(run_id))] # make run_id numeric for taking minimum later
dt <- dt[, .SD[run_id == min(run_id)], by = id] # keep only min run_id for each pp. Removes multiple runs for some pps.
dt <- dt[dt$experiment_finished == 'true', ] # keep only finished experiments
dt = subset(dt, select = -c(condition.1)) # remove one 'condition' column (for some reason there are two)

#dt2 = subset(dt2, select = -c(condition)) # remove one 'condition' column (for some reason there are two) 
#dt <- rbind(dt, dt2)

# Convert column types
dt <- dt[, rt:=as.double(as.character(rt))] # make rt column as double instead of string
dt <- dt[, acc:=as.numeric(as.character(acc))]
dt <- dt[, time_elapsed:=as.double(as.character(time_elapsed))]
dt <- dt[, distance:=as.double(as.character(distance))]
dt <- dt[, bProblem:=as.double(as.character(bProblem))]
dt <- dt[, sProblem:=as.double(as.character(sProblem))]
dt <- dt[, trial:=as.numeric(as.character(trial))]
dt <- dt[, cb_trial:=as.numeric(as.character(cb_trial))]
dt <- dt[, id:=as.factor(id)] # make id column as factor instead of string
dt <- dt[, training:=as.logical(as.character(training))]
dt <- dt[, success:=as.logical(as.character(success))]
summary(dt)


# make two dt's of brms and CB
only_animation <- dt[trial_type == 'animation'] # take only animation trials
only_animation <- dt[cb_trial >= 1] # discard demo trials


only_brms <- dt[trial_type == 'bRMS']
only_brms <- only_brms[is.na(training),] # discard demo trials

# Clean brms data -------

# Keep only trials with good animation
only_brms <- only_brms[bProblem == 0 & sProblem < 5]

# keep only subjects with 33 good trials (75%)
trialCount <- only_brms[, .(trials = .N), by = id]
trialCount <- trialCount[trials >= 33] 
only_brms <- only_brms[id %in% trialCount$id]

# Keep only correct trials
only_brms <- only_brms[acc == 1]

  
# Exclude short trials
only_brms <- only_brms[rt > 200]

# Exclude long trials
only_brms <- only_brms[rt < 30000]

# keep only subjects with >30 correct trials (sufficiant for NPS accuracy)
trialCount <- only_brms[, .(trials = .N), by = id]
trialCount <- trialCount[trials >= 30]
only_brms <- only_brms[id %in% trialCount$id]
only_animation <- only_animation[id %in% trialCount$id]

# Exclude outlier trials per subject
only_brms[, zrt := scale(rt), by = id]  #ask yaniv: should i scale again after removing extreme z scores?
only_brms <- only_brms[abs(zrt) < 3]

brms_summary <- only_brms[, .(brms_rt = mean(rt), distance = mean(distance)), by = id]

# Clean CB data -------

only_animation[is.na(rt), success := NA] # make all 60 s trials unanswered instead of FALSE (or TRUE)
cb_num_wrongs <- only_animation[,.(cb_num_wrongs = sum(success == FALSE, na.rm = T)), by = id] # count number of wrong answers with response
only_animation <- only_animation[success == TRUE | is.na(success)] # remove all false trials with response ("guesses")
cb_acc <- only_animation[,.(cb_acc = sum(success == TRUE, na.rm = TRUE)/30), by = id] # returns cb correct rate (after removal of false guesses)

cb_rt <- only_animation[success == TRUE, .(cb_rt = mean(rt, na.rm = T)), by = id]

only_animation[is.na(success), rt := 60000]
cb_rt_with_unanswered_trials <- only_animation[, .(cb_rt_with_unfinished = mean(rt, na.rm = T)), by = id]

cb_summary <- merge(cb_rt, cb_acc) %>%
  merge(cb_num_wrongs) %>%
  merge(cb_rt_with_unanswered_trials)


all_summary <- merge(brms_summary, cb_summary, by = 'id')
only_animation <- merge(all_summary[,.(brms_rt, id)], only_animation) # add mean brms score for each sbj


boxplot(rt~id, data= only_animation)  
boxplot(rt~id, data= only_brms) 

# Violin plot for BRMS average RT per subject
p <- ggplot(brms_summary, aes(x=as.factor(1), y=brms_rt)) + 
  geom_violin()
p + geom_jitter(shape=16, position=position_jitter(0.03)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, color = "red")

# histogram of average BRMS
h = hist(all_summary$brms_rt, breaks = c(seq(from = 0, to = 30000, by = 2000)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)
abline(v = mean(all_summary$brms_rt), col = 'red')

p2 <- ggplot(cb_summary, aes(x=as.factor(1), y=cb_rt)) + 
  geom_violin()
p2 + geom_jitter(shape=16, position=position_jitter(0.03)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, color = "red")

p3 <- ggplot(cb_summary, aes(x=as.factor(1), y=cb_rt_with_unfinished)) + 
  geom_violin()
p3 + geom_jitter(shape=16, position=position_jitter(0.03)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, color = "red")

# histogram of average CB RT including unanswered trials as 60 s
h = hist(cb_summary$cb_rt_with_unfinished/1000, breaks = c(seq(from = 0, to = 30, by = 2.5)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)
abline(v = mean(cb_summary$cb_rt_with_unfinished/1000), col = 'red')

# histogram of average CB RT *not* including unanswered trials
h = hist(cb_summary$cb_rt/1000, breaks = c(seq(from = 0, to = 30, by = 2.5)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE)
abline(v = mean(cb_summary$cb_rt/1000), col = 'red')


# Connection between CB acc and CB RT
cor.test(all_summary$cb_rt, all_summary$cb_rt_with_unfinished)
ggplot(all_summary, aes(x = cb_rt, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$cb_rt, all_summary$cb_acc)
ggplot(all_summary, aes(x = cb_rt, y = cb_acc)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$cb_rt_with_unfinished, all_summary$cb_acc)
ggplot(all_summary, aes(x = cb_rt_with_unfinished, y = cb_acc)) +
  geom_point() +
  geom_smooth(method='lm')

# Make BRMS quentiles
library(dplyr)
all_summary$brms_quentile <- ntile(all_summary$brms_rt, 10)  

only_animation <- merge(only_animation, all_summary[, c("id","brms_quentile")], by="id")


# MAKE CB CUMULATIVE % CORRECT GRAPH PER SUBJECT ----

only_animation[is.na(success), rt := 99999] # make all unanswered trials have like infinate rt for asimptote in 60 s

only_animation <- only_animation[, brms_rt:=as.double(brms_rt)] # make rt column as double instead of string

mid <- mean(all_summary$brms_rt)
d.f <- arrange(only_animation,brms_rt,rt)

d.f.ecdf <- ddply(d.f, .(brms_rt), transform, ecdf=ecdf(rt)(rt) )
p <- ggplot( d.f.ecdf, aes(rt, ecdf, group = id, colour = brms_rt))
p + geom_line() + coord_cartesian(xlim = c(0, 60000), ylim = c(0, 1)) + scale_color_gradient2( low = "red", midpoint = mid, mid = "white",  #can use scale_color_gradient2 as well
                                                                                              high = "black", space = "Lab" ) +
    ggtitle('Accumulated CB proportion correct at each time point per participant') +
  xlab ('Cut-off point (ms)') + ylab ('% Correct')

# MAKE CB CUMULATIVE % CORRECT GRAPH PER  DECILE----

#mid <- mean(all_summary$brms_rt)
d.f <- only_animation[brms_quentile > 8 | brms_quentile < 3]
d.f <- arrange(d.f,brms_quentile,rt)

d.f.ecdf <- ddply(d.f, .(brms_quentile), transform, ecdf=ecdf(rt)(rt) )
p <- ggplot( d.f.ecdf, aes(rt, ecdf, group = brms_quentile, colour = factor(brms_quentile)))
p + geom_line() + coord_cartesian(xlim = c(0, 60000), ylim = c(0, 1)) + scale_color_brewer(type = "div", palette = "RdBu") +
  ggtitle('Accumulated CB proportion correct at each time point per brms decile') +
  xlab ('Cut-off point (ms)') + ylab ('% Correct')

only_middle <- only_animation[brms_quentile < 9 & brms_quentile > 2]
d.f <- arrange(only_middle,id,rt)

d.f.ecdf <- ddply(d.f, .(trial_type), transform, ecdf=ecdf(rt)(rt) )
p <- ggplot( d.f.ecdf, aes(rt, ecdf, group = trial_type))
p + geom_line() + coord_cartesian(xlim = c(0, 60000), ylim = c(0, 1)) +
  ggtitle('Accumulated CB proportion correct at each time point per brms decile') +
  xlab ('Cut-off point (ms)') + ylab ('% Correct')

# Check correlation of BRMS and CB (without unfinished) between participants
cor.test(all_summary$brms_rt, all_summary$cb_rt)
ggplot(all_summary, aes(x = brms_rt, y = cb_rt)) +
  geom_point() +
  geom_smooth(method='lm')

# Check correlation of BRMS and CB (with unfinished trials) between participants
cor.test(all_summary$brms_rt, all_summary$cb_rt_with_unfinished)
ggplot(all_summary, aes(x = brms_rt, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm')

# Check correlation of BRMS and CB between deciles
cor.test(all_summary[brms_quentile > 8 | brms_quentile < 3]$brms_rt, all_summary[brms_quentile > 8 | brms_quentile < 3]$cb_rt_with_unfinished)
ggplot(all_summary[brms_quentile > 8 | brms_quentile < 3], aes(x = brms_rt, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm')

# Check kendall tau of BRMS and CB RT
plot(all_summary$brms_rt, all_summary$cb_rt_with_unfinished, col = "blue")
cor.test(all_summary$brms_rt, all_summary$cb_rt_with_unfinished, method = "kendall")

# Check for correlations with distance
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



