library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(Hmisc)

# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io")
dt <- fread('2nd-page-pilot.csv')
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

acc_column <- only_brms[,.(brms_acc = mean(acc)), by = subject_id]
brms_summary <- only_brms[acc == 1, .(brms_rt = mean(rt), distance = mean(distance)), by = subject_id]
brms_summary <- merge(brms_summary, acc_column)
cb_summary <- only_animation[!is.na(rt) & success == TRUE, .(cb_rt = mean(rt, na.rm = T)), by = subject_id]
all_summary <- merge(brms_summary, cb_summary)
only_animation <- merge(all_summary[,.(brms_rt, subject_id)], only_animation) # add mean brms score for each sbj


boxplot(rt~subject_id, data= only_animation)  
boxplot(rt~subject_id, data= only_brms)  
# MAKE CB CUMULATIVE % CORRECT GRAPH PER SUBJECT ----

only_animation[is.na(rt), success := NA] # make all 60 s trials unanswered instead of FALSE
only_animation <- only_animation[success == TRUE | is.na(success)]
only_animation[is.na(success), rt := 66666] # make all unanswered trials have rt bigger then 60 s for graph

only_animation <- only_animation[, brms_rt:=as.double(brms_rt)] # make rt column as double instead of string

d.f <- arrange(only_animation,brms_rt,rt)
d.f.ecdf <- ddply(d.f, .(brms_rt), transform, ecdf=ecdf(rt)(rt) )
p <- ggplot( d.f.ecdf, aes(rt, ecdf, group = subject_id, colour = brms_rt))
p + geom_line()



# TO STACK OVERFLOW
set.seed(125)
dat <- data.frame(
  subject = c(rep(c("A"), 5), rep(c("B"), 5), rep(c("C"), 5)),
  color_factor = c(rep(0.3, 5), rep(0.6,5), rep(0.9,5)),
  rt = sample(1:50, 15, replace =T)
)

dat <- arrange(dat,color_factor,rt)
dat.ecdf <- ddply(dat, .(color_factor), transform, ecdf=ecdf(rt)(rt) )
p <- ggplot( dat.ecdf, aes(rt, ecdf, colour = subject)) + geom_line()
p2 <- ggplot( dat.ecdf, aes(rt, ecdf, colour = color_factor)) + geom_line()
p3 <- ggplot( dat.ecdf, aes(rt, ecdf, group = subject, colour = color_factor)) + geom_line()

p4 <- ggplot( dat.ecdf, aes(rt, ecdf, colour = subject,group=subject)) + geom_line()+
  scale_color_manual(values = c('lightblue','blue','darkblue'))+
  labs(color='Subject')

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



