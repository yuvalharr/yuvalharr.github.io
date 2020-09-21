library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(Hmisc)

setwd ("C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io")
dt <- fread('cognitive-experiment- pilot.csv')
dt <- dt[, rt:=as.double(rt)] # make rt column as int instead of string

#check total running times

only_animation_detection<- dt[trial_type == 'animation' | trial_type =='image-button-response'] # take only animation trials
only_animation_detection <- only_animation_detection[trial_index >15] # discard demo trials
ordered <- only_animation_detection[order(run_id, -rt)] # order by rt for each subject
only_animation_detection[, .(trial_type)]
ans <- only_animation_detection[trial_type == 'animation',
               .(
                 mean_rt = mean(rt, na.rm = T),
                 task_finish_time = max(time_elapsed),
                 task_start_time = min(time_elapsed)
                 ),
                 by = .(run_id)]
viewing_distance <- dt[trial_type == 'virtual-chin', .(run_id, viewing_distance_cm)]
mili_to_min <- ans[,.(run_id, task_finish_min = ans$task_finish_time/1000/60, task_start_min = task_start_time/1000/60)]
ans <- merge(ans, mili_to_min)
ans <- merge(ans, viewing_distance)
ans$total_task_time_min <- ans[,task_finish_min-task_start_min]

ggplot(ans, aes(x=viewing_distance_cm, y=mean_rt)) + geom_point()

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



