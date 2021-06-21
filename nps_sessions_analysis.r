library(data.table)
library(ggplot2)
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
setwd ("C:/Users/yuval/Downloads/Experiment 1")
# Get all file names in working directory above
first_files = list.files(pattern="*.csv")
# load all files into one datatable
first = do.call(plyr::rbind.fill, lapply(first_files, fread))
first <- data.table(first)

setwd ("C:/Users/yuval/Downloads/Experiment 2 - brms+control")
# Get all file names in working directory above
second_files = list.files(pattern="*.csv")
# load all files into one datatable
second = do.call(plyr::rbind.fill, lapply(second_files, fread))
second <- data.table(second)

# LOOK AT TRYING TO SWITCH THE REFRESH ELIMINATION WITH RUN ID DUPLICATES (LIKE IN THE OTHER SCRIPTS) !!!!!!!!!!!!!!!!!!!!!!!!!!
#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# all participants that have refreshed and will be discarded next - 
# maybe can try and take from them pps that refreshed after fps check
#refreshed_1 <- first %>%
#  group_by(id) %>%
#  filter(length(unique(start_time)) > 1) %>%
#  ungroup()
#refreshed_1 <- data.table(refreshed_1)
#unique(refreshed_1$id)

#refreshed_2 <- second %>%
#  group_by(id) %>%
#  filter(length(unique(start_time)) > 1) %>%
#  ungroup()
#refreshed_2 <- data.table(refreshed_2)
#unique(refreshed_2$id)

# list of all participants that submitted to more than one part 2 HIT
#cheaters <- c('A1MAT8TVA0P9PU', 'A3PR7FCPPZ88JR', 'A2KMED79GMTHF9','A161PY8QJHF3IK', 'A179M1VP5QMHNW','A39FJ8DCK0T28','A1JADORW8YJNOT','A28GWWTH0HOFCE', 'AC1NK8NJ84V8Y','A1WIRNAFVTFNJL','A1BDPHQIAOC1WB')

# delete all participants with more than one start-time (meaning they refreshed page)
#first <- first %>%
#  group_by(id) %>%
#  filter(length(unique(start_time)) == 1) %>%
#  ungroup()

#second <- second %>%
#  group_by(id) %>%
#  filter(length(unique(start_time)) == 1) %>%
#  ungroup()


first <- first[, run_id:=as.numeric(as.character(run_id))] # make run_id numeric for taking minimum later
first <- first[, .SD[run_id == min(run_id)], by = id] # keep only min run_id for each pp. Removes multiple runs for some pps.
finished_exp_1 <- unique(first[cb_trial == 30, id]) # mark as finished if last cb trial exists
unfinished_1 <- first[!(id %in% finished_exp_1)] # unfinished experiments are saved here before removal
first <- first[id %in% finished_exp_1,] # keep only finished experiments

second <- second[, run_id:=as.numeric(as.character(run_id))] # make run_id numeric for taking minimum later
second <- second[, .SD[run_id == min(run_id)], by = id] # keep only min run_id for each pp. Removes multiple runs for some pps.
second <- second[, control_trial:=as.numeric(as.character(control_trial))]
finished_exp_2 <- unique(second[control_trial == 11, id]) # mark as finished if last control trial exists
unfinished_2 <- first[!(id %in% finished_exp_2)] # unfinished experiments are saved here before removal
second <- second[id %in% finished_exp_2,] # keep only finished experiments

# count participants
length(unique(first$id))
length(unique(second$id))


# Extract viewing distance
first[, view_dist_mm:=as.double(view_dist_mm)] # make distance column as double instead of string
second[, view_dist_mm:=as.double(view_dist_mm)] # make distance column as double instead of string


distance_1 <- first[!(is.na(view_dist_mm)),.(id, view_dist_mm_1 = view_dist_mm)]
distance_2 <- second[!(is.na(view_dist_mm)),.(id, view_dist_mm_2 = view_dist_mm)]

# take only brms
only_brms_1 <- first[trial_type == 'bRMS']
only_brms_1 <- only_brms_1[, training:=as.logical(as.character(training))]
only_brms_1 <- only_brms_1[is.na(training),] # discard demo trials

only_brms_2 <- second[trial_type == 'bRMS']
only_brms_2 <- only_brms_2[, training:=as.logical(as.character(training))]
only_brms_2 <- only_brms_2[is.na(training),] # discard demo trials



# Convert column types
only_brms_1 <- only_brms_1[, rt:=as.double(as.character(rt))] # make rt column as double instead of string
only_brms_1 <- only_brms_1[, acc:=as.numeric(as.character(acc))]
only_brms_1 <- only_brms_1[, time_elapsed:=as.double(as.character(time_elapsed))]
only_brms_1 <- only_brms_1[, bProblem:=as.double(as.character(bProblem))]
only_brms_1 <- only_brms_1[, sProblem:=as.double(as.character(sProblem))]
only_brms_1 <- only_brms_1[, trial:=as.numeric(as.character(trial))]
only_brms_1 <- only_brms_1[, id:=as.factor(id)] # make id column as factor instead of string
only_brms_1 <- only_brms_1[, success:=as.logical(as.character(success))]
only_brms_2 <- only_brms_2[, rt:=as.double(as.character(rt))] # make rt column as double instead of string
only_brms_2 <- only_brms_2[, acc:=as.numeric(as.character(acc))]
only_brms_2 <- only_brms_2[, time_elapsed:=as.double(as.character(time_elapsed))]
only_brms_2 <- only_brms_2[, bProblem:=as.double(as.character(bProblem))]
only_brms_2 <- only_brms_2[, sProblem:=as.double(as.character(sProblem))]
only_brms_2 <- only_brms_2[, trial:=as.numeric(as.character(trial))]
only_brms_2 <- only_brms_2[, id:=as.factor(id)] # make id column as factor instead of string
only_brms_2 <- only_brms_2[, success:=as.logical(as.character(success))]
summary(only_brms_1)
summary(only_brms_2)



# Clean brms data -------

# Keep only trials with good animation
only_brms_1 <- only_brms_1[bProblem == 0 & sProblem < 5]
only_brms_2 <- only_brms_2[bProblem == 0 & sProblem < 5]

both_brms <- rbind(only_brms_1, only_brms_2, fill = T) # make one big dt for both NPS

# keep only subjects with 30 good BRMS trials
only_brms_1 <- only_brms_1[id %in% trialCount_1$id]
only_brms_2 <- only_brms_2[id %in% trialCount_2$id]

trialCount_both <- both_brms[, .(trials = .N), by = id]

# Keep only correct trials
only_brms_1 <- only_brms_1[acc == 1]
only_brms_2 <- only_brms_2[acc == 1]
both_brms <- both_brms[acc == 1]

# Exclude short trials
only_brms_1 <- only_brms_1[rt > 200]
only_brms_2 <- only_brms_2[rt > 200]
both_brms <- both_brms[rt > 200]


# Exclude long trials
only_brms_1 <- only_brms_1[rt < 28000]
only_brms_2 <- only_brms_2[rt < 28000]
both_brms <- both_brms[rt < 28000]


# keep only subjects with >30 correct trials (sufficiant for NPS accuracy)
trialCount_1 <- only_brms_1[, .(trials = .N), by = id]
trialCount_2 <- only_brms_2[, .(trials = .N), by = id]
trialCount_both <- both_brms[, .(trials = .N), by = id]


trialCount_1 <- trialCount_1[trials >= 30]
trialCount_2 <- trialCount_2[trials >= 30]
trialCount_both <- trialCount_both[trials >= 30]


only_brms_1 <- only_brms_1[id %in% trialCount_1$id]
only_brms_2 <- only_brms_2[id %in% trialCount_2$id]
both_brms <- both_brms[id %in% trialCount_both$id]

# Exclude outlier trials per subject
only_brms_1[, zrt := scale(rt), by = id]
only_brms_1 <- only_brms_1[abs(zrt) < 3]
only_brms_2[, zrt := scale(rt), by = id]
only_brms_2 <- only_brms_2[abs(zrt) < 3]
both_brms[, zrt := scale(rt), by = id]
both_brms <- both_brms[abs(zrt) < 3]

# make time columns
brms_summary_1 <- only_brms_1[, .(brms_rt_1= mean(rt), day_1= substr(tail(start_time,1), 6, 7), hour_1= substr(tail(start_time,1), 18,19), minute_1= substr(tail(start_time,1), 21,22)), by = id]
brms_summary_2 <- only_brms_2[, .(brms_rt_2= mean(rt), day_2= substr(tail(start_time,1), 6, 7), hour_2= substr(tail(start_time,1), 18,19), minute_2= substr(tail(start_time,1), 21,22)), by = id]
brms_summary_both <- both_brms[, .(brms_rt_both= mean(rt)), by = id]

# merge two studies
all_brms_summary <- merge(brms_summary_1, brms_summary_2, by = 'id', all = T)
all_brms_summary <- merge(all_brms_summary, brms_summary_both, by = 'id', all = T)

all_brms_summary[, hour_1:=as.double(as.character(hour_1))]
all_brms_summary[, hour_2:=as.double(as.character(hour_2))]
all_brms_summary[, minute_1:=as.double(as.character(minute_1))]
all_brms_summary[, minute_2:=as.double(as.character(minute_2))]
all_brms_summary[, day_1:=as.double(as.character(day_1))]
all_brms_summary[, day_2:=as.double(as.character(day_2))]

all_brms_summary$day_diff <- all_brms_summary[,.(day_diff = (day_2 - day_1)*24)] # take number of days and turn into hours
all_brms_summary$hour_diff <- all_brms_summary[,.(hour_diff = hour_2 - hour_1)] # take hour difference
all_brms_summary$min_diff <- all_brms_summary[,.(min_diff = (minute_2 - minute_1)/60)] # take hour difference
all_brms_summary$time_diff <- all_brms_summary[,.(time_diff = day_diff+hour_diff+min_diff)] # make column for time interval betwean the two sessions
all_brms_summary[, c("day_diff","hour_diff", "min_diff","day_1","hour_1", "minute_1", "day_2","hour_2", "minute_2"):=NULL]


all_brms_summary$combined_brms <- rowMeans(all_brms_summary[,.(brms_rt_1, brms_rt_2)], na.rm = T) # make column - Both sessions brms

distance_1 <- distance_1[id %in% all_brms_summary$id]
distance_2 <- distance_2[id %in% all_brms_summary$id]

all_brms_summary <- merge(all_brms_summary,distance_1, by = 'id', all = T)
all_brms_summary <- merge(all_brms_summary,distance_2, by = 'id', all = T)

# correlation between brms rt in each session
ggplot(all_brms_summary, aes(x = brms_rt_1, y = brms_rt_2)) +
  geom_point(aes(color = time_diff), size = 3) +
  scale_color_viridis_c()  +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

# correlation between diff in NPS and time interval length (hours)
ggplot(all_brms_summary, aes(x = time_diff, y = abs(brms_rt_1-brms_rt_2))) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )


# correlation between distances in both sessions
ggplot(all_brms_summary, aes(x = view_dist_mm_1, y = view_dist_mm_2)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

# correlation between distance and NPS
ggplot(all_brms_summary, aes(x = view_dist_mm_1, y = brms_rt_1)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(all_brms_summary, aes(x = view_dist_mm_2, y = brms_rt_2)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )



# LOOK AT CONTROL TASK ---------------------------------------------------------------------

only_control_1 <- first[trial_type == 'control-bRMS']
only_control_2 <- second[trial_type == 'control-bRMS']

# Convert column types
only_control_1 <- only_control_1[, rt:=as.double(as.character(rt))] # make rt column as double instead of string
only_control_1 <- only_control_1[, acc:=as.numeric(as.character(acc))]
only_control_1 <- only_control_1[, time_elapsed:=as.double(as.character(time_elapsed))]
only_control_1 <- only_control_1[, bProblem:=as.double(as.character(bProblem))]
only_control_1 <- only_control_1[, sProblem:=as.double(as.character(sProblem))]
only_control_1 <- only_control_1[, trial:=as.numeric(as.character(trial))]
only_control_1 <- only_control_1[, id:=as.factor(id)] # make id column as factor instead of string
only_control_1 <- only_control_1[, training:=as.logical(as.character(training))]
only_control_1 <- only_control_1[, success:=as.logical(as.character(success))]
only_control_2 <- only_control_2[, rt:=as.double(as.character(rt))] # make rt column as double instead of string
only_control_2 <- only_control_2[, acc:=as.numeric(as.character(acc))]
only_control_2 <- only_control_2[, time_elapsed:=as.double(as.character(time_elapsed))]
only_control_2 <- only_control_2[, bProblem:=as.double(as.character(bProblem))]
only_control_2 <- only_control_2[, sProblem:=as.double(as.character(sProblem))]
only_control_2 <- only_control_2[, trial:=as.numeric(as.character(trial))]
only_control_2 <- only_control_2[, id:=as.factor(id)] # make id column as factor instead of string
only_control_2 <- only_control_2[, training:=as.logical(as.character(training))]
only_control_2 <- only_control_2[, success:=as.logical(as.character(success))]
summary(only_control_1)
summary(only_control_2)


# Clean control data -------

# Keep only trials with good animation
#only_control_1 <- only_control_1[bProblem == 0 & sProblem < 5]
#only_control_2 <- only_control_2[bProblem == 0 & sProblem < 5]

# keep only subjects with ? good control trials
controlCount_1 <- only_control_1[, .(trials = .N), by = id]
controlCount_2 <- only_control_2[, .(trials = .N), by = id]


# Keep only correct trials
only_control_1 <- only_control_1[acc == 1]
only_control_2 <- only_control_2[acc == 1]

# Exclude short trials
only_control_1 <- only_control_1[rt > 200]
only_control_2 <- only_control_2[rt > 200]

# Exclude long trials
only_control_1 <- only_control_1[rt < 28000]
only_control_2 <- only_control_2[rt < 28000]

# Combine both controls
both_control <- rbind(only_control_1, only_control_2, fill = T) # make one big dt for both Control

# keep only subjects with >30 correct trials (sufficiant for NPS accuracy)
controlCount_1 <- only_control_1[, .(trials = .N), by = id]
controlCount_2 <- only_control_2[, .(trials = .N), by = id]

controlCount_both <- both_control[, .(trials = .N), by = id]


controlCount_1 <- controlCount_1[trials >= 8]
controlCount_2 <- controlCount_2[trials >= 8]

only_control_1 <- only_control_1[id %in% controlCount_1$id]
only_control_2 <- only_control_2[id %in% controlCount_2$id]

controlCount_both <- controlCount_both[trials >= 8] # remove participants without enough control trials (in both together)
both_control <- both_control[id %in% controlCount_both$id]

# Exclude outlier trials per subject
only_control_1[, zrt := scale(rt), by = id]
only_control_1 <- only_control_1[abs(zrt) < 3]
only_control_2[, zrt := scale(rt), by = id]
only_control_2 <- only_control_2[abs(zrt) < 3]
both_control[, zrt := scale(rt), by = id]
both_control <- both_control[abs(zrt) < 3]


# merge both controls
control_summary_1 <- only_control_1[, .(control_rt_1= mean(rt)) , by = id]
control_summary_2 <- only_control_2[, .(control_rt_2= mean(rt)) , by = id]
control_summary_both <- both_control[, .(control_rt_both= mean(rt)) , by = id]


all_control_summary <- merge(control_summary_1, control_summary_2, by = 'id', all = T)
all_control_summary <- merge(all_control_summary, control_summary_both, by = 'id', all = T)


# merge with brms summary
all_brms_summary <- merge(all_brms_summary, all_control_summary, all = T)

# correlation between control rt in each session
ggplot(all_brms_summary, aes(x = control_rt_1, y = control_rt_2)) +
  geom_point(aes(color = time_diff), size = 3) +
  scale_color_viridis_c()  +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

# correlation between brms rt and control - in session 1
ggplot(all_brms_summary, aes(x = brms_rt_1, y = control_rt_1)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

# correlation between brms rt and control - in session 2
ggplot(all_brms_summary, aes(x = brms_rt_2, y = control_rt_2)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

# correlation between brms rt and control - in both sessions
ggplot(all_brms_summary, aes(x = brms_rt_both, y = control_rt_both)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

# correlation between brms rt and control - in different sessions
ggplot(all_brms_summary, aes(x = brms_rt_1, y = control_rt_2)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(all_brms_summary, aes(x = brms_rt_2, y = control_rt_1)) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

ggplot(all_brms_summary, aes(x = time_diff, y = abs(control_rt_1-control_rt_2))) +
  geom_point() +
  geom_smooth(method='lm') +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )


# take questions from exp 2a ----
only_quest <- first[trial_type == 'survey-text' | trial_type == 'survey-likert' | trial_type == 'survey-multi-choice'] # switch from first to second to choose
only_quest <- only_quest[id %in% all_brms_summary$id]
only_quest$responses <- gsub(':",', ':9,', only_quest$responses)
only_quest$responses <- gsub('\"\"', '\"', only_quest$responses)
only_quest$responses <- gsub(':\"}', ':\"\"}', only_quest$responses)

# keep only relevant columns
only_quest <- data.frame(only_quest)
homogenous = apply(only_quest, 2, function(var) length(unique(var)) == 1) #tag columns without any variance
only_quest <- only_quest[, !homogenous] # remove them

# extract values
only_quest <- data.table(only_quest)

only_quest[internal_node_id == "0.0-15.0-1.0", gender := fromJSON(responses)$'gender', by = id]
only_quest[internal_node_id == "0.0-15.0-1.0", hand := fromJSON(responses)$'hand', by = id]
only_quest[internal_node_id == "0.0-15.0-1.0", native := fromJSON(responses)$'native', by = id]
only_quest[internal_node_id == "0.0-15.0-2.0", age := fromJSON(responses)$'age', by = id]
only_quest[internal_node_id == "0.0-15.0-3.0", add := fromJSON(responses)$'add', by = id]

gender_dt <- only_quest[!is.na(gender), list(id, gender, hand, native)]
age_dt <- only_quest[!is.na(age), list(id, age)]

dems <- merge(gender_dt, age_dt)

length(dems[gender == 'Female', id])
dems[,age := as.numeric(as.character(age))]
mean(dems[,age])
sd(dems[,age])
max(dems[,age])
min(dems[,age])


# merge dems with exp data
all_brms_summary <- merge(dems, all_brms_summary)

length(all_brms_summary[!is.na(brms_rt_1),id])
mean(all_brms_summary[!is.na(brms_rt_1),brms_rt_1])
sd(all_brms_summary[!is.na(brms_rt_1),brms_rt_1])

length(all_brms_summary[!is.na(brms_rt_2),id])
mean(all_brms_summary[!is.na(brms_rt_2),brms_rt_2])
sd(all_brms_summary[!is.na(brms_rt_2),brms_rt_2])


# take questions from exp 2b ----

only_quest <- second[trial_type == 'survey-text' | trial_type == 'survey-likert' | trial_type == 'survey-multi-choice'] # switch from first to second to choose
only_quest <- only_quest[id %in% all_brms_summary$id]
only_quest$responses <- gsub(':",', ':9,', only_quest$responses)
only_quest$responses <- gsub('\"\"', '\"', only_quest$responses)
only_quest$responses <- gsub(':\"}', ':\"\"}', only_quest$responses)

# keep only relevant columns
only_quest <- data.frame(only_quest)
homogenous = apply(only_quest, 2, function(var) length(unique(var)) == 1) #tag columns without any variance
only_quest <- only_quest[, !homogenous] # remove them

# extract values
only_quest <- data.table(only_quest)

only_quest[internal_node_id == "0.0-12.0-1.0", q1 := fromJSON(responses)$'q1', by = id]
only_quest[internal_node_id == "0.0-12.0-1.0", q2 := fromJSON(responses)$'q2', by = id]
only_quest[internal_node_id == "0.0-12.0-1.0", q3 := fromJSON(responses)$'q3', by = id]
only_quest[internal_node_id == "0.0-12.0-1.0", q4 := fromJSON(responses)$'q4', by = id]
only_quest[internal_node_id == "0.0-12.0-2.0", q5 := fromJSON(responses)$'q5', by = id]
only_quest[internal_node_id == "0.0-12.0-2.0", q6 := fromJSON(responses)$'q6', by = id]
only_quest[internal_node_id == "0.0-12.0-2.0", q7 := fromJSON(responses)$'q7', by = id]
only_quest[internal_node_id == "0.0-12.0-2.0", q8 := fromJSON(responses)$'q8', by = id]
only_quest[internal_node_id == "0.0-12.0-3.0", q9 := fromJSON(responses)$'q9', by = id]
only_quest[internal_node_id == "0.0-12.0-3.0", q10 := fromJSON(responses)$'q10', by = id]
only_quest[internal_node_id == "0.0-12.0-3.0", q11 := fromJSON(responses)$'q11', by = id]
only_quest[internal_node_id == "0.0-12.0-3.0", q12 := fromJSON(responses)$'q12', by = id]

only_quest[internal_node_id == "0.0-12.0-4.0", driver := fromJSON(responses)$'is_driver', by = id]
only_quest[internal_node_id == "0.0-13.0-0.0", driving_abilities := responses, by = id]
only_quest[internal_node_id == "0.0-13.0-1.0", driver_accidents := responses, by = id]
only_quest[internal_node_id == "0.0-14.0-0.0", pedestrian_accidents := fromJSON(responses), by = id]


qa_dt <- only_quest[!is.na(q1), list(id, q1, q2, q3, q4)]
qb_dt <- only_quest[!is.na(q5), list(id, q5, q6, q7, q8)]
qc_dt <- only_quest[!is.na(q9), list(id, q9, q10, q11, q12)]

driver_dt <- only_quest[!is.na(driver), list(id, driver)]
abilities_dt <- only_quest[!is.na(driving_abilities), list(id, driving_abilities)]
driver_accidents_dt <- only_quest[!is.na(driver_accidents), list(id, driver_accidents)]
pedestrian_accidents_dt <- only_quest[!is.na(pedestrian_accidents), list(id, pedestrian_accidents)]

dems <- merge(qa_dt, qb_dt, all = T) %>%
  merge(qc_dt, all = T) %>%
  merge(driver_dt, all = T)  %>%
  merge(abilities_dt, all = T) %>%
  merge(driver_accidents_dt, all = T) %>%
  merge(pedestrian_accidents_dt, all = T)


length(dems[gender == 'Female', id])
dems[,age := as.numeric(as.character(age))]
mean(dems[,age])
sd(dems[,age])
max(dems[,age])
min(dems[,age])


dems[!(is.na(driving_abilities)), c('driving_ability', 'driving_accidents') := 
       list(as.numeric(fromJSON(driving_abilities)), as.numeric(fromJSON(driver_accidents))), 
     by = id]
dems[, c("driving_abilities","driver_accidents"):=NULL]

# remove 9s now
is.na(dems) <- dems == 9
dems <- dems[,pedestrian_accidents := as.numeric(pedestrian_accidents)] # make numeric
dems <- dems[,driving_accidents := as.numeric(driving_accidents)] # make numeric

dems$total_hsp <- rowMeans(dems[,2:13], na.rm = T, dims = 1)
dems$total_accidents <- rowSums(dems[, c('driving_accidents', 'pedestrian_accidents')], na.rm = T)

# merge dems with exp data
all_brms_summary <- merge(dems, all_brms_summary, all = T)
all_brms_summary[pedestrian_accidents > 10, pedestrian_accidents := NA] # remove people who said more then 10 pedestrian accidents



# write summary csv
write.csv(all_brms_summary,"C:/Users/yuval/Downloads/both_sessions.csv", row.names = FALSE)
