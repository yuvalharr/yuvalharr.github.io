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

# INITIAL DATA PREPERATION ----

setwd ("C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io")
dt <- read.csv('28.1.csv')
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

# keep only subjects with 30 good BRMS trials
trialCount <- only_brms[, .(trials = .N), by = id]
trialCount <- trialCount[trials >= 30] 
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

only_animation[is.na(rt) | rt > 60500, success := NA] # make all 60 s trials unanswered instead of FALSE (or TRUE)
cb_num_wrongs <- only_animation[,.(cb_num_wrongs = sum(success == FALSE, na.rm = T)), by = id] # count number of wrong answers with response
only_animation <- only_animation[success == TRUE | is.na(success)] # remove all false trials with response ("guesses")
only_animation$id <- droplevels(only_animation$id) # drop unused id (factor) levels

count_id_rows <- data.table(table(only_animation$id)) # count number of trials for each pps (without false "presses")
setnames(count_id_rows, 1, "id")
only_animation <-  merge(only_animation, count_id_rows) # add column - number of vadlid trials for each participant
only_animation$N <- as.numeric(only_animation$N)

cb_acc <- only_animation[,.(cb_acc = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
cb_acc <- aggregate(cb_acc[,c("cb_acc")], by = list(id = cb_acc$id), FUN = mean)
cb_rt <- only_animation[success == TRUE, .(cb_rt = mean(rt, na.rm = T)), by = id]

only_animation[is.na(success), rt := 60500]
cb_rt_with_unanswered_trials <- only_animation[, .(cb_rt_with_unfinished = mean(rt, na.rm = T)), by = id]

cb_summary <- merge(cb_rt, cb_acc) %>%
  merge(cb_num_wrongs) %>%
  merge(cb_rt_with_unanswered_trials)


all_summary <- merge(brms_summary, cb_summary, by = 'id')
only_animation <- merge(only_animation, all_summary[,.(brms_rt, id)]) # add mean brms score for each sbj


boxplot(rt~id, data= only_animation)  
boxplot(rt~id, data= only_brms) 

# Violin plot for BRMS average RT per subject
p <- ggplot(brms_summary, aes(x=as.factor(1), y=brms_rt)) + 
  geom_violin()
p + geom_jitter(shape=16, position=position_jitter(0.03)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, color = "red")

# histogram of average BRMS
h = hist(all_summary$brms_rt, breaks = c(seq(from = 0, to = 30000, by = 1000)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "NPS distribution", xlab = "NPS (ms)", ylab = "% of participants")
abline(v = mean(all_summary$brms_rt), col = 'red')

p2 <- ggplot(cb_summary, aes(x=as.factor(1), y=cb_rt)) + 
  geom_violin()
p2 + geom_jitter(shape=16, position=position_jitter(0.03)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, color = "red")

p3 <- ggplot(cb_summary, aes(x=as.factor(1), y=cb_rt_with_unfinished)) + 
  geom_violin()
p3 + geom_jitter(shape=16, position=position_jitter(0.03)) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=3, color = "red")

# histogram of average CB RT *including* unanswered trials as 60 s
h = hist(cb_summary$cb_rt_with_unfinished/1000, breaks = c(seq(from = 0, to = 30, by = 2.5)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h$density = h$counts/sum(h$counts)*100
plot(h,freq=FALSE, main = "CB scores distribution", xlab = "Average CB rt (sec)", ylab = "% of participants")
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
  geom_smooth(method='lm') +
  geom_abline(slope=1, intercept = 0, color = "red", linetype = "dashed") +
  #scale_y_continuous(breaks = seq(0, 30000, by = 10000)) +
  #scale_x_continuous(breaks = seq(0, 30000, by = 10000))
  scale_x_continuous(expand = c(0, 500), limits = c(0, NA), breaks = seq(0, 30000, by = 10000)) + 
  scale_y_continuous(expand = c(0, 500), limits = c(0, NA), breaks = seq(0, 30000, by = 10000))

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


# ---- MAKE CB CUMULATIVE % CORRECT GRAPH PER SUBJECT ---- BY MYSELF as there are problems in ecdf
d.f <- arrange(only_animation,brms_rt,rt)

d.f <- as.data.table(d.f)
d.f[ , percent := 1/N] # set "jump" for every correct answer 
d.f <- d.f[is.na(success), c("percent", "rt") := list(0, 60500)] # remove unfinished trials

d.f$cum_correct <- ave(d.f$percent, d.f$id, FUN=cumsum)
d.f <- d.f[, rt_sec := rt / 1000]
d.f <- as.data.table(d.f)

participants_params <- data.frame() # create table for model parameters

for (i in unique(d.f$id)) {

  participant_1 <- d.f[id == as.factor(i), ] # subset one participant
  # fit function to data
  fit <- nls(cum_correct ~ alpha*exp(b/rt_sec),data=participant_1,
             algorithm="port",
             start=c(alpha=0.9,b=-5),lower=c(alpha=0,b=-Inf), upper=c(alpha=1.5,b=0))
  summary(fit)
  participant_1$pred <- predict(fit)
  params <- as.character(fit$m$getPars())
  #get some estimation of goodness of fit
  cor <- cor(participant_1$cum_correct,predict(fit))
  participants_params <-  rbind(participants_params, data.frame(id = i, alpha = as.numeric(params[1]), beta = as.numeric(params[2]), cor = as.numeric(cor)))  
  
  p <- ggplot( participant_1, aes(rt_sec, cum_correct))
  print(p + geom_point() + coord_cartesian(xlim = c(0, 60), ylim = c(0, 1)) +
          ggtitle('Accumulated CB proportion correct at each time point') +
          xlab ('Cut-off point (sec)') + ylab ('% Correct') +
          geom_line(aes(x = rt_sec, y = pred), color = "red") +
          annotate("text", x=40, y=0.2, label= paste("alpha: ", round(as.numeric(params[1]), 2) ," ", "beta: ", round(as.numeric(params[2]), 2), " ", "cor: ", round(cor, 2)))
  )

}
  
all_summary <- merge(all_summary, participants_params)
all_summary[, mean_beta:=mean(beta), by=list(brms_quentile)]

d.f_deciles <- d.f[brms_quentile > 8 | brms_quentile < 3]
d.f_deciles <- arrange(d.f_deciles,brms_quentile,rt_sec)
d.f_deciles <- ddply(d.f_deciles, .(brms_quentile), transform, ecdf=ecdf(rt)(rt) )

d.f_deciles <- d.f_deciles[!is.na(d.f_deciles$success),] # remove unfinished trials


p <- ggplot( d.f_deciles, aes(rt_sec, ecdf, group = brms_quentile, colour = factor(brms_quentile))) +
        geom_line() + scale_color_brewer(type = "div", palette = "RdBu", name = "bRMS decile", labels = c("1 - fastest NPS", "2", "9", "10 - slowest NPS")) + 
        coord_cartesian(xlim = c(0, 60), ylim = c(0, 1)) +
        ggtitle('Accumulated CB proportion correct at each time point') +
        xlab ('Cut-off point (sec)') + ylab ('% Correct') 

        
p

# Check accuracy at different time points ----

d.f_10s <- arrange(only_animation,brms_rt,rt)
d.f_10s <- data.table(d.f_10s)
d.f_10s[rt > 10000, success := FALSE]
cb_acc_10s <- d.f_10s[,.(cb_acc_10s = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
cb_acc_10s <- aggregate(cb_acc_10s[,c("cb_acc_10s")], by = list(id = cb_acc_10s$id), FUN = mean)

d.f_20s <- arrange(only_animation,brms_rt,rt)
d.f_20s <- data.table(d.f_20s)
d.f_20s[rt > 20000, success := FALSE]
cb_acc_20s <- d.f_20s[,.(cb_acc_20s = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
cb_acc_20s <- aggregate(cb_acc_20s[,c("cb_acc_20s")], by = list(id = cb_acc_20s$id), FUN = mean)

d.f_30s <- arrange(only_animation,brms_rt,rt)
d.f_30s <- data.table(d.f_30s)
d.f_30s[rt > 30000, success := FALSE]
cb_acc_30s <- d.f_30s[,.(cb_acc_30s = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
cb_acc_30s <- aggregate(cb_acc_30s[,c("cb_acc_30s")], by = list(id = cb_acc_30s$id), FUN = mean)

d.f_40s <- arrange(only_animation,brms_rt,rt)
d.f_40s <- data.table(d.f_40s)
d.f_40s[rt > 40000, success := FALSE]
cb_acc_40s <- d.f_40s[,.(cb_acc_40s = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
cb_acc_40s <- aggregate(cb_acc_40s[,c("cb_acc_40s")], by = list(id = cb_acc_40s$id), FUN = mean)

d.f_50s <- arrange(only_animation,brms_rt,rt)
d.f_50s <- data.table(d.f_50s)
d.f_50s[rt > 50000, success := FALSE]
cb_acc_50s <- d.f_50s[,.(cb_acc_50s = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
cb_acc_50s <- aggregate(cb_acc_50s[,c("cb_acc_50s")], by = list(id = cb_acc_50s$id), FUN = mean)

cb_acc <- merge(cb_acc, cb_acc_10s) %>%
  merge(cb_acc_20s) %>%
  merge(cb_acc_30s) %>%
  merge(cb_acc_40s) %>%
  merge(cb_acc_50s)

all_summary <- merge(cb_acc, all_summary)
cor_matrix <- all_summary[,c(2:9, 12:15)]
setnames(cor_matrix, 'cb_acc', "cb_acc_60s")

library(psych)
library(corrplot)
cor_results <- corr.test(cor_matrix, adjust = "none")
corrplot(cor_results$r, p.mat = cor_results$p, insig = "blank", method = "color",addCoef.col = "black", tl.col = "black", order = "alphabet", sig.level = .05)

# Make a CB accumalated acc plot based on bRMS groups (with SD whiskers) ----
d.f_i <- arrange(only_animation,brms_rt,rt)
d.f_i <- data.table(d.f_i)

num_participants <- length(unique(d.f_i$id))

datalist = list() 
for (i in 1:60) {    #### *** rbind not working inside for loop ******
  d.f_i <- arrange(only_animation,brms_rt,rt)
  d.f_i <- data.table(d.f_i)
  d.f_i[rt > i*1000, success := FALSE]
  cb_acc <- d.f_i[,.(cb_acc = sum(success == TRUE, na.rm = TRUE)/N), by = id] # returns cb correct rate
  cb_acc <- aggregate(cb_acc[,c("cb_acc")], by = list(id = cb_acc$id), FUN = mean)
  cb_acc$time_sec <- rep(c(i),times=num_participants)
  df <- data.frame(cb_acc)
  #df_total <- rbind(df_total,df)
  datalist[[i]] <- cb_acc
}

big_data <- data.table::rbindlist(datalist)
big_data <- merge(big_data, all_summary[,c("id", "brms_quentile")])
big_data[brms_quentile > 9, rank := "worst"]
big_data[brms_quentile < 2, rank := "best"]
big_data[brms_quentile > 1 & brms_quentile < 10, rank := "medium"]
big_data <- big_data[rank != "medium"]
big_data <- data.frame(big_data)

#+++++++++++++++++++++++++
# Function to calculate the mean and the standard deviation
# for each group
#+++++++++++++++++++++++++
# data : a data frame
# varname : the name of a column containing the variable
#to be summariezed
# groupnames : vector of column names to be used as
# grouping variables
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

big_data <- big_data[,c(2:3,5)]
big_data$time_sec <- as.factor(big_data$time_sec)

library(dplyr)
df.summary <- big_data %>%
  group_by(time_sec, rank) %>%
  summarise(
    sd = sd(cb_acc, na.rm = TRUE),
    mean = mean(cb_acc)
  )
df.summary

p<- ggplot(df.summary, aes(x=time_sec, y=mean, group=rank, color=rank)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.5,
                position=position_dodge(0.4))
p + labs(title="Tooth length per dose", x="Time (sec)", y = "% correct")+
  theme_classic()



# Check correlations ----

cor.test(all_summary$alpha, all_summary$beta)
ggplot(all_summary, aes(x = alpha, y = beta)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$brms_quentile, all_summary$mean_beta)
ggplot(all_summary, aes(x = brms_quentile, y = mean_beta)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$brms_quentile, all_summary$beta)
ggplot(all_summary, aes(x = brms_quentile, y = beta)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$brms_rt, all_summary$beta)
ggplot(all_summary, aes(x = brms_rt, y = beta)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$brms_rt, all_summary$alpha)
ggplot(all_summary, aes(x = brms_rt, y = alpha)) +
  geom_point() +
  geom_smooth(method='lm') +
  annotate("text", x=15000, y=0.95, label = "cor= - 0.35.   p < 0.001 ")
pcor.test(x=all_summary$alpha, y=all_summary$brms_rt, z=all_summary$distance)

model <- lm(brms_rt ~ cb_rt_with_unfinished + cb_acc_20s + distance, data = all_summary)
summary(model)
  
cor.test(all_summary$brms_quentile, all_summary$alpha)
ggplot(all_summary, aes(x = brms_quentile, y = alpha)) +
  geom_point() +
  geom_smooth(method='lm') +
  annotate("text", x=3, y=0.9, label = "cor= - 0.43.   p < 0.01 ")

cor.test(all_summary$cb_rt_with_unfinished, all_summary$beta)
ggplot(all_summary, aes(x = cb_rt_with_unfinished, y = beta)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$cb_rt, all_summary$beta)
ggplot(all_summary, aes(x = cb_rt, y = beta)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$cb_rt, all_summary$alpha)
ggplot(all_summary, aes(x = cb_rt, y = alpha)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary$cb_rt_with_unfinished, all_summary$alpha)
ggplot(all_summary, aes(x = cb_rt_with_unfinished, y = alpha)) +
  geom_point() +
  geom_smooth(method='lm')


# Check correlation of BRMS and CB (without unfinished) between participants
cor.test(all_summary$brms_rt, all_summary$cb_rt)
ggplot(all_summary, aes(x = brms_rt, y = cb_rt)) +
  geom_point() +
  geom_smooth(method='lm')

# Check correlation of BRMS and CB (with unfinished trials) between participants
cor.test(all_summary$brms_rt, all_summary$cb_rt_with_unfinished)
ggplot(all_summary, aes(x = brms_rt, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm') +
  annotate("text", x=15000, y=3000, label = "r = 0.27.   p = 0.006 ")

pcor.test(x=all_summary$cb_rt_with_unfinished, y=all_summary$brms_rt, z=all_summary$distance)

cor.test(all_summary$brms_quentile, all_summary$cb_rt_with_unfinished)
ggplot(all_summary, aes(x = brms_quentile, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm') +
  annotate("text", x=3, y=3000, label = "r = 0.25.   p = 0.01 ")



# Check correlation of BRMS and CB between deciles ----
all_summary <- data.table(all_summary)
cor.test(all_summary[brms_rt > 20000]$brms_rt, all_summary[brms_rt > 20000]$cb_rt_with_unfinished)
ggplot(all_summary[brms_rt > 20000], aes(x = brms_rt, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary[brms_quentile > 8 | brms_quentile < 3]$brms_rt, all_summary[brms_quentile > 8 | brms_quentile < 3]$cb_rt_with_unfinished)
ggplot(all_summary[brms_quentile > 8 | brms_quentile < 3], aes(x = brms_rt, y = cb_rt_with_unfinished)) +
  geom_point() +
  geom_smooth(method='lm')

cor.test(all_summary[brms_quentile > 8 | brms_quentile < 3]$brms_rt, all_summary[brms_quentile > 8 | brms_quentile < 3]$beta)
ggplot(all_summary[brms_quentile > 8 | brms_quentile < 3], aes(x = brms_rt, y = beta)) +
  geom_point() +
  geom_smooth(method='lm')



# print example inverse exponantial curves ----
# First curve is plotted
cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=100))

curve(1*exp(-2/x), 0, 60, col = cc[1])
curve(1*exp(-4/x), 0, 60, col = cc[20], add = T)
curve(1*exp(-6/x), 0, 60, col = cc[40], add = T)
curve(1*exp(-8/x), 0, 60, col = cc[60], add = T)
curve(1*exp(-10/x), 0, 60, col = cc[80], add = T)
curve(1*exp(-12/x), 0, 60, col = cc[100], add = T)
legend(40, 0.6, legend=c("?? = -2", "?? = -4", "?? = -6", "?? = -8", "?? = -10", "?? = -12"),
       col=c(cc[1], cc[20], cc[40], cc[60], cc[80], cc[100]), lty=1, cex=0.8)

curve(1.5*exp(-5/x), 0, 60, col = cc[1])
curve(1.4*exp(-5/x), 0, 60, col = cc[20], add = T)
curve(1.3*exp(-5/x), 0, 60, col = cc[40], add = T)
curve(1.2*exp(-5/x), 0, 60, col = cc[60], add = T)
curve(1.1*exp(-5/x), 0, 60, col = cc[80], add = T)
curve(1*exp(-5/x), 0, 60, col = cc[100], add = T)
abline(h = 1, col="black", lwd=1, lty=2)
legend(40, 0.65, legend=c("?? = 1.5", "?? = 1.4", "?? = 1.3", "?? = 1.2", "?? = 1.1", "?? = 1"),
       col=c(cc[1], cc[20], cc[40], cc[60], cc[80], cc[100]), lty=1, cex=0.8)



# Extract questionnaires ----

only_quest <- dt[trial_type == 'survey-text' | trial_type == 'survey-likert' | trial_type == 'survey-multi-choice']
only_quest <- only_quest[id %in% all_summary$id]
only_quest$responses <- gsub(':",', ':9,', only_quest$responses)
only_quest$responses <- gsub('\"\"', '\"', only_quest$responses)
only_quest$responses <- gsub(':\"}', ':\"\"}', only_quest$responses)

# keep only relevant columns
only_quest <- data.frame(only_quest)
homogenous = apply(only_quest, 2, function(var) length(unique(var)) == 1) #tag columns without any variance
only_quest <- only_quest[, !homogenous] # remove them

# extract values
only_quest <- data.table(only_quest)


dems <- only_quest[, .(gender = fromJSON(responses[internal_node_id == "0.0-11.0-1.0"])$'gender',
                     hand = fromJSON(responses[internal_node_id == "0.0-11.0-1.0"])$'hand',
                     native = fromJSON(responses[internal_node_id == "0.0-11.0-1.0"])$'native',
                     age = as.numeric(fromJSON(responses[internal_node_id == "0.0-11.0-2.0"])$'age'),
                     attention = fromJSON(responses[internal_node_id == "0.0-11.0-3.0"])$'add',
                     q1 = fromJSON(responses[internal_node_id == "0.0-11.0-9.0"])$'q1',
                     q2 = fromJSON(responses[internal_node_id == "0.0-11.0-9.0"])$'q2',
                     q3 = fromJSON(responses[internal_node_id == "0.0-11.0-9.0"])$'q3',
                     q4 = fromJSON(responses[internal_node_id == "0.0-11.0-9.0"])$'q4',
                     q5 = fromJSON(responses[internal_node_id == "0.0-11.0-10.0"])$'q5',
                     q6 = fromJSON(responses[internal_node_id == "0.0-11.0-10.0"])$'q6',
                     q7 = fromJSON(responses[internal_node_id == "0.0-11.0-10.0"])$'q7',
                     q8 = fromJSON(responses[internal_node_id == "0.0-11.0-10.0"])$'q8',
                     q9 = fromJSON(responses[internal_node_id == "0.0-11.0-11.0"])$'q9',
                     q10 = fromJSON(responses[internal_node_id == "0.0-11.0-11.0"])$'q10',
                     q11 = fromJSON(responses[internal_node_id == "0.0-11.0-11.0"])$'q11',
                     q12 = fromJSON(responses[internal_node_id == "0.0-11.0-11.0"])$'q12',
                     driver = fromJSON(responses[internal_node_id == "0.0-11.0-12.0"])$'is_driver',
                     driving_abilities = responses[internal_node_id == "0.0-12.0-0.0"],
                     driver_accidents = responses[internal_node_id == "0.0-12.0-1.0"],
                     pedestrian_accidents = fromJSON(responses[internal_node_id == "0.0-13.0-0.0"])$'Q0'
                     ),
                     by = .(id)]

dems[!(is.na(driving_abilities)), c('driving_ability', 'driving_accidents') := 
       list(as.numeric(fromJSON(driving_abilities)), as.numeric(fromJSON(driver_accidents))), 
     by = id]
dems[, c("driving_abilities","driver_accidents"):=NULL]

# remove 9s now
is.na(dems) <- dems == 9

dems$total_hsp <- rowMeans(dems[,7:18], na.rm = T, dims = 1)

# merge dems with exp data
all_summary <- merge(dems, all_summary)
all_summary <- all_summary[,pedestrian_accidents := as.numeric(pedestrian_accidents)] # make numeric
all_summary[pedestrian_accidents > 10, pedestrian_accidents := NA] # remove people who said more then 10 pedestrian accidents

# dems histograms ----
# HSP
h1 = hist(all_summary$total_hsp+1, breaks = c(seq(from = 1, to = 7, by = 1)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h1$density = h1$counts/sum(h1$counts)*100

# Age
h2 = hist(all_summary$age, breaks = c(seq(from = 10, to = 70, by = 5)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h2$density = h2$counts/sum(h$counts)*100

# Driver accidents
h3 = hist(all_summary$driving_accidents, breaks = c(seq(from = -1, to = 11, by = 1)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h3$density = h3$counts/sum(h3$counts)*100

# Driver ability
h4 = hist(all_summary$driving_ability+1, breaks = c(seq(from = 1, to = 7, by = 1)), plot = F) # or hist(x,plot=FALSE) to avoid the plot of the histogram
h4$density = h4$counts/sum(h4$counts)*100

par(mfrow=c(2,2)) # make 2X2 plot 
plot(h1, main = "HSP Score", freq = F)
plot(h2, main = "Age", freq = F)
plot(h3, main = "Car Accidents", freq = F)
plot(h4, main = "Reported Driving Abilities", freq = F)
par(mfrow=c(1,1)) # reset back to 1X1 plot

# make questionnaires and behavioral correlations matrix ----
cor_matrix <- all_summary[,c('age', 'total_hsp','driving_accidents','driving_ability','pedestrian_accidents', 'brms_rt', 'cb_rt_with_unfinished', 'cb_acc_20s', 'cb_acc_40s', 'alpha', 'beta')]
cor_results <- corr.test(cor_matrix, adjust = "none")
corrplot(cor_results$r, p.mat = cor_results$p, method = "color", tl.col = "black", addCoef.col = "black", insig = "blank")


# Look at CB stimuli ----

# Mean and SD of every stimulus
only_animation[is.na(success), rt := 60500]
stimuli <- only_animation[,list(stimulus_mean=mean(rt),sd=sd(rt)),by=stimulus]
stimuli$stimulus_rank <- NA
order.mean<-order(stimuli$stimulus_mean,stimuli$stimulus)
stimuli$stimulus_rank[order.mean] <- 1:nrow(stimuli)

cor.test(stimuli$stimulus_mean, stimuli$sd)
ggplot(stimuli, aes(x = stimulus_mean, y = sd)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Correlation between stimulus mean and SD")+
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "left" )

# plots of stimuli rt by divided for 'best' and 'worse' NPS pps
temp <- merge(only_animation, stimuli, by = 'stimulus')
temp <- temp[brms_quentile > 8 | brms_quentile < 3]
temp <- temp[brms_quentile > 8, brms_group := 'worst']
temp <- temp[brms_quentile < 3, brms_group := 'best']
temp <- temp[stimulus_rank < 11 | stimulus_rank > 20,]
temp$stimulus_rank <- as.factor(temp$stimulus_rank)
temp$brms_group <- as.factor(temp$brms_group)

temp <- data.frame(temp)
homogenous = apply(temp, 2, function(var) length(unique(var)) == 1) #tag columns without any variance
temp <- temp[, !homogenous] # remove them
temp <- data.frame(temp)

p<-ggplot(temp, aes(reorder(stimulus,rt,na.rm = TRUE), y=rt, fill = brms_group)) +
  geom_boxplot()
  
p

require(ggpubr)
p <- ggboxplot(temp, x = "brms_group", y = "rt",
               color = "brms_group",
               add = "jitter",
               facet.by = "stimulus_rank", short.panel.labs = T)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format")


# Make Mean rt deciles
stimuli$stimuli_decile <- ntile(stimuli$stimulus_mean, 10)  
only_animation <- merge(only_animation, stimuli[, c("stimulus","stimulus_mean", "stimuli_decile", "stimulus_rank")], by="stimulus")

# Can CB abilities explain CB performance better in the "hard" stimuli vs the "easy" ones?

only_animation <- merge(only_animation, all_summary[, c("id","cb_rt_with_unfinished", "alpha", "beta", "cb_acc_20s", "cb_acc_40s", "total_hsp")], by="id")
easy_stimuli <- only_animation[stimulus_rank <= 10]
hard_stimuli <-  only_animation[stimulus_rank >= 21]

# CB rt
cor.test(easy_stimuli$cb_rt_with_unfinished, easy_stimuli$rt)
p1 <- ggplot(easy_stimuli, aes(x = cb_rt_with_unfinished, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Easy stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "left" )

cor.test(hard_stimuli$cb_rt_with_unfinished, hard_stimuli$rt)
p2 <- ggplot(hard_stimuli, aes(x = cb_rt_with_unfinished, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Hard stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "left" )

require(gridExtra)
require(ggpubr)
grid.arrange(p1, p2, ncol=1)

# bRMS NPS
cor.test(easy_stimuli$brms_rt, easy_stimuli$rt)
p3 <- ggplot(easy_stimuli, aes(x = brms_rt, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Easy stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

cor.test(hard_stimuli$brms_rt, hard_stimuli$rt)
p4 <- ggplot(hard_stimuli, aes(x = brms_rt, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Hard stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

grid.arrange(p3, p4, ncol=1)

# HSP
cor.test(easy_stimuli$total_hsp, easy_stimuli$rt)
p5 <- ggplot(easy_stimuli, aes(x = total_hsp, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Easy stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

cor.test(hard_stimuli$total_hsp, hard_stimuli$rt)
p6 <- ggplot(hard_stimuli, aes(x = total_hsp, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Hard stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

grid.arrange(p5, p6, ncol=1)

# CB acc
cor.test(easy_stimuli$cb_acc_20s, easy_stimuli$rt)
p7 <- ggplot(easy_stimuli, aes(x = cb_acc_20s, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Easy stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

cor.test(hard_stimuli$cb_acc_20s, hard_stimuli$rt)
p8 <- ggplot(hard_stimuli, aes(x = cb_acc_20s, y = rt)) +
  geom_point() +
  geom_smooth(method='lm') +
  ggtitle("Hard stimuli") +
  stat_cor(method="pearson", color = 'red', label.y.npc="top", label.x.npc = "center" )

grid.arrange(p7, p8, ncol=1)


stimuli$stimulus <- sapply(stimuli$stimulus, function(x) paste0("'", x))
write.csv(stimuli,"C:/Users/yuval/Desktop/lab/Thesis/yuvalharr.github.io//stimuli.csv", row.names = FALSE)


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



