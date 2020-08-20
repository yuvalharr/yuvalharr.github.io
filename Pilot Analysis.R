library(data.table)
library(ggplot2)
theme_set(theme_bw())
library(rjson)
library(ez)
library(plyr)
library(Hmisc)

setwd ("C:/Users/yuval/Desktop")
dt <- fread(paste('pilot_data.csv', sep= ''))

boxplot(V1~V2,dt, main="Pilot Data",
        xlab="Participant", ylab="RT")

ggplot(dt, aes(x=V2, y=V1)) + 
  geom_boxplot(outlier.shape=NA) + #avoid plotting outliers twice
  geom_jitter(position=position_jitter(width=.1, height=0))

# Compute the analysis of variance
res.aov <- aov(V1 ~ V2, data = dt)
# Summary of the analysis
summary(res.aov)
