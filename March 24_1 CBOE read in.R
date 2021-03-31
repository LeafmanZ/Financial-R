library(lubridate)
library(pastecs)
library(DescTools)
library(dplyr)
library(ggplot2)
library(psych)

df <-read.csv(file.choose())

df$Date <- ymd(df$Date)

head(df)
tail(df)

cboe <- df[,-1]
head(cboe)

objects(cboe)
d1 <- summary(cboe) #base R

d2 <- stat.desc(cboe) #pastecs

d3 <- Desc(cboe) #DescTools

d4 <- describe(cboe) #psych

capture.output(d1,d2,d3,d4, file = "describe.doc")