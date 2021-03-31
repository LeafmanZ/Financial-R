library(edgar)
library(ggplot2)
library(ggthemes)
library(dplyr)

form.type <- "10-K"
cik.no <- 1185348

filing.year <- c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

df10 <- getSentiment(cik.no, form.type, filing.year)

df10$compl <- df10$complex.word.count/df10$word.count
df10$tone  <- df10$lm.positive.count/(df10$lm.negative.count+df10$lm.positive.count)

ggplot(df10)+
  geom_line(aes(x=date.filed, y=tone, color = "Tone"))+
  geom_line(aes(x=date.filed, y=compl, color = "Complexity"))+
  labs(colour="Sentiment measure")+
  ylim(0, NA)+
  theme_classic()