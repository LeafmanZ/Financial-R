library(edgar)
library(ggplot2)
library(ggthemes)
library(dplyr)

cik.no <- 1744489
filing.year <- c(2016,2017,2018,2019,2020)

df8 <- get8KItems(cik.no, filing.year)

num <-df8 %>% count(event.info, sort=TRUE)

ggplot(num, aes(x=event.info, y=n))+
  geom_bar(stat="identity", fill="darkblue")+
  coord_flip()+
  aes(stringr::str_wrap(event.info))

