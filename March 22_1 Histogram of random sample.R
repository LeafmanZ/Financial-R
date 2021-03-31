library(ggplot2)
library(ggthemes)

sample_size = 30
trials=100
m =.02
s=.005

sample_means <-0

for (i in 1:trials){
  samp <-rnorm(sample_size, mean=m, sd=s)
  sample_means[i] <- mean(samp)
}

print(sample_means)

hist(sample_means, col='lightblue', border = 'grey', breaks = 11, xlab ='sample means')

sm<-data.frame(sample_means)

ggplot(sm,aes(x=sample_means)) + 
  geom_histogram(fill = 'darkred', color = 'grey', bins = 11)
  theme_fivethirtyeight()