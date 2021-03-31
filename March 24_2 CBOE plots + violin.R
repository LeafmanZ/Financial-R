library(aplpack)
library(vioplot)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(lubridate)

setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

df <-read.csv(file.choose())

df$Date <- ymd(df$Date)

colnames(df)

gl1<-ggplot(df, aes(x = Date)) + geom_line(aes(y=vix), color = 'turquoise') + ylim(0,80)
gl2<-ggplot(df, aes(x = Date)) + geom_line(aes(y=vxo), color = 'lightgreen') + ylim(0,80)
gl3<-ggplot(df, aes(x = Date)) + geom_line(aes(y=vxn), color = 'orchid1')  + ylim(0,80)
gl4<-ggplot(df, aes(x = Date)) + geom_line(aes(y=vxd), color = 'lightblue')  + ylim(0,80)
grid.arrange(gl1,gl2,gl3,gl4,nrow=2,top='CBOE volatility Indexes over time') + ylim(0,80)

#Density plots
ggplot(df) + geom_density(aes(x= vix),color = 'turquoise4', fill = 'turquoise', alpha = .2) + theme_classic()

#Histogram
hist(df$vix, main = 'Histogram Base R')

ggplot(df,aes(vix)) + geom_histogram(color = "turquoise4", fill = " turquoise", bins = 7)
labs(y = "Number of observations", x = "Bin")
ggtitle("Histogram using ggplot")+
theme_classic()

#Box plots
par(mfrow = c(1,2))
boxplot(df$vix,col='turquoise', main='VIX distribution', ylab = 'Index level')
boxplot(df$vxo,col='red', main='VXO distribution', ylab = 'Index level')
par(mfrow=c(1,1))

ggplot(df, aes(x="", y = vix)) + geom_boxplot(color = 'black', fill = 'turquoise', outlier.color='red') +
  labs(y = "Index level", x = '') +
  ggtitle("Box PLot ggplot") +
  stat_boxplot(geom="errorbar") + theme_classic()


vioplot(df$vix,df$vxo,df$vxn,df$vxd,
        col = c('turquoise', 'lightgreen', 'orchid1', 'lightblue'),
        names = c('VIX', 'VXO', 'VXN', 'VXD'))

ggplot(df, aes(x = '', y = vix)) +
  geom_violin(color='black',fill='turquoise')+
  geom_boxplot(width = .1) +
  stat_summary(fun=mean,geom='point',shape=23,size =3, fill = 'gold')+
  ggtitle('Violion using ggplot\nVIX', subtitle = 'interquartile range and mean indicator')

