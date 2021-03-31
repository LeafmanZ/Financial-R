library(BatchGetSymbols)       # Get data from Yahoo! Finance
library(rvest)                 # Web scraping
library(dplyr)                 # Data manipulation
library(ggplot2)               # Plotting
library(ggthemes)              # Adding style
library(reshape2)              # Pivot data
library(tidyr)                 # Manipulate date (e.g., melt)
library(PerformanceAnalytics)  # Compounding (among other things)


first.date <- Sys.Date()-1200  # Set number of trading days to retrieve
last.date <- Sys.Date()        # Today's date for last date
freq.data <- 'daily'           # Frequency of data set to 'daily'

# stklist <- c('FB','KO','MSFT','AMZN') #Hard coding of tickers

########################### Query user ##########################

n <- readline("How many stocks would you like to see? ")
n <- as.numeric(n)
stklist =c()   #Create empty array for tickers

for (i in seq(n)) {
  stklist <- c(stklist,readline(prompt="Input stock symbol: "))
}

########################## Retrieve data ########################

df <- BatchGetSymbols(stklist,
                      first.date=first.date,
                      last.date=last.date,
                      freq.data = freq.data,
                      do.cache=FALSE)

######################### Plot data ############################

ggplot(df$df.tickers,aes(x=ref.date,y=price.close))+
  geom_line()+
  facet_wrap(~ticker,scales="free_y")+
  theme_classic()

ggplot(df$df.tickers,aes(x=ref.date,y=volume/1000,color=ticker,fill=ticker))+
  geom_col() + facet_wrap(~ticker,scales='free_y')+
  ggtitle("Volume over time")+
  theme_classic()

####################### Create time series of Value of $1 #####

df2 <- reshape.wide(df$df.tickers)    # Pivot
df3 <- df2$ret.closing.prices+1       # Get returns based on closing prices
date <- df3$ref.date                  # Store date into array
df3 <- subset(df3,select=-ref.date)
df3[is.na(df3)] <- 1 
df4 <- cumprod(df3)
df4$date <- date

m <- melt(df4,id.vars="date",variable.name="Ticker") #"un"pivot (i.e., stack) data

ggplot(m,aes(x=date,y=value,color=Ticker,group=Ticker))+
  geom_line()+
  facet_wrap(~Ticker,scales="free_y")+
  ggtitle("Value of $1")+
  theme_classic()

