library(tidyquant)
library(tidyverse)

msft <- tq_get("MSFT", get="stock.prices", from="2020-01-01",to="2021-03-15")
summary(msft)
ggplot(data=msft, aes(x=date, y=close))+geom_line()+theme_tq()

FANG <- c("FB", "AMZN", "NFLX", "GOOG") %>%
  tq_get(get="stock.prices", from = "2015-01-01", to="2021-03-30")
end <-ymd("2021-03-01")
start <-end-weeks(40)

n <- 20

FANG %>%
  ggplot(aes(x=date, y=close, group = symbol))+
  geom_candlestick(aes(open=open, close=close, high=high, low=low))+
  geom_bbands(aes(high=high, low=low, close=close),
              ma_fun=SMA, n=20, sd=2, size=.5)+
  labs(title = "FANG Stocks",
        subtitle="Four stocks at once", x="Date", y="Closing price")+
  coord_x_date(xlim=c(start,end))+
  facet_wrap(~symbol,ncol=2, scales="free_y")+
  theme_tq()