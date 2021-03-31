library(jrvFinance)
library(lubridate)

settle <- Sys.Date()

d<-readline("What is the maturity date? Report as mm-dd-yyyy: ")
c<-readline("What is the coupon rate? ")
y<-readline("What is the yield? ")
f<-readline("What is the frequency of coupons in a year? ")

mature <- as.Date(d, format='%m-%d-%Y')

coupon <- as.numeric(c)

if (coupon > 1) {coupon <- coupon / 100}

yield <- as.numeric(y)

if (yield > 1) {yield <- yield /100}

freq <- as.numeric(f)

price <- bond.price(settle, mature, coupon, freq=2, yield,
                    convention = "30/360",
                    comp.freq = freq, redemption_value=100)
print(price)

