library(FinCal)
library(rgl)
library(magick)

r <- .05
pv <- 100
pmt <-0
type <-0
n <-1:25

future_value <-fv(r,n,-pv,pmt,type)
plot(n, future_value, main = "Future Value With Different Periods",
     xlab = "Number of periods", ylab = 'Future Value',
     col = 'darkblue', gb = 'blue', pch=20)

rates <-seq(.01,.2,.01)
x = function(rates,n) {fv(rates, n, -pv, pmt, type)}
z <-outer(rates,n,x)


persp(rates,n,z,xlab='interest rates',ylab='number of periods', zlab = 'FV')

nbcol=100
color = rev(rainbow(nbcol, start = 0/6, end = 4/6))
zcol = cut(z,nbcol)

persp3d(rates,n,z,xlab='interest rates',ylab='number of periods', zlab = 'FV',
        col = color[zcol],expand=.5,ticktype = 'detailed', axes=TRUE)
 
movie3d(spin3d(axis=c(0,0,1),rpm = 2),duration = 60, type = 'gif')