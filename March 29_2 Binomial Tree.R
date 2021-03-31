library(fOptions)
library(xlsx)

tree<-BinomialTreeOption(TypeFlag="ca", S=50, X = 55,
                         Time=4, r=0.01, b=0.00, sigma = 0.2, n = 4)
t <- BinomialTreePlot(tree, cex=.8,
                  xlab = "period", ylab = "Option Value", digits=3)
title(main="Option Tree")
print(tree)
write.xlsx(tree, file="binomial_tree.xlsx")
