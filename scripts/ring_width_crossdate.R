#load packages
install.packages("wesanderson")
library(dplR)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library("signal")

data(ca533) # the result of ca533 <- read.rwl('ca533.rwl')
nrow(test)
ncol(test) # 34 series
colnames(test)
head(time(test),n = 2) # the first 10 years
class(ca533) 
plot(test, plot.type="spag")
rwl.report(test)
rwl.stats(test)
test.stats <- rwl.stats(test) # same as calling rwl.stats(ca533)
head(test.stats,n=5) # look at the first five series

boxplot(test.stats$ar1,ylab=expression(phi[1]),col = "lightblue")
stripchart(test.stats$ar1, vertical = TRUE,  
           method = "jitter", jitter = 0.02,add = TRUE, pch = 20, col = 'darkblue',cex=1.25)
ar1Quant <- quantile(test.stats$ar1,probs = c(0.25,0.5,0.75))
abline(h=ar1Quant,lty="dashed",col="grey")
mtext(text = names(ar1Quant),side = 4,at = ar1Quant,las=2)

ar1 <- data.frame(x="CA 533",y=test.stats$ar1)
ggplot(ar1,aes(x,y)) + geom_boxplot(width=.2) +
  geom_jitter(width=0.1) + 
  labs(y=expression(phi[1]),x=element_blank()) +
  theme_minimal()
find.package("ggplot2")
data(wa082)
plot(wa082, plot.type="spag")

wa082RWI <- detrend(wa082, method="AgeDepSpline")
wa082Crn <- chron(wa082RWI)
str(wa082Crn)

dplR::plot(wa082, add.spline=TRUE, nyrs=30)

plot(wa082Crn, add.spline=TRUE, nyrs=30)
