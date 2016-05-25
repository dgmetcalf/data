library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(bootstrap)
library(ROCR)
library(Hmisc)
library(VGAM)
library(rms)
library(stargazer)
library(ggplot2)
library(coefplot)
install.packages("AER")
library(AER)

rm(list=ls())

ms <- read.table ( "msrepl87.asc" ,header=TRUE, 
                   colClasses=c("character" , rep("numeric",22)))
rownames(ms) <- ms$country

ms$logdeaths1 <- log(ms$deaths75+1)
ms$logdeaths10 <- log(ms$deaths75+10)
ms$logdeaths100 <- log(ms$deaths75+100)
ms$logdeaths1000 <- log(ms$deaths75+1000)
max(ms$deaths75)

ols1 <- lm(logdeaths1 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms)
ols10 <- lm(logdeaths10 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms)
ols100 <- lm(logdeaths100 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms)
ols1000 <- lm(logdeaths1000 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms)

stargazer(ols1,ols10,ols100,ols1000)

ms2<-ms
ms2$sanctions75 <- ms2$sanctions75/sd(ms2$sanctions75)
ms2$polrights75 <- ms2$polrights75/sd(ms2$polrights75)
ms2$pop75 <- ms2$pop75/sd(ms2$pop75)
ms2$giniland <- ms2$giniland/sd(ms2$giniland,na.rm=T)
ms2$aglabor70 <- ms2$aglabor70/sd(ms2$aglabor70)

ols1.2 <- lm(logdeaths1 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms2)
ols10.2 <- lm(logdeaths10 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms2)
ols100.2 <- lm(logdeaths100 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms2)
ols1000.2 <- lm(logdeaths1000 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, data=ms2)

pdf("PS6coefplot.pdf",width=5,height=7)
multiplot(ols1.2,ols10.2,ols100.2,ols1000.2, names=c("+1","+10","+100","+1000"),intercept=F)
dev.off()

#part B

pois <- glm(deaths75 ~ sanctions75 + polrights75 + 
              pop75 + giniland + aglabor70, family="poisson", data=ms)
dispersiontest(pois,trafo=1)
pois2 <- glm(deaths75 ~ sanctions75 + polrights75 + 
              pop75 + giniland + aglabor70, family="poisson", data=subset(ms, deaths75<60000))
dispersiontest(pois2,trafo=1)
summary(pois)
max(ms$deaths75[ms$deaths75<60000])

negbin <- glm.nb(deaths75 ~ sanctions75 + polrights75 + pop75 + giniland + aglabor70, 
                     data = ms, control=glm.control(maxit=1000))
summary(negbin)
stargazer(pois,negbin)