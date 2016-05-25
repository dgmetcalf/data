rm(list=ls())

library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
install.packages("arm")
library(arm)
install.packages("bootstrap")
library(bootstrap)
install.packages("ROCR")
library(ROCR)
install.packages("Hmisc")
library(Hmisc)

ms <- read.table ( "msrepl87.asc" ,header=TRUE, 
                   colClasses=c("character" , rep("numeric",22)))
rownames(ms) <- ms$country

attach(ms)
ms$sanctions <- (sanctions70+sanctions75)/2
ms$deaths <- c(0)
ms$deaths[deaths75 > 0] <- c(1)

x1 <- as.matrix(cbind(ms$sanctions))
x2 <- as.matrix(cbind(ms$sanctions, ms$aglabor, ms$pop75))
y <- as.matrix(cbind(ms$deaths))

#Using glm() got me the perfect separation error message
#i.e "fitted probabilities numerically 1 or 0"
#Sarah reccommended trying firth regression

install.packages("logistf")
library(logistf)

logistf.1<-logistf(y~x1, family=binomial(link="logit"), data=ms)
logistf.2<-logistf(y~x2, family=binomial(link="logit"), data=ms)

#Rock that ROC Plot
pred1 <- prediction(logistf.1$predict, y)
pred2 <- prediction(logistf.2$predict, y)
roc1<-performance(pred1, "tpr", "fpr")
roc2<-performance(pred2, "tpr","fpr")
plot(roc1, col="red")
plot(roc2, add=TRUE, col="blue")
abline(0,1,lty=3)

beta.fit1<-function(x1,y){
  logistf.1<-logistf(y~x1, family=binomial(link="logit"), data=ms)
}
beta.predict1<-function(logistf.1, x1){
  inv.logit(cbind(1,x1)%*%coef(logistf.1))
}
beta.fit2<-function(x2,y){
  logistf.1<-logistf(y~x2, family=binomial(link="logit"), data=ms)
}
beta.predict2<-function(logistf.2, x2){
  inv.logit(cbind(1,x2)%*%coef(logistf.2))
}
crossval1<-crossval(x1,y,beta.fit1, beta.predict1,ngroup=10)
crossval2<-crossval(x2,y,beta.fit2, beta.predict2,ngroup=10)
pred3<-prediction(crossval1$cv.fit, y)
pred4<-prediction(crossval2$cv.fit, y)
roc3<-performance(pred3, "tpr", "fpr")
roc4<-performance(pred4, "tpr","fpr")
plot(roc3, col="red")
plot(roc4, add=TRUE, col="blue")
abline(0,1,lty=3)

performance(pred3,measure="auc")
performance(pred4,measure="auc")

beta.tilde<-mvrnorm(10000,coef(logistf.2),vcov(logistf.2))
sanctions.values <- seq(from=min(ms$sanctions),to=max(ms$sanctions),by=5)
x.matrix <- as.matrix(cbind(1,sanctions.values,mean(ms$aglabor),mean(ms$pop75)))
pred.prob<-inv.logit(beta.tilde %*% t(x.matrix))
pred.prob<-apply(pred.prob, 2, quantile, c(0.025,0.975))

plot(sanctions.values, pred.prob[1,], col="white", xlim=c(min(ms$sanctions), max(ms$sanctions)), 
     ylim=c(-.05,1.05),
     xlab = "Range of Sanctions", 
     ylab = "Predicted Probability of Deaths > 0",
     lwd=3, main = "Relationship between Sanctions and Deaths")

scat1d(ms$sanctions[ms$deaths==0], side=1, add=T, col="black",lwd=1,frac=.075)
scat1d(ms$sanctions[ms$deaths==1], side=3, add=T, col="black",lwd=1,frac=.075)
abline(1,0, lwd=.05)
abline(0,0, lwd=.05)

segments(x0=sanctions.values, y0=pred.prob[1,], 
         x1=sanctions.values, y1=pred.prob[2,], 
         col="cornflowerblue", lwd=3)

scen.values <- c(quantile(ms$sanctions, c(0, .25, .5, .75, 1)),mean(ms$sanctions))
scen.x.matrix <- as.matrix(cbind(1,scen.values,mean(ms$aglabor),mean(ms$pop75)))
scen.pred.prob<-inv.logit(beta.tilde %*% t(scen.x.matrix))
library(vioplot)
vioplot(scen.pred.prob[,1],scen.pred.prob[,2],
        scen.pred.prob[,3],scen.pred.prob[,6],
        scen.pred.prob[,4],scen.pred.prob[,5], col="cornflowerblue", 
        names=c("Min","1st Quartile","Median","Mean", "3rd Quartile","Max"),
        border="white", rectCol="white",colMed="black")
