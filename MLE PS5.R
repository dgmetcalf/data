rm(list=ls())

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


cox <- read.dta("coxappend.dta")
attach(cox)
cox$logml <- log(ml)
ols <- lm(enps ~ eneth + logml + eneth*logml, data=cox)
summary(ols)
coef(ols)[3]
vcov(ols)

marg.tab <- cbind(c("1st Quartile","Median","3rd Quartile"),
                  c(coef(ols)[2]+coef(ols)[3]*quantile(cox$logml, c(.25,.5,.75))),
                  c(sqrt(vcov(ols)[2,2]+vcov(ols)[3,3]*quantile(cox$logml, c(.25,.5,.75))^2
                         +2*vcov(ols)[2,4]*quantile(cox$logml, c(.25,.5,.75))^2)))
stargazer(marg.tab)


#Problem 2
impeach <- read.csv("impeach.csv")

impeach$vote <- 0
impeach$vote[impeach$votesum > 0] <- 1
y <- as.matrix(cbind(impeach$vote))

impeach.fit <- glm(y~clint96+aflcio97+clint96*aflcio97,family=binomial(link="probit"), data=impeach)
summary(impeach.fit)

beta.tilde<-mvrnorm(10000,coef(impeach.fit),vcov(impeach.fit))
clint96.values <- seq(from=quantile(impeach$clint96,.25),to=quantile(impeach$clint96,.75),by=.5)

scen.low.afl <- cbind(1,clint96.values,0,0)
scen.high.afl <- cbind(1,clint96.values,100,100*clint96.values)
pred.low.afl<-inv.logit(beta.tilde %*% t(scen.low.afl))
pred.low.afl<-apply(pred.low.afl, 2, quantile, c(0.025,0.975))
pred.high.afl<-inv.logit(beta.tilde %*% t(scen.high.afl))
pred.high.afl<-apply(pred.high.afl, 2, quantile, c(0.025,0.975))

pdf("PS5predprob.pdf",width=7,height=5)
par(mfrow=c(1,1))
par(mar=c(4,4,3,3))
plot(clint96.values, pred.low.afl[1,], col="white", xlim=c(min(clint96.values), max(clint96.values)), 
     ylim=c(0,1),
     xlab = "Clinton '96 Share Interquartile Range", 
     ylab = "Predicted Probability of Voting for Impeachment",
     lwd=3, main = "Impeachment")
polygon(x=c(clint96.values,rev(clint96.values)),
        y=c(pred.low.afl[1,]-pred.high.afl[2,],
            rev(pred.low.afl[2,]-pred.high.afl[1,])),col=rgb(0,0,0,alpha=.1),border=rgb(0,0,0,alpha=.1))
segments(x0=clint96.values, y0=pred.low.afl[1,], 
         x1=clint96.values, y1=pred.low.afl[2,], 
         col="orange", lwd=3)
segments(x0=clint96.values, y0=pred.high.afl[1,], 
         x1=clint96.values, y1=pred.high.afl[2,], 
         col="black", lwd=3)
dev.off()

impeach.fit2 <- glm(y~clint96+aflcio97,family=binomial(link="probit"), data=impeach)
x.p1a <- as.matrix(cbind(impeach$clint96,impeach$aflcio97,impeach$clint96*impeach$aflcio97))
x.p1b <- as.matrix(cbind(impeach$clint96,impeach$aflcio97))

beta.fit1<-function(x.p1a,y){
  impeach.fit<-glm(y~x.p1a, family=binomial(link="probit"), data=impeach)
}
beta.predict1<-function(impeach.fit, x.p1a){
  probit(cbind(1,x.p1a)%*%coef(impeach.fit),inverse=T)
}
beta.fit2<-function(x.p1b,y){
  impeach.fit2<-glm(y~x.p1b, family=binomial(link="probit"), data=impeach)
}
beta.predict2<-function(impeach.fit2, x.p1b){
  probit(cbind(1,x.p1b)%*%coef(impeach.fit2),inverse=T)
}
crossval1<-crossval(x.p1a,y,beta.fit1, beta.predict1,ngroup=10)
crossval2<-crossval(x.p1b,y,beta.fit2, beta.predict2,ngroup=10)
pred3<-prediction(crossval1$cv.fit, y)
pred4<-prediction(crossval2$cv.fit, y)
roc3<-performance(pred3, "tpr", "fpr")
roc4<-performance(pred4, "tpr","fpr")

pred1 <- prediction(predict(impeach.fit), impeach$vote[is.na(impeach$aflcio97)==F])
pred2 <- prediction(predict(impeach.fit2), impeach$vote[is.na(impeach$aflcio97)==F])
roc1<-performance(pred1, "tpr", "fpr")
roc2<-performance(pred2, "tpr","fpr")

pdf("PS5roc.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot(roc1, col="orange", main="In Sample Predictions",lwd=5)
plot(roc2, add=TRUE, col="black",lwd=2)
abline(0,1,lty=2)
plot(roc3, col="orange", main="Cross Evaluations",lwd=5)
plot(roc4, add=TRUE, col="black",lwd=2)
abline(0,1,lty=2)
dev.off()

#ehhh close enough I can change it in LaTex
xtable(cbind(c(AIC(impeach.fit),AIC(impeach.fit2)),c(BIC(impeach.fit),BIC(impeach.fit2))))


#Problem 3
income<-ordered(c("Mid","High","Low"))
income
as.numeric(income)

#Problem 4
drury <- read.csv("drury_jpr_data.csv")

drury$result <- as.factor(drury$result)
ologit <- polr(result ~ log(gnprat) + trade + tarcst 
               + cost +coop, data=drury, Hess=T, method="logistic")
summary(ologit)
stargazer(ologit, title="Ordered Logit Regression of Result", align=TRUE, 
          dep.var.labels=c("Result"), covariate.labels=c("log(gnprat)", "trade", "tarcst", "cost", "coop"))


beta.p4 <- coef(ologit)
tau.p4 <- ologit$zeta
attach(drury)
trade.values<-seq(from=min(trade),to=max(trade),by=5)
x.p4 <- cbind(mean(log(gnprat)),trade.values,mean(tarcst), median(cost), median(coop))

pdf("PS5stackedologit.pdf",width=7,height=5)
par(mfrow=c(1,1))
plot(trade.values,plogis(tau.p4[1] - x.p4 %*% beta.p4),type="l",ylim=c(-.05,1),
     ylab=expression("Predicted Probabilities of Result  " <= "  M"),xlab="Range of Trade",col="white",main="Stacked Predicted Probabilities of Result (Ordered Logit)")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(plogis(tau.p4[1] - x.p4 %*% beta.p4),0),col="red",border='red')
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(plogis(tau.p4[2] - x.p4 %*% beta.p4),rev(plogis(tau.p4[1] - x.p4 %*% beta.p4))),col="blue",border="blue")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(plogis(tau.p4[3] - x.p4 %*% beta.p4), rev(plogis(tau.p4[2] - x.p4 %*% beta.p4))),col="green", border="green")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(1,rev(plogis(tau.p4[3] - x.p4 %*% beta.p4))),col="yellow", border="yellow")
scat1d(drury$trade[drury$result==1], side=1, add=T, col="red",lwd=1,frac=.075)
scat1d(drury$trade[drury$result==2], side=1, add=T, col="blue",lwd=1,frac=.075)
scat1d(drury$trade[drury$result==3], side=1, add=T, col="green",lwd=1,frac=.075)
scat1d(drury$trade[drury$result==4], side=1, add=T, col="yellow",lwd=1,frac=.075)
abline(0,0, lwd=.05)
legend(140,1,legend=c("Result=1","Result=2","Result=3","Result=4"),
       col=c("red","blue","green","yellow"),lty=1,lwd=5,cex=.7,bg="white")
dev.off()

mnlogit <- multinom(as.factor(result) ~ log(gnprat) + trade + tarcst 
               + cost +coop, data=drury, Hess=T, method="logistic")
summary(mnlogit)
stargazer(mnlogit, title="Ordered Logit Regression of Result", align=TRUE, 
          dep.var.labels=c("Result"), covariate.labels=c("log(gnprat)", "trade", "tarcst", "cost", "coop"))



x.p4b <-cbind(1,x.p4)
predmn2 <- exp(x.p4b%*%Coef.mn[1,])/
  (1+exp(x.p4b %*% Coef.mn[1,])+exp(x.p4b %*% Coef.mn[2,])+exp(x.p4b %*% Coef.mn[3,]))
predmn3 <- exp(x.p4b%*%Coef.mn[2,])/
  (1+exp(x.p4b %*% Coef.mn[1,])+exp(x.p4b %*% Coef.mn[2,])+exp(x.p4b %*% Coef.mn[3,]))
predmn4 <- exp(x.p4b%*%Coef.mn[3,])/
  (1+exp(x.p4b %*% Coef.mn[1,])+exp(x.p4b %*% Coef.mn[2,])+exp(x.p4b %*% Coef.mn[3,]))
predmn1 <- 1-predmn2-predmn3-predmn4


pdf("PS5stackedmnlogit.pdf",width=7,height=5)
par(mfrow=c(1,1))
plot(trade.values,plogis(tau.p4[1] - x.p4 %*% beta.p4),type="l",ylim=c(-.05,1),
     ylab=expression("Predicted Probabilities of Result  " <= "  M"),xlab="Range of Trade",col="white",main="Stacked Predicted Probabilities of Result (Multinomial Logit)")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(predmn1,0),col="red",border="red")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(predmn1,rev(predmn1+predmn2)),,col="blue",border="blue")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(predmn1+predmn2,rev(predmn1+predmn2+predmn3)),col="green",border="green")
polygon(x=cbind(trade.values,rev(trade.values)),
        y=cbind(predmn1+predmn2+predmn3,rev(predmn1+predmn2+predmn3+predmn4)),col="yellow",border="yellow")
scat1d(drury$trade[drury$result==1], side=1, add=T, col="red",lwd=1,frac=.075)
scat1d(drury$trade[drury$result==2], side=1, add=T, col="blue",lwd=1,frac=.075)
scat1d(drury$trade[drury$result==3], side=1, add=T, col="green",lwd=1,frac=.075)
scat1d(drury$trade[drury$result==4], side=1, add=T, col="yellow",lwd=1,frac=.075)
abline(0,0, lwd=.05)
legend(140,1,legend=c("Result=1","Result=2","Result=3","Result=4"),
       col=c("red","blue","green","yellow"),lty=1,lwd=5,cex=.7,bg="white")
dev.off()



lines(trade.values,predmn1)
lines(trade.values,predmn2)
lines(trade.values,predmn3)
lines(trade.values,predmn4)

Coef.mn<-coef(mnlogit)
varcovar.mn<-vcov(mnlogit)
SEmn<-matrix(sqrt(diag(varcovar.mn)), nrow=2, byrow=TRUE)

ncut<-3 #num categories in dv minus 1
k<-5 #number of coefficients not including the intercept

pdf("PS5somethingelse.pdf",width=7,height=5)
par(mfrow=c(2,2))
for(i in 2:k){
  plot(1:ncut, Coef.mn[,i], type="b", lty=2, xlab="Result - 1", 
       ylab=colnames(Coef.mn)[i],ylim=c((min(Coef.mn[,i])-2*mean(SEmn[,i])),
      (max(Coef.mn[,i])+2*mean(SEmn[,i]))),)
  abline(h=0, lty=1, lwd=.75, col=grey(.5))
}
dev.off()


model.ord<-vglm(ordered(drury$result)~ log(gnprat) + trade + tarcst + cost +coop, family=propodds)

model.multinom<-vglm(drury$result~ log(gnprat) + trade + tarcst + cost +coop, data=snails, family=multinomial)

test<-pchisq(deviance(model.ord)-deviance(model.multinom), df=df.residual(model.ord)-df.residual(model.multinom), lower.tail=FALSE)
test

xtable(summary(ologit)$coefficients,digits=2)


