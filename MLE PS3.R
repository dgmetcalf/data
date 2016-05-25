library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)

rm(list=ls())

wdi<-na.omit(data.frame(read.csv("wdi2010.csv",header=TRUE)))
problem1 <- glm(log(wdi$co2.kt)~log(wdi$gdp.pc.ppp))
summary(problem1)

coefficients1<-coef(problem1)
var.covar.matrix1<-vcov(problem1)

beta.tilde1<-mvrnorm(10000,coefficients1,var.covar.matrix1)

GDP.values <- seq(from=quantile(wdi$gdp.pc.ppp, 1/4),to=quantile(wdi$gdp.pc.ppp, 3/4),by=100)

x.matrix1 <- cbind(1,GDP.values)

pred.values1 <- beta.tilde1%*%t(x.matrix1)
pred.values1<-apply(pred.values1, 2, quantile, c(0.025,0.975))

pdf("PS3P1.pdf",width=7,height=5)
plot(GDP.values, pred.values1[1,], col="white", xlim=c(quantile(wdi$gdp.pc.ppp, 1/4), quantile(wdi$gdp.pc.ppp, 3/4)), ylim=c(min(pred.values1[1,]), max(pred.values1[2,]))) 

segments(x0=GDP.values, y0=pred.values1[1,], 
         x1=GDP.values, y1=pred.values1[2,], 
         col="blue", lwd=1)
dev.off()

#problem 2
library(foreign)
labor.data <- read.dta("binlfp2.dta")
#recode binary data
x <- as.matrix(cbind(labor.data$age, labor.data$lwg, labor.data$inc))
y <- as.numeric(labor.data$lfp)-1
y[is.na(y)] = 0

#MLE Function with gradient function included
binreg<- function(X,y){ 
  X<- cbind(1,X)
  negLL<- function(b,X,y){
    p<-as.vector(1/(1+exp(-X %*% b)))
    - sum(y*log(p) + (1-y)*log(1-p))
  }
  logit.gr <- function(b,X,y) {
    p<-as.vector(1/(1+exp(-X %*% b))) 
      -apply(((y - p)*X) , 2,sum)
      
  }
  results<- optim (rep(0,ncol(X)),fn=negLL,gr=logit.gr,
                   hessian=T,method="BFGS",X=X,y=y)
  list(coefficients=results$par,varcovariance=solve(results$hessian),
       deviance=2*results$value,
       converged=results$convergence==0)
}

logit.mle.lfp <- binreg(x,y)

std.errors <- sqrt(diag(logit.mle.lfp$varcovariance))
out.table2 <- cbind(logit.mle.lfp$coefficients, std.errors)
library(xtable)
xtable(out.table2)

#same model as above using glm() 
logit.glm.lfp <- glm(lfp~age+lwg+inc, family = binomial(link = "logit"), data=labor.data)
summary(logit.glm.lfp)

xtable(logit.glm.lfp)

#making a Coefficient plot
install.packages("ggplot2")
library("ggplot2")
install.packages("coefplot")
library("coefplot")

pdf("PS3P2.pdf",width=7,height=5)
coefplot(logit.glm.lfp)
dev.off()

#Problem 3
impeach.data <- read.csv("impeach.csv")
attach(impeach.data)
impeach.data$vote <- 0
impeach.data$vote[impeach.data$votesum > 0] <- 1
impeach.data$vote[is.na(impeach.data$votesum)] <- 1
impeach.data$ccoal98[is.na(impeach.data$ccoal98)] <- mean(impeach.data$ccoal98, na.rm=TRUE)


glm.impeach <- glm(vote~partyid + clint96 + ccoal98, family = binomial(link = "logit"), data=impeach.data)
summary(glm.impeach)

coefficients3<-coef(glm.impeach)
var.covar.matrix3<-vcov(glm.impeach)
beta.tilde3<-mvrnorm(10000,coefficients3,var.covar.matrix3)

clint96.values <- seq(from=0,to=100,by=1)
dem.matrix3 <- cbind(1,0,clint96.values,mean(impeach.data$ccoal98, na.rm=TRUE))
rep.matrix3 <- cbind(1,1,clint96.values,mean(impeach.data$ccoal98, na.rm=TRUE))

pred.values.dem3 <- inv.logit(beta.tilde3%*%t(dem.matrix3))
pred.values.rep3 <- inv.logit(beta.tilde3%*%t(rep.matrix3))

pred.values.dem3<-apply(pred.values.dem3, 2, quantile, c(0.025,0.975))
pred.values.rep3<-apply(pred.values.rep3, 2, quantile, c(0.025,0.975))

par(mfrow=c(1,1))

pdf("PS3P3A.pdf",width=7,height=5)
plot(clint96.values, pred.values.dem3[1,], col="white", xlim=c(0, 100), ylim=c(0,1), xlab="Clinton's 1996 Vote Share",ylab="Probability of Voting for Impeachment")

segments(x0=clint96.values, y0=pred.values.dem3[1,], 
         x1=clint96.values, y1=pred.values.dem3[2,], 
         col="blue", lwd=2)

segments(x0=clint96.values+.5, y0=pred.values.rep3[1,], 
         x1=clint96.values+.5, y1=pred.values.rep3[2,], 
         col="red", lwd=2)
abline(v=max(impeach.data$clint96), col= "black", lwd=3)
abline(v=min(impeach.data$clint96), col="black", lwd=3)
dev.off()


mean(impeach.data$ccoal98, na.rm=TRUE)

install.packages("separationplot")
library(separationplot)
yhat<-predict(glm.impeach, type="response")
yhat.noideology<-predict(update(glm.impeach, .~. - ccoal98), type="response")
yhat.onlyparty <-predict(update(glm.impeach, .~. - ccoal98 - clint96), type="response")
pdf("PS3P3B.pdf",width=7,height=5)
par(mfrow=c(3,1))
separationplot(yhat,glm.impeach$y, heading="Separation plot of Voting for Impeachment",
               xlab="Including Conservative Coalition Scores",newplot=F,type="rect")
separationplot(yhat.noideology,glm.impeach$y, xlab="Only Party ID and Clinton Vote Share", newplot=F,type="rect")
separationplot(yhat.onlyparty,glm.impeach$y, xlab="Only Party ID", newplot=F,type="rect")
dev.off()