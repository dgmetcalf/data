#MLE Problem Set 2
#Danny Metcalf
#9/23/2014
rm(list=ls())


wdi<-na.omit(data.frame(read.csv("wdi2010.csv",header=TRUE)))
library(foreign) #to read in data in a variety of formats
library(xtable) #to easily generate LaTeX-formatted results

# set up matrices for simple implementation
x <-cbind(1,as.matrix(log(wdi$gdp.pc.ppp))) #note the column of 1s
y <- as.matrix(log(wdi$co2.kt))
# number of cases (n) and variables (K)
K <- ncol(x) ; n <- nrow(x)

# define likelihood function
loglik.my <- function(par,X,Y) {
  Y <- as.vector(y)
  X <- as.matrix(x)
  xbeta <- X%*%par[1:K]
  sigma <- sqrt(1/(n-K)*sum((y-xbeta)^2))
  sum(-(1/2)*log(2*pi)-(1/2)*log(sigma^2)-(1/(2*sigma^2))*(y-xbeta)^2)
}

mle.fit <- optim(par=c(5,5), fn=loglik.my, method = "BFGS",
                 control = list(trace,maxit=10000,fnscale = -1),hessian = T)
if(mle.fit$convergence!=0)
  print(" WARNING: Convergence Problems; Try different starting values.")
mle.fit

stderrors<- sqrt(diag(-solve(mle.fit$hessian)))
z<-mle.fit$par/stderrors
p.z <- qnorm(0.975)* (1 - pnorm(abs(z)))
out.table <- cbind(mle.fit$par,stderrors,z,p.z)
out.table


plot(log(co2.kt)~log(gdp.pc.ppp),data=wdi, main="MLE Regression")
abline(a=mle.fit$par[1],b=mle.fit$par[2])

#Checking my results against standard ols regression
check1 <- lm(log(wdi$co2.kt)~log(wdi$gdp.pc.ppp))
check2 <- glm(log(wdi$co2.kt)~log(wdi$gdp.pc.ppp))
summary(check1)
summary(check2)

#plotting likelihood
lik.plotting<-function(beta0,beta1){
  Y <- as.vector(y)
  X <- as.matrix(x)
  likelihood <- cbind()
  for (i in beta1){
  xbeta <- beta0*X[,1] + i*X[,2]
  sigma <- sqrt(1/(n-K)*sum((y-xbeta)^2))
  likelihood<- cbind(likelihood,sum(-(1/2)*log(2*pi)-(1/2)*log(sigma^2)-(1/(2*sigma^2))*(y-xbeta)^2))
}
plot(beta1,likelihood, type="lines")
}

beta1.holder <- seq(mle.fit$par[2]-stderrors[2], mle.fit$par[2]+stderrors[2], .01)
beta0.holder <- mle.fit$par[1]

lik.plotting(mle.fit$par[1],beta1.holder)

aic <- -2*mle.fit$value+2*2
bic <- -2*mle.fit$value+2*log(128)

aic.check1 <- -2*logLik(check1)+2*2
aic.check2 <- -2*logLik(check2)+2*2
bic.check1 <- -2*logLik(check1)+2*log(128)
bic.check2 <- -2*logLik(check2)+2*log(128)

columnheads <- c("MLE", "lm()", "glm()")
aiclist <- c(aic,aic.check1,aic.check2)
biclist <- c(bic,bic.check1,bic.check2)
out.table2 <- cbind(columnheads,aiclist,biclist)
print(xtable(out.table2,floating=FALSE))

restricted <- lm(log(wdi$co2.kt)~1)
summary(restricted)
logLik(restricted)
-2*logLik(restricted)+2*log(128)

LR <- -2*(logLik(restricted)-logLik(check1))
1-pchisq(LR[1],df=1)

