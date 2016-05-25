#MLE Problem Set 1
#Danny Metcalf
#9/16/2014

rm(list=ls())

#Write an R program that will replicate Figure 1.2 from the Ward & Ahlquist manuscript.
#Create a matrix of nine such histograms which illustrate the effect of changing the size
#of the sampled groups ??? {10, 100, 1000} and the effect of changing the number of random
#samples ??? {10, 100, 1000}. Put the horizontal axis on the same scale for all plots

#naming convention: vectorx.y indicates a vector of means of y draws with sample size x
#Note: after doing this, I decided that this was nowhere near as intuitive as I hoped
#I will not use naming conventions like this again in the future

sample1 <- rnorm(10000,mean=10, sd=1)
vector10.1000 <- c()
for(i in 1:1000){
  vector10.1000 <- c(vector1, mean(sample(sample1,10)))
  }
hist(vector10.1000, breaks=20, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE)
abline(a=NULL, b=NULL, v=mean(vector10.1000), col="red")

vector10.10 <- c()
for(i in 1:10){
  vector10.10 <- c(vector10.10, mean(sample(sample1,10)))
}

vector10.100 <- c()
for(i in 1:100){
  vector10.100 <- c(vector10.100, mean(sample(sample1,10)))
}

vector100.10 <- c()
for(i in 1:10){
  vector100.10 <- c(vector100.10, mean(sample(sample1,100)))
}

vector100.100 <- c()
for(i in 1:100){
  vector100.100 <- c(vector100.100, mean(sample(sample1,100)))
}

vector100.1000 <- c()
for(i in 1:1000){
  vector100.1000 <- c(vector100.1000, mean(sample(sample1,100)))
}

vector1000.10 <- c()
for(i in 1:10){
  vector1000.10 <- c(vector1000.10, mean(sample(sample1,1000)))
}

vector1000.100 <- c()
for(i in 1:100){
  vector1000.100 <- c(vector1000.100, mean(sample(sample1,1000)))
}

vector1000.1000 <- c()
for(i in 1:1000){
  vector1000.1000 <- c(vector1000.1000, mean(sample(sample1,1000)))
}

par(mfrow=c(3,3))
par(oma=c(3,1,2,2))
par(mar=c(2,2,2,2))
hist(vector10.10, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE, xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector10.10), col="red")
hist(vector10.100, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector10.100), col="red")
hist(vector10.1000, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector10.1000), col="red")
hist(vector100.10, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector100.10), col="red")
hist(vector100.100, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector100.100), col="red")
hist(vector100.1000, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector100.1000), col="red")
hist(vector1000.10, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector1000.10), col="red")
hist(vector1000.100, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector1000.100), col="red")
hist(vector1000.1000, freq=FALSE, col="slategrey", yaxt='n', ann=FALSE,xlim=(9.3:10.7))
abline(a=NULL, b=NULL, v=mean(vector1000.1000), col="red")
title("Problem 3: Small Multiples", outer= TRUE)

#Problem 4 a
#Generate a missingness map for the Muller-Seligson data. See missmap in the Amelia 
#library.

getwd()
data <- read.table("msrepl87.asc", header=TRUE)
install.packages("Amelia")
library(Amelia)
par(mfrow=c(2,2))
missmap(data)

#Problem 4 b
#Provide a kernel density plot of log deaths75). Add a constant to avoid taking the log
#of zero

data$log.deaths75 <- log(data$deaths75+1)
dens <- density(data$log.deaths75)
plot(dens, main="Deaths '75")

#4 c
#Provide a pair of boxplots for sanctions70 and sanctions75

X <- as.data.frame(cbind(data$sanctions70,data$sanctions75))
colnames(X) <- c("Deaths '70", "Deaths '70")
boxplot(X, boxwex=0.25, horizontal=TRUE)

#4 d
#Provide a series of violin plots for giniland, aglabor70, and upper20. Transform the
#scale of the giniland variable for more appropriate plotting

install.packages("vioplot")
library(sm)
library(vioplot)
Y <- as.data.frame(cbind(data$giniland,data$aglabor,data$upper20))
vioplot(na.omit(data$giniland*80),na.omit(data$aglabor),na.omit(data$upper20),names = c("Gini x 80", "Ag Labor", "Upper 20"))

#5

wdi<-read.csv("wdi2010.csv")
Z<-as.data.frame(cbind(wdi$co2.kt,wdi$gdp.pc.ppp))
Z<-na.omit(Z)
Z$V1<-log(Z$V1)
Z$V2<-log(Z$V2)
attach(Z)
plot(V2,V1,pch=16,col="slategrey", xlab = "2010 GDP per capita at PPP, log $US", ylab = "2010 CO2 emissions, log Ktons", axes=FALSE)
abline(lm(V1~V2))
axis(1, at=6:11)
axis(2, at=0:16)

plot(V2,V1,pch=16,col="slategrey", xlab = "2010 GDP per capita at PPP, log $US", ylab = "2010 CO2 emissions, log Ktons", axes=FALSE)
axis(1, at=6:11)
axis(2, at=0:16)
fit<-loess(V1 ~ V2, span=.001)
aa <- 0:10000
bb <- predict(fit, aa)
lines(aa,bb, col="black")

detach(Z)
        
#6
library(foreign)
cox<-read.dta("coxappend.dta")
attach(cox)
x<-cbind(1,eneth,log(ml),eneth*log(ml))
xprime <- t(x)
xprimex <- xprime%*%x
xprimex.inv <- solve(xprimex)
y <- cbind(enps)
beta <- xprimex.inv%*%xprime%*%y
xb <- x%*%beta
resid <- y-xb
e<-y-x%*%beta
S2<-t(e)%*%e/50
varcovar<-sqrt(1.394888*xprimex.inv)
se<-diag(varcovar)
table <- cbind(beta,se)
        
#b
qqnorm(resid)

#c
test <- lm(enps~eneth+log(ml)+eneth*log(ml))
summary(test)

#d
plot.lm(test)