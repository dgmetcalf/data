rm(list=ls())
library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(arm)
library(bootstrap)
library(ROCR)
library(Hmisc)
library(multilevel)
library(lme4)
library(stargazer)


CCES.2006 <- read.dta("cces2006old.dta")
CCES.2009 <- read.dta("cces2009.dta")
GINI <- read.csv("StateGini.csv")

attach(CCES.2006)

sub <- CCES.2006[which(V4040<101 & V2089!="Don't know"& V2089!="Different Party"
                       & V2089!="No vote" & V2089!="Skipped" & V2089!="Not asked"),]
sub <-as.matrix(sub)
sub <-as.data.frame(sub)

summary(V4040[which(V4040<998)])
lm(V2089[which(V4040<998)]~V4040[which(V4040<998)])

sub$genbal[sub$V2089=="Definitely Democratic"] <- "aDefDem"
sub$genbal[sub$V2089=="Probably Democratic"] <- "bProbDem"
sub$genbal[sub$V2089=="Probably Republican"] <- "cProbRep"
sub$genbal[sub$V2089=="Definitely Republican"] <- "dDefRep"
table(sub$genbal)

sub$black <- 0
sub$black[sub$V2005=="Black or African American"] <- 1
sub$age <- 2009-as.numeric(sub$V2020)
sub$female <- 0
sub$female[sub$V2004=="Female"] <-1

sub$abort <- NA
sub$abort[sub$V3019 == "By law, abortion should never be permitted"] <-3
sub$abort[sub$V3019 == "The law should permit abortion only in case of rape, incest"] <-2
sub$abort[sub$V3019 == "The law should permit abortion for reasons other than rape,"] <-1
sub$abort[sub$V3019 == "By law, a woman should always be able to obtain an abortion"] <-0

sub$gay <-NA
sub$gay[sub$V2103 == "Strongly support"] <-3
sub$gay[sub$V2103 == "Somewhat support"] <-2
sub$gay[sub$V2103 == "Somewhat oppose"] <-1
sub$gay[sub$V2103 == "Strongly oppose"] <-0

sub$social <- sub$gay+sub$abort
sub$interact <- sub$social*as.numeric(sub$V4040)

#ordered logit regression of budget policy on generic congressional ballot
ologit.genbal.budget <- polr(as.factor(genbal) ~ as.numeric(V4040) + social +black + age +female, data=sub,Hess=T, method="logistic")
ologit.genbal.interact <- polr(as.factor(genbal) ~ as.numeric(V4040) + social + interact + black + age +female, data=sub,Hess=T, method="logistic")

#shitty version
ologit <-polr(as.factor(genbal) ~ as.numeric(V4040), data=sub,Hess=T, method="logistic")

summary(ologit.genbal.budget)
#plotting pred probs
beta06 <- coef(ologit.genbal.budget)
zeta06 <- ologit.genbal.budget$zeta
budget.values<-seq(from=1,to=100,by=1)
social.values <-seq(from=0,to=6,by=1)
x06 <- cbind(budget.values, 2,mean(sub$black),mean(sub$age),0)
xsoc06 <- cbind(72, social.values,mean(sub$black),mean(sub$age),0)

#par(mfrow=c(1,1))
#plot(budget.values,plogis(zeta06[1] - x06 %*% beta06),type="l",ylim=c(-.05,1),
#     ylab=expression("Predicted Probabilities of Generic Ballot Voting"),
#     xlab="Range of Budget Preferences",col="black",main="Predicted Probabilities")
#lines(budget.values,dlogis(zeta06[2] - x %*% beta2))
#lines(budget.values,dlogis(zeta06[3] - x %*% beta2))
#lines(budget.values, dlogis(x %*% beta2))

pdf("821PaperPredProbs.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot(budget.values,plogis(zeta06[1] - x06 %*% beta06),type="l",ylim=c(-.05,1),
     ylab=expression("Cumulative Probabilities of Generic Ballot Voting"),
     xlab="Range of Budget Preferences",col="black")
polygon(x=cbind(budget.values,rev(budget.values)),
        y=cbind(plogis(zeta06[1] - x06 %*% beta06),0),col="deepskyblue3",border='deepskyblue3')
polygon(x=cbind(budget.values,rev(budget.values)),
        y=cbind(plogis(zeta06[2] - x06 %*% beta06),rev(plogis(zeta06[1] - x06 %*% beta06))),col="lightblue",border="lightblue")
polygon(x=cbind(budget.values,rev(budget.values)),
        y=cbind(plogis(zeta06[3] - x06 %*% beta06), rev(plogis(zeta06[2] - x06 %*% beta06))),col="salmon", border="salmon")
polygon(x=cbind(budget.values,rev(budget.values)),
        y=cbind(1,rev(plogis(zeta06[3] - x06 %*% beta06))),col="firebrick", border="firebrick")
segments(x0=c(46,86),x1=c(46,86),y0=c(0,0),y1=c(plogis(zeta06[2] - 
                  x06[46,] %*% beta06),plogis(zeta06[2] - x06[86,] %*% beta06)),lwd=3)
plot(social.values,plogis(zeta06[1] - xsoc06 %*% beta06),type="l",ylim=c(-.05,1),
     ylab=" ",
     xlab="Range of Social Preferences",col="black")
polygon(x=cbind(social.values,rev(social.values)),
        y=cbind(plogis(zeta06[1] - xsoc06 %*% beta06),0),col="deepskyblue3",border='deepskyblue3')
polygon(x=cbind(social.values,rev(social.values)),
        y=cbind(plogis(zeta06[2] - xsoc06 %*% beta06),rev(plogis(zeta06[1] - xsoc06 %*% beta06))),col="lightblue",border="lightblue")
polygon(x=cbind(social.values,rev(social.values)),
        y=cbind(plogis(zeta06[3] - xsoc06 %*% beta06),rev(plogis(zeta06[2] - xsoc06 %*% beta06))),col="salmon", border="salmon")
polygon(x=cbind(social.values,rev(social.values)),
        y=cbind(1,rev(plogis(zeta06[3] - xsoc06 %*% beta06))),col="firebrick", border="firebrick")
segments(x0=c(0,5),x1=c(0,5),y0=c(0,0),y1=c(plogis(zeta06[2] - 
          xsoc06[1,] %*% beta06),plogis(zeta06[2] - xsoc06[6,] %*% beta06)),lwd=3)
dev.off()

#WOOOOOO!


plot(sub$V4040, sub$V2089)

subsub <- sub[sub$State!="HI",]
subsub <- subsub[subsub$State!="RI",]
subsub <- subsub[subsub$State!="VT",]
subsub <- subsub[subsub$State!="NH",]
subsub <- subsub[subsub$State!="ND",]
subsub <- subsub[subsub$State!="SD",]
subsub <- subsub[subsub$State!="WY",]
GINIsub <-GINI[GINI$State!="HI",]
GINIsub <-GINIsub[GINIsub$State!="RI",]
GINIsub <-GINIsub[GINIsub$State!="VT",]
GINIsub <-GINIsub[GINIsub$State!="NH",]
GINIsub <-GINIsub[GINIsub$State!="ND",]
GINIsub <-GINIsub[GINIsub$State!="SD",]
GINIsub <-GINIsub[GINIsub$State!="WY",]


#New regression for each state, coefficients saved in vector coefs
rm(i)
coefs <- c()
std.errors <- c()
regressions <- c()
prob.diff <- c()
coefs.soc <- c()
std.errors.soc <- c()
prob.diff.soc <- c()
for (i in GINIsub$State) {
  state.ologit.genbal.budget <- polr(as.factor(genbal) ~ as.numeric(V4040) +social+ black + age+female, data=sub[V1002==i,],Hess=T, method="logistic")
  regressions <- cbind(regressions, state.ologit.genbal.budget)
  coefs <- c(coefs, coef(state.ologit.genbal.budget)[1])
  coefs.soc <- c(coefs.soc, coef(state.ologit.genbal.budget)[2])
  std.errors <- c(std.errors, sqrt(vcov(state.ologit.genbal.budget)[1,1]))
  std.errors.soc <- c(std.errors.soc, sqrt(vcov(state.ologit.genbal.budget)[2,2]))
  beta <- coef(state.ologit.genbal.budget)
  zeta <- state.ologit.genbal.budget$zeta
  prob.diff <- c(prob.diff, plogis(zeta[2] - x06[46,] %*% beta) - plogis(zeta[2] - x06[86,] %*% beta))
  prob.diff.soc <- c(prob.diff.soc, plogis(zeta[2] - xsoc06[1,] %*% beta) - plogis(zeta[2] - xsoc06[6,] %*% beta))
}

GINIsub$coefs <- coefs
GINIsub$se06 <-std.errors
plot(GINI$GINI,coefs)
plot(GINIsub$GINI,prob.diff)
summary(lm(prob.diff~GINIsub$GINI))
#Inequality is negatively related to how much each state cares about budget policy... for some reason

hist(as.numeric(sub2009$cc09_56))
attach(CCES.2009)
sub2009 <- CCES.2009[which(cc09_56!="don't know" & cc09_56!="NA" & cc09_56!="(other)" & as.numeric(cc423) != 4 & is.na(v201) & cc423!="not sure" & cc423!="skipped" & cc423!="not asked"),]
sub2009 <-as.matrix(sub2009)
sub2009 <-as.data.frame(sub2009)
ologit.pid.budget09 <- polr(as.factor(cc423) ~ as.numeric(cc09_56), data=sub2009,Hess=T, method="logistic")
summary(ologit.pid.budget09)

coefs09 <- c()
std.errors09 <-c()
for (i in GINI$state) {
  state.ologit.pid.budget09 <- polr(as.factor(cc423) ~ as.numeric(cc09_56), data=sub2009[v259==i,],Hess=T, method="logistic", na.action=na.omit)
  coefs09 <- c(coefs09, coef(state.ologit.pid.budget09))
  std.errors09 <- c(std.errors09, sqrt(vcov(state.ologit.pid.budget09)[1,1]))
}
GINI$coefs09 <- coefs09
GINI$se09 <-std.errors09
state <-c(GINI$State)

pdf("StateGINIplot09.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot(GINI$GINI,coefs09, xlab="GINI",ylab="Regression Coefficient",ylim=c(min(GINI$coefs09-GINI$se09),max(GINI$coefs09+GINI$se09)), col="white")
text(GINI$GINI,coefs09, labels=GINI$State, cex=.75)
abline(lm(coefs09~GINI$GINI), col="red")
abline(0,0,lty=2)
plot(GINI$GINI,coefs09, xlab="GINI",ylab="Regression Coefficient",ylim=c(min(GINI$coefs09-GINI$se09),max(GINI$coefs09+GINI$se09)), col="white")
segments(x0=GINI$GINI,x1=GINI$GINI, y0=GINI$coefs09-GINI$se09, y1=GINI$coefs09+GINI$se09)
abline(0,0,lty=2)
dev.off()


pdf("StateGINIbyPredProb.pdf",width=8,height=4)
par(mfrow=c(1,2))
par(mar=c(5,5.5,1,1))
plot(GINIsub$GINI,prob.diff,xlab="GINI",ylab="Change in Probability of Dem Vote \n Over IQR of Budget Preferences",ylim=c(min(prob.diff-.02),max(prob.diff+.02)), col="white")
text(GINIsub$GINI,prob.diff, labels=GINIsub$State, cex=.75)
abline(lm(prob.diff~GINIsub$GINI), col="red")
title(sub="p=.0598")

plot(GINIsub$GINI,prob.diff.soc,xlab="GINI",ylab="Change in Probability of Dem Vote \n Over IQR of Cultural Preferences",ylim=c(min(prob.diff.soc-.02),max(prob.diff.soc+.02)), col="white")
text(GINIsub$GINI,prob.diff.soc, labels=GINIsub$State, cex=.75)
title(sub="p=.436")
dev.off()

pdf("BudgVsSoc.pdf",width=5,height=5)
par(mfrow=c(1,1))
par(mar=c(5,6,1,1))
plot(prob.diff,prob.diff.soc,xlab="Change in Probability of Dem Vote \n Over IQR of Budget Preferences",
     ylab="Change in Probability of Dem Vote \n Over IQR of Cultural Preferences",sub="P=.0216", ylim=c(min(prob.diff.soc-.02),max(prob.diff.soc+.02)), col="white")
text(prob.diff,prob.diff.soc, labels=GINIsub$State, cex=.75)
abline(lm(prob.diff.soc~prob.diff), col="red")
dev.off()


aggbudget <-aggregate(as.numeric(sub$V4040), by=list(sub$State), 
                    FUN=mean, na.rm=TRUE)
summary(lm(aggbudget$x~GINI$GINI))

stargazer(ologit.genbal.budget, ord.intercepts=T)
bud.ols.full <-lm(prob.diff~GINIsub$GINI + GINIsub$Tax.burden +GINIsub$Poverty.rate)
bud.ols.red <- lm(prob.diff~GINIsub$GINI)
bud.ols.inv <-lm(prob.diff~GINIsub$Tax.burden +GINIsub$Poverty.rate)

soc.ols.full <-lm(prob.diff.soc~GINIsub$GINI + GINIsub$Tax.burden +GINIsub$Poverty.rate)
soc.ols.red <-lm(prob.diff.soc~GINIsub$GINI)
stargazer(bud.ols.red,bud.ols.full,soc.ols.red,soc.ols.full)


GINI$Percent.inc <- GINI$Income.Taxes/GINI$Total.Taxes
GINI$Tax.burden <- GINI$Total.Taxes/(GINI$GDP.state*1000)

sub$State <- sub$V1002
merged.sub <- merge(subsub,GINI,by=c("State"))

sub2009$state <- sub2009$v259
merged.sub2009 <- merge(sub2009,GINI,by=c("state"))


write.dta(merged.sub, "CCES2006GiniMerged.dta")
write.dta(merged.sub2009, "CCES2009GiniMerged.dta")

getwd()
setwd("C:/Users/Danny Metcalf/Documents")
