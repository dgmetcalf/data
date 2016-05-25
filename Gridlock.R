library(reshape)
library(foreign) 
library(xtable)  
library(MASS) 
library(boot)
library(graphics)
library(bootstrap)
library(ROCR)
library(Hmisc)
library(multilevel)
library(lme4)
library(stargazer)

rm(list=ls())

nom <- read.dta("SenDWNom113redo.dta")
jcs <- read.dta("JCS medians.dta")
jud <- read.dta("CtAppWide10-17-2015.dta")
jud <- jud[c(1:45,88)]
#Judiciary Committee
committee <- read.dta("judiciaryCommNom.dta")

ENP <- read.dta("ENPsencode1.dta")
RJO <- read.dta("RJOsencode1.dta")
DM <- read.dta("DMsencode1.dta")

DM <- rename(DM, c(v10="notes"))
ENP <- rename(ENP, c(icpsrscoresource="notes"))
RJO <- rename(RJO, c(imputedfrom="notes"))
DM$icpsr<-NA
RJO$icpsr<-NA
RJO <- RJO[c(1:10,12:14)]

senators <- rbind(ENP,RJO,DM)
write.csv(senators, file="allsenate.csv")

newjcs <- read.dta("V2_mergedjudgesold.dta")

#Hand imputed cases without Nom scores in STATA
senators <- read.dta("allsen1.dta")

jcs<-rename(jcs,c(Year="year"))
judin <- merge(jud,jcs,by="year")
fix(judin)

#Millet Patricia Anne in 113th congress not 112
#presidential ideology scores in our data set are incomplete. I will use voteview numbers
#from House scores, because they have pres scores for our whole period
#just in case the old numbers mean something else I'll create a new variable
#presparty is exactly what it sounds like
#there are 13 judges who don't have a president entered, I will hand edit them
#If it means anything, all 13 were DC judges missing some other data
fix(judin)
#after these hand edits, I want to save this as a distinct data file
write.csv(judin,file="judinhandedit.csv")
judin <- read.csv("judinhandedit.csv")

judin$presnom <-NA
judin$presparty <-NA
judin$presnom[judin$pres=="FDR"] <- -.505
judin$presparty[judin$pres=="FDR"] <- 100
judin$presnom[judin$pres=="HST"] <- -.276
judin$presparty[judin$pres=="HST"] <- 100
judin$presnom[judin$pres=="DDE"] <- .127
judin$presparty[judin$pres=="DDE"] <- 200
judin$presnom[judin$pres=="JFK"] <- -.63
judin$presparty[judin$pres=="JFK"] <- 100
judin$presnom[judin$pres=="LBJ"] <- -.521
judin$presparty[judin$pres=="LBJ"] <- 100
judin$presnom[judin$pres=="RMN"] <- .461
judin$presparty[judin$pres=="RMN"] <- 200
judin$presnom[judin$pres=="GF"] <- .468
judin$presparty[judin$pres=="GF"] <- 200
judin$presnom[judin$pres=="JC"] <- -.729
judin$presparty[judin$pres=="JC"] <- 100
judin$presnom[judin$pres=="RWR"] <- .738
judin$presparty[judin$pres=="RWR"] <- 200
judin$presnom[judin$pres=="GHWB"] <- .649
judin$presparty[judin$pres=="GHWB"] <- 200
judin$presnom[judin$pres=="WJC"] <- -.48
judin$presparty[judin$pres=="WJC"] <- 100
judin$presnom[judin$pres=="GWB"] <- .981
judin$presparty[judin$pres=="GWB"] <- 200
judin$presnom[judin$pres=="BHO"] <- -.378
judin$presparty[judin$pres=="BHO"] <- 100


x<- cbind(c())
y<- cbind(c(),c())
judgrid <- array(list(), dim=c(795, 17))

for (i in 1:length(judin$nomdate)){
  x <- senators[senators$cong == judin$congress.x[i],]
  x <- x[judin$nomdatenum[i] >= x$datebeginnum & judin$nomdatenum[i] <= x$dateendnum,]
  x <- x[is.na(x$name)==F,]
  z <- x[judin$statenm[i] == x$statenm,]
  if (nrow(x[x$party==100,])>nrow(x[x$party==200,])){
    judgrid[i,6] <- 100
  }
  judgrid[i,1] <- judin$name[i]
  judgrid[i,2] <- judin$year[i]
  judgrid[i,3] <- list(x$name)
  judgrid[i,4] <- list(x$idno)
  judgrid[i,5] <- list(x$dwnom1)
  if (nrow(x[x$party==100,])>nrow(x[x$party==200,])){
    judgrid[i,6] <- 100
  } else if (nrow(x[x$party==100,])<nrow(x[x$party==200,])) {
    judgrid[i,6] <- 200
  } else {
    judgrid[i,6] <- 200
  }
  #I handchecked. All the ties in our dataset happened under Republican Presidents
  #Meaning they're the de facto majority party
  judgrid[i,7] <- list(x[x$party==100,]$dwnom1)
  judgrid[i,8] <- list(x[x$party==200,]$dwnom1)
  if (length(z$name)!=0) {
  judgrid[i,16] <- z[1,]$dwnom1
  judgrid[i,17] <- z[2,]$dwnom1
  } else {
  judgrid[i,16] <- NA
  judgrid[i,17] <- NA
  }
  y <- cbind(c(y[,1],length(x$name)),c(y[,2],judin$name[i]),c(y[,3],judin$congress.x[i]),c(y[,4],length(z$name)))
}
#the above loop gives you 795 lists of Senators, their id numbers and their 1st dim score
#some 1st dim scores were hand imputed to "senators" in STATA
#Below gives the 40th and 60th Sens, the difference, the 33rd, 67th and median Sens
#and then puts them back into the main dataset

a<- cbind(c())
b<- cbind(c(),c())
judjcs <- array(list(), dim=c(795, 6))


for (i in 1:length(judin$nomdate)){
  a <- newjcs[newjcs$circuit == judin$circuit[i],]
  a <- a[judin$nomdatenum[i] >= a$commdatenum & judin$nomdatenum[i] <= a$retiredatenum,]
  a <- a[is.na(a$lastname)==F,]
  judjcs[i,1] <- judin$name[i]
  judjcs[i,2] <- judin$year[i]
  judjcs[i,3] <- list(a$lastname)
  judjcs[i,4] <- list(a$firstname)
  judjcs[i,5] <- list(a$JCS)
  b <- cbind(c(b[,1],length(a$lastname)),c(b[,2],judin$name[i]),c(b[,3],judin$year[i]))
}

plot(judjcs[,2], b[,1], xlab="year", ylab="number of judges")

judjcs[,6]<- lapply(judjcs[,5],quantile,probs= c(.5),  na.rm = TRUE)
judin$jcsmedian <- judjcs[,6]
#above generates lists of judges on the relevant court at the time of nomination
#that is, the status quo arrangement of the court



#Judiciary committee officially appointed late Jan-late Feb, but probably known early Jan
#After running this once I'll put it as a comment to not accidentally rerun
#committee$DateofAppointment <- committee$DateofAppointment - 51
#THIS IS A TEMPORARY FIX
d<- cbind(c())
e<- cbind(c(),c())
judcomm <- array(list(), dim=c(795, 5))

for (i in 1:length(judin$nomdate)){
  d <- committee[committee$cong == judin$congress.x[i],]
  d <- d[judin$nomdatenum[i] >= d$DateofAppointment & judin$nomdatenum[i] <= d$DateofTermination,]
  d <- d[is.na(d$name)==F,]
  judcomm[i,1] <- judin$name[i]
  judcomm[i,2] <- judin$year[i]
  judcomm[i,3] <- list(d$name)
  judcomm[i,4] <- list(d$dwnom1)
  e <- cbind(c(e[,1],length(d$name)),c(e[,2],judin$name[i]),c(e[,3],judin$year[i]))
}
#Above gives Judiciary Committee members at time of nomination
judcomm[,5]<- lapply(judcomm[,4],quantile,probs= c(0.5),  na.rm = TRUE)


judgrid[,9]<- lapply(judgrid[,5],quantile,probs= c(0.4),  na.rm = TRUE)
judgrid[,10]<- lapply(judgrid[,5],quantile,probs= c(0.6),  na.rm = TRUE)
judgrid[,11]<- as.numeric(judgrid[,10])-as.numeric(judgrid[,9])
judgrid[,12]<- lapply(judgrid[,5],quantile,probs= c(0.33),  na.rm = TRUE)
judgrid[,13]<- lapply(judgrid[,5],quantile,probs= c(0.67),  na.rm = TRUE)
judgrid[,14]<- lapply(judgrid[,5],quantile,probs= c(0.5),  na.rm = TRUE)
judgrid[,15]<- lapply(judgrid[,7],quantile,probs= c(0.5),  na.rm = TRUE)
for (i in 1:length(judgrid[,15])){
  if (judgrid[i,6]==100){
    judgrid[i,15]<- lapply(judgrid[i,7],quantile,probs= c(0.5),  na.rm = TRUE)
  } else {
    judgrid[i,15]<- lapply(judgrid[i,8],quantile,probs= c(0.5),  na.rm = TRUE)
  }
}


judin$gridlock4060 <- judgrid[,11]
judin$gridlock4060<-as.numeric(judin$gridlock4060)
judin$gridlock40 <- judgrid[,9]
judin$gridlock40<-as.numeric(judin$gridlock40)
judin$gridlock60 <- judgrid[,10]
judin$gridlock60<-as.numeric(judin$gridlock60)
judin$gridlock33 <- judgrid[,12]
judin$gridlock33<-as.numeric(judin$gridlock33)
judin$gridlock67 <- judgrid[,13]
judin$gridlock67<-as.numeric(judin$gridlock67)
judin$gridlockMed <- judgrid[,14]
judin$gridlockMed<-as.numeric(judin$gridlockMed)
judin$gridlockBS1 <- judgrid[,16]
judin$gridlockBS1<-as.numeric(judin$gridlockBS1)
judin$gridlockBS2 <- judgrid[,17]
judin$gridlockBS2<-as.numeric(judin$gridlockBS2)
judin$gridlock3367 <- as.numeric(judin$gridlock67) - as.numeric(judin$gridlock33)
judin$gridlock3367<-as.numeric(judin$gridlock3367)
judin$majParty <- as.numeric(judgrid[,6])
judin$gridlockMajMed <- as.numeric(judgrid[,15])
judin$gridlockComm <- as.numeric(judcomm[,5])

judin$gridlockLowFili <- 0
judin$gridlockLowFili[judin$nomdatenum < 5492] <- judin$gridlock33[judin$nomdatenum < 5492]
judin$gridlockLowFili[judin$nomdatenum >= 5492] <- judin$gridlock40[judin$nomdatenum >= 5492]
judin$gridlockHighFili <- 0
judin$gridlockHighFili[judin$nomdatenum < 5492] <- judin$gridlock67[judin$nomdatenum < 5492]
judin$gridlockHighFili[judin$nomdatenum >= 5492] <- judin$gridlock60[judin$nomdatenum >= 5492]

#I have all but committees so now I'm going to make windows for each to replicate PBM
#Window names match PBM see Table 1
#After that I'll test if JCS is in the window for each
for (i in 1:length(judin$name)){
judin$windowM[i] <- max(judin$presnom[i],judin$gridlockMed[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockMed[i],na.rm=T)
judin$windowMJ[i] <- max(judin$presnom[i],judin$gridlockMed[i],judin$gridlockMajMed[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockMed[i],judin$gridlockMajMed[i],na.rm=T)
judin$windowF1F2[i] <- max(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$windowH1H2M[i] <- max(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$windowCM[i] <- max(judin$presnom[i],judin$gridlockMed[i],judin$gridlockComm,na.rm=T) -
  min(judin$presnom[i],judin$gridlockMed[i],judin$gridlockComm,na.rm=T)
judin$windowJH1H2M[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$windowJF1F2[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$windowJCM[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockMed[i],judin$gridlockCommjudin$gridlockComm,na.rm=T) -
  min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockMed[i],judin$gridlockComm,na.rm=T)
judin$windowJF1F2H1H2[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$windowJH1H2CM[i] <- max(judin$presnom[i],judin$gridlockComm,judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockComm,judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$windowJF1F2C[i] <- max(judin$presnom[i],judin$gridlockComm,judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockComm,judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$windowF1F2H1H2[i] <- max(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$windowF1F2C[i] <- max(judin$presnom[i],judin$gridlockComm,judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockComm,judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$windowF1F2H1H2C[i] <- max(judin$presnom[i],judin$gridlockComm,judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockComm,judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$windowH1H2CM[i] <- max(judin$presnom[i],judin$gridlockComm,judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T) -
  min(judin$presnom[i],judin$gridlockComm,judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
}
#The next step is to take all of those windows and replicate PBM into a table
modelwindowM <-glm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1976 & judin$year<=2006,], 
               family=binomial(link="logit"))
modelwindowMJ <-glm(unsuccessful~as.numeric(windowMJ), data=judin[judin$year>=1976 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelwindowF1F2 <-glm(unsuccessful~as.numeric(windowF1F2), data=judin[judin$year>=1976 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelwindowH1H2M <-glm(unsuccessful~as.numeric(windowH1H2M), data=judin[judin$year>=1976 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelwindowCM <-glm(unsuccessful~as.numeric(windowCM), data=judin[judin$year>=1976 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelwindowJH1H2M <-glm(unsuccessful~as.numeric(windowJH1H2M), data=judin[judin$year>=1976 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelwindowJF1F2 <-glm(unsuccessful~as.numeric(windowJF1F2), data=judin[judin$year>=1976 & judin$year<=2006,], 
                      family=binomial(link="logit"))
modelwindowJCM <-glm(unsuccessful~as.numeric(windowJCM), data=judin[judin$year>=1976 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelwindowJF1F2H1H2 <-glm(unsuccessful~as.numeric(windowJF1F2H1H2), data=judin[judin$year>=1976 & judin$year<=2006,], 
                  family=binomial(link="logit"))
modelwindowJH1H2CM <-glm(unsuccessful~as.numeric(windowJH1H2CM), data=judin[judin$year>=1976 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelwindowJF1F2C <-glm(unsuccessful~as.numeric(windowJF1F2C), data=judin[judin$year>=1976 & judin$year<=2006,], 
                  family=binomial(link="logit"))                  
modelwindowF1F2H1H2 <-glm(unsuccessful~as.numeric(windowF1F2H1H2), data=judin[judin$year>=1976 & judin$year<=2006,], 
                  family=binomial(link="logit"))
modelwindowF1F2C<-glm(unsuccessful~as.numeric(windowF1F2C), data=judin[judin$year>=1976 & judin$year<=2006,], 
                  family=binomial(link="logit"))
modelwindowF1F2H1H2C<-glm(unsuccessful~as.numeric(windowF1F2H1H2C), data=judin[judin$year>=1976 & judin$year<=2006,], 
                      family=binomial(link="logit"))
modelwindowH1H2CM<-glm(unsuccessful~as.numeric(windowH1H2CM), data=judin[judin$year>=1976 & judin$year<=2006,], 
                      family=binomial(link="logit"))
outtable <- cbind(c(coef(modelwindowM)[2], coef(modelwindowMJ)[2], coef(modelwindowF1F2)[2],
        coef(modelwindowH1H2M)[2],coef(modelwindowCM)[2],coef(modelwindowJH1H2M)[2],
        coef(modelwindowJF1F2)[2],coef(modelwindowJCM)[2],coef(modelwindowJF1F2H1H2)[2],
        coef(modelwindowJH1H2CM)[2],coef(modelwindowJF1F2C)[2],coef(modelwindowF1F2H1H2C)[2],
                    coef(modelwindowH1H2CM)[2]), 
        c(coef(summary(modelwindowM))[2, 2],coef(summary(modelwindowMJ))[2, 2],coef(summary(modelwindowF1F2))[2, 2],
        coef(summary(modelwindowH1H2M))[2, 2],coef(summary(modelwindowCM))[2, 2],coef(summary(modelwindowJH1H2M))[2, 2],
        coef(summary(modelwindowJF1F2))[2, 2],coef(summary(modelwindowJCM))[2, 2],coef(summary(modelwindowJF1F2H1H2))[2, 2],
        coef(summary(modelwindowJH1H2CM))[2, 2],coef(summary(modelwindowJF1F2C))[2, 2],coef(summary(modelwindowF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowH1H2CM))[2, 2]),
        c(BIC(modelwindowM),BIC(modelwindowMJ),BIC(modelwindowF1F2),
        BIC(modelwindowH1H2M),BIC(modelwindowCM),BIC(modelwindowJH1H2M),
        BIC(modelwindowJF1F2),BIC(modelwindowJCM),BIC(modelwindowJF1F2H1H2),
        BIC(modelwindowJH1H2CM),BIC(modelwindowJF1F2C),BIC(modelwindowF1F2H1H2C),
                    BIC(modelwindowH1H2CM)))

xtable(outtable)

#Next is to do the same thing with JCS inside/outside model
#Check Majority Party Median later

#in is all fucked up SHIITTTTTTfgvs;
#Okay, we're good now. Sorry for that minor freakout
#judin$inM[judin$jcsmedian[i]<=max(judin$presnom[i],judin$gridlockMed[i],na.rm=T) & 
#  judin$jcsmedian[i]>=min(judin$presnom[i],judin$gridlockMed[i],na.rm=T)]<-1
}}
#here's my fix
for (i in 1:length(judin$name)){
judin$maxM[i] <- max(judin$presnom[i],judin$gridlockMed[i],na.rm=T)
judin$minM[i] <- min(judin$presnom[i],judin$gridlockMed[i],na.rm=T)
judin$maxMJ[i] <- max(judin$presnom[i],judin$gridlockMed[i],judin$gridlockMajMed[i],na.rm=T)
judin$minMJ[i] <- min(judin$presnom[i],judin$gridlockMed[i],judin$gridlockMajMed[i],na.rm=T)
judin$maxF1F2[i] <- max(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$minF1F2[i] <- min(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$maxH1H2M[i] <-max(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$minH1H2M[i] <-min(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$maxCM[i] <- max(judin$presnom[i],judin$gridlockMed[i],judin$gridlockComm[i],na.rm=T)
judin$minCM[i] <- min(judin$presnom[i],judin$gridlockMed[i],judin$gridlockComm[i],na.rm=T)
judin$maxJH1H2M[i] <-max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$minJH1H2M[i] <-min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$maxJF1F2[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$minJF1F2[i] <- min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$maxJCM[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockMed[i],judin$gridlockComm[i],na.rm=T)
judin$minJCM[i] <- min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockMed[i],judin$gridlockComm[i],na.rm=T)
judin$maxJF1F2H1H2[i] <- max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$minJF1F2H1H2[i] <- min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$maxJH1H2CM[i] <- max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$minJH1H2CM[i] <- min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$maxJF1F2C[i] <- max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$minJF1F2C[i] <- min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$maxF1F2H1H2[i] <- max(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$minF1F2H1H2[i] <- min(judin$presnom[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$maxF1F2C[i] <- max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$minF1F2C[i] <- min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
judin$maxF1F2H1H2C[i] <- max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$minF1F2H1H2C[i] <- min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T)
judin$maxH1H2CM[i] <- max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
judin$minH1H2CM[i] <- min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMed[i],na.rm=T)
}
judin$inM <- 0
judin$inMJ <- 0
judin$inF1F2 <- 0
judin$inH1H2M <- 0
judin$inCM <- 0
judin$inJH1H2M <- 0
judin$inJF1F2 <- 0
judin$inJCM <- 0
judin$inJF1F2H1H2 <- 0
judin$inJH1H2CM <- 0
judin$inJF1F2C <- 0
judin$inF1F2H1H2 <- 0
judin$inF1F2C <- 0
judin$inF1F2H1H2C <- 0
judin$inH1H2CM <- 0
judin$inFiliNoPres <- 0

judin$inM[judin$jcsmedian<=judin$maxM & judin$jcsmedian>=judin$minM] <- 1
judin$inMJ[judin$jcsmedian<=judin$maxMJ & judin$jcsmedian>=judin$minMJ] <- 1
judin$inF1F2[judin$jcsmedian<=judin$maxF1F2 & judin$jcsmedian>=judin$minF1F2] <- 1
judin$inH1H2M[judin$jcsmedian<=judin$maxH1H2M & judin$jcsmedian>=judin$minH1H2M] <- 1
judin$inCM[judin$jcsmedian<=judin$maxCM & judin$jcsmedian>=judin$minCM] <- 1
judin$inJH1H2M[judin$jcsmedian<=judin$maxJH1H2M & judin$jcsmedian>=judin$minJH1H2M] <- 1
judin$inJF1F2[judin$jcsmedian<=judin$maxJF1F2 & judin$jcsmedian>=judin$minJF1F2] <- 1
judin$inJCM[judin$jcsmedian<=judin$maxJCM & judin$jcsmedian>=judin$minJCM] <- 1
judin$inJF1F2H1H2[judin$jcsmedian<=judin$maxJF1F2H1H2 & judin$jcsmedian>=judin$minJF1F2H1H2] <- 1
judin$inJH1H2CM[judin$jcsmedian<=judin$maxJH1H2CM & judin$jcsmedian>=judin$minJH1H2CM] <- 1
judin$inJF1F2C[judin$jcsmedian<=judin$maxJF1F2C & judin$jcsmedian>=judin$minJF1F2C] <- 1
judin$inF1F2H1H2[judin$jcsmedian<=judin$maxF1F2H1H2 & judin$jcsmedian>=judin$minF1F2H1H2] <- 1
judin$inF1F2C[judin$jcsmedian<=judin$maxF1F2C & judin$jcsmedian>=judin$minF1F2C] <- 1
judin$inF1F2H1H2C[judin$jcsmedian<=judin$maxF1F2H1H2C & judin$jcsmedian>=judin$minF1F2H1H2C] <- 1
judin$inH1H2CM[judin$jcsmedian<=judin$maxH1H2CM & judin$jcsmedian>=judin$minH1H2CM] <- 1
judin$inFiliNoPres[judin$jcsmedian<=judin$gridlockHighFili & judin$jcsmedian>=judin$gridlockLowFili] <- 1



modelinM <-glm(unsuccessful~as.numeric(inM)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                   family=binomial(link="logit"))
modelinMJ <-glm(unsuccessful~as.numeric(inMJ)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                    family=binomial(link="logit"))
modelinF1F2 <-glm(unsuccessful~as.numeric(inF1F2)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                      family=binomial(link="logit"))
modelinOurFili<-glm(unsuccessful~as.numeric(allinpres2)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                    family=binomial(link="logit"))
modelinH1H2M <-glm(unsuccessful~as.numeric(inH1H2M)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                       family=binomial(link="logit"))
modelinCM <-glm(unsuccessful~as.numeric(inCM)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
               family=binomial(link="logit"))
modelinJH1H2M <-glm(unsuccessful~as.numeric(inJH1H2M)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                        family=binomial(link="logit"))
modelinJF1F2 <-glm(unsuccessful~as.numeric(inJF1F2)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                       family=binomial(link="logit"))
modelinJCM <-glm(unsuccessful~as.numeric(inJCM)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                family=binomial(link="logit"))
modelinJF1F2H1H2 <-glm(unsuccessful~as.numeric(inJF1F2H1H2)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                           family=binomial(link="logit"))
modelinJH1H2CM <-glm(unsuccessful~as.numeric(inJH1H2CM)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                    family=binomial(link="logit"))
modelinJF1F2C <-glm(unsuccessful~as.numeric(inJF1F2C)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                   family=binomial(link="logit"))
modelinF1F2H1H2 <-glm(unsuccessful~as.numeric(inF1F2H1H2)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                          family=binomial(link="logit"))
modelinF1F2C <-glm(unsuccessful~as.numeric(inF1F2C)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                    family=binomial(link="logit"))
modelinF1F2H1H2C <-glm(unsuccessful~as.numeric(inF1F2H1H2C)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                      family=binomial(link="logit"))
modelinH1H2CM <-glm(unsuccessful~as.numeric(inH1H2CM)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                     family=binomial(link="logit"))
modelinFiliNoPres <-glm(unsuccessful~as.numeric(inFiliNoPres)+as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                    family=binomial(link="logit"))


outtable2 <- cbind(c(coef(modelinM)[2], coef(modelinMJ)[2], coef(modelinF1F2)[2],
                    coef(modelinH1H2M)[2],coef(modelinCM)[2],coef(modelinJH1H2M)[2],
                    coef(modelinJF1F2)[2],coef(modelinJCM)[2],coef(modelinJF1F2H1H2)[2],
                    coef(modelinJH1H2CM)[2],coef(modelinJF1F2C)[2],coef(modelinF1F2H1H2C)[2],
                    coef(modelinH1H2CM)[2],coef(modelinOurFili)[2]), 
                  c(coef(summary(modelinM))[2, 2],coef(summary(modelinMJ))[2, 2],coef(summary(modelinF1F2))[2, 2],
                    coef(summary(modelinH1H2M))[2, 2],coef(summary(modelinCM))[2, 2],coef(summary(modelinJH1H2M))[2, 2],
                    coef(summary(modelinJF1F2))[2, 2],coef(summary(modelinJCM))[2, 2],coef(summary(modelinJF1F2H1H2))[2, 2],
                    coef(summary(modelinJH1H2CM))[2, 2],coef(summary(modelinJF1F2C))[2, 2],coef(summary(modelinF1F2H1H2C))[2, 2],
                    coef(summary(modelinH1H2CM))[2, 2],coef(summary(modelinOurFili))[2, 2]),
                  c(coef(summary(modelinM))[2, 4],coef(summary(modelinMJ))[2, 4],coef(summary(modelinF1F2))[2, 4],
                     coef(summary(modelinH1H2M))[2, 4],coef(summary(modelinCM))[2, 4],coef(summary(modelinJH1H2M))[2, 4],
                     coef(summary(modelinJF1F2))[2, 4],coef(summary(modelinJCM))[2, 4],coef(summary(modelinJF1F2H1H2))[2, 4],
                     coef(summary(modelinJH1H2CM))[2, 4],coef(summary(modelinJF1F2C))[2, 4],coef(summary(modelinF1F2H1H2C))[2, 4],
                     coef(summary(modelinH1H2CM))[2, 4],coef(summary(modelinOurFili))[2, 4]),
                  c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                    BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                    BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                    BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
                    BIC(modelinH1H2CM),BIC(modelinOurFili)))
xtable(outtable2)

#marginal effect:
inv.logit(coef(modelinMJ) %*% c(1,1,1))-inv.logit(coef(modelinMJ) %*% c(1,0,1))
inv.logit(coef(modelinMJ) %*% c(1,1,0))-inv.logit(coef(modelinMJ) %*% c(1,1,1))



#time loop with BIC ranks
BICs <-c()
ranks <- cbind(c())
for (i in 1940:2010) {
  j <- i+10
#  rankMJ <-glm(unsuccessful~as.numeric(inMJ)+
#                 as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
#               family=binomial(link="logit"))
#  rankJF1F2 <- glm(unsuccessful~as.numeric(inF1F2)+
#                    as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
#                  family=binomial(link="logit"))
  rankH1H2CM <- glm(unsuccessful~as.numeric(inH1H2CM)+
                    as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                  family=binomial(link="logit"))
  rankMJ <- glm(unsuccessful~as.numeric(inMJ)+
                    as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                  family=binomial(link="logit"))
  rankOurFili <- glm(unsuccessful~as.numeric(allinpres2)+
                    as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                  family=binomial(link="logit"))
  BICs <- c(BIC(rankH1H2CM),BIC(rankMJ),BIC(rankOurFili))
  ranks <- cbind(ranks,c(i+5,rank(BICs)))
}

pdf("BICranks2.pdf",width=5,height=7)
par(mfrow=c(3,1)) 
plot(ranks[1,],ranks[3,],main="Party Gatekeeper",pch="O",xlab="5th Year of 10-Year Range", ylab="BIC Ranks (Ties Averaged)", ylim=rev(range(ranks[3,])))
plot(ranks[1,],ranks[2,],main="Blue Slips",xlab="5th Year of 10-Year Range", ylab="BIC Ranks (Ties Averaged)",pch="O", ylim=rev(range(ranks[3,])))
plot(ranks[1,],ranks[4,],main="Filibuster",xlab="5th Year of 10-Year Range", ylab="BIC Ranks (Ties Averaged)",pch="O", ylim=rev(range(ranks[3,])))
dev.off()

#1= PartyGateKeeper, 2=BlueSlips, 3=Filibuster
first <- rbind(ranks[1,6:71],0)
first[2,ranks[3,6:71]==1] <- 1
first[2,ranks[2,6:71]==1] <- 2
first[2,ranks[4,6:71]==1] <- 3
pdf("BICfirstrank.pdf",width=5,height=2.5)
par(mfrow=c(1,1)) 
par(mar=c(3, 1, 3, 1) + 0.1)
plot(first[1,],first[2,],yaxt='n',xaxt='n',ylab="",xlab=""
     , xlim=c(1900,2015),bty="n")
axis(1, at=c(1950,1960,1970,1980,1990,2000,2010))
text(1910,1.1, "Party Gatekeeper", pos=4)
text(1910,2, "Blue Slips", pos=4)
text(1910,2.9, "Filibuster", pos=4)
text(1960,-1, "5th Year of 10-Year Range", pos=4)
mtext("5th Year of 10-Year Range", 1,line=2, at=c(1980,-1))
dev.off()

#I'm rerunning the models changing time periods to put BIC scores in this table
bicAll<- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
           BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
           BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
           BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
           BIC(modelinH1H2CM),BIC(modelinOurFili))
bicStarttoFili <- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
           BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
           BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
           BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
           BIC(modelinH1H2CM),BIC(modelinOurFili))
bicFilitoBork <- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
           BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
           BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
           BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
           BIC(modelinH1H2CM),BIC(modelinOurFili)) 
bicBorktoFili2 <- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
           BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
           BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
           BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
           BIC(modelinH1H2CM),BIC(modelinOurFili))
bicFili2toNow <-c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
           BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
           BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
           BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
           BIC(modelinH1H2CM),BIC(modelinOurFili))

xtable(cbind(bicStarttoFili,bicFilitoBork,bicBorktoFili2,bicFili2toNow))

judin$PrM <- pnorm(judin$maxM,judin$jcsmedian,medianSD)-
  pnorm(judin$minM,judin$jcsmedian,medianSD)
judin$PrJM <- pnorm(judin$maxMJ,judin$jcsmedian,medianSD)-
  pnorm(judin$minMJ,judin$jcsmedian,medianSD)
judin$PrF1F2 <- pnorm(judin$maxF1F2,judin$jcsmedian,medianSD)-
  pnorm(judin$minF1F2,judin$jcsmedian,medianSD)
judin$PrH1H2M <- pnorm(judin$maxH1H2M,judin$jcsmedian,medianSD)-
  pnorm(judin$minH1H2M,judin$jcsmedian,medianSD)
judin$PrCM <- pnorm(max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMed[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMed[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrJH1H2M <- pnorm(max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrJF1F2 <- pnorm(max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrJCM <- pnorm(max(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockComm[i],judin$gridlockMed[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockMajMed[i],judin$gridlockComm[i],judin$gridlockMed[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrJF1F2H1H2 <- pnorm(max(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrH1H2M <-pnorm(max(judin$presnom[i],judin$gridlockMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrJF1F2C <- pnorm(max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrF1F2H1H2 <- pnorm(max(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrF1F2C <- pnorm(max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrF1F2H1H2C <- pnorm(max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T),judin$jcsmedian,medianSD)
judin$PrH1H2CM <-pnorm(max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T),judin$jcsmedian,medianSD)-
  pnorm(min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockMed[i],judin$gridlockBS1[i],judin$gridlockBS2[i],na.rm=T),judin$jcsmedian,medianSD)




#stargazer(modelinM,modelinMJ,modelinF1F2,modelinOurFili,modelinH1H2M,modelinJH1H2M,modelinJF1F2,
#          modelinJF1F2H1H2,modelinF1F2H1H2,flip=F)



judin$gridlockFili <- 0
judin$gridlockFili[judin$nomdatenum < 5492] <- judin$gridlock3367[judin$nomdatenum < 5492]
judin$gridlockFili[judin$nomdatenum >= 5492] <- judin$gridlock4060[judin$nomdatenum >= 5492]

#This next part is kinda out of order
judin$gridlockFiliPres <- 0
judin[judin$nomdatenum < 5492 & judin$presparty==100,]$gridlockFiliPres <- as.numeric(judin[judin$nomdatenum < 5492 & judin$presparty==100,]$gridlock67) - as.numeric(judin[judin$nomdatenum < 5492 & judin$presparty==100,]$presnom)
judin[judin$nomdatenum < 5492 & judin$presparty==200,]$gridlockFiliPres <- as.numeric(judin[judin$nomdatenum < 5492 & judin$presparty==200,]$presnom) - as.numeric(judin[judin$nomdatenum < 5492 & judin$presparty==200,]$gridlock33)
judin[judin$nomdatenum >= 5492 & judin$presparty==100,]$gridlockFiliPres <- as.numeric(judin[judin$nomdatenum >= 5492 & judin$presparty==100,]$gridlock60) - as.numeric(judin[judin$nomdatenum >= 5492 & judin$presparty==100,]$presnom)
judin[judin$nomdatenum >= 5492 & judin$presparty==200,]$gridlockFiliPres <- as.numeric(judin[judin$nomdatenum >= 5492 & judin$presparty==200,]$presnom) - as.numeric(judin[judin$nomdatenum >= 5492 & judin$presparty==200,]$gridlock40)


#Next I modify the main dataset to include whether the JCS Median is in the 40-60 window
#this whole ugly, repetetive mass of code does that
#Since writing this I have modified it to use the 33-67 window until Jan 14 1975 (5492)
#and the 40-60 window after

####This whole part is now obsolete. We have more accurate medians
judin$firstin <- 0
judin$firstin[judin$first<=judin$gridlock60 & judin$first>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$firstin[judin$first<=judin$gridlock67 & judin$first>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$secondin <- 0
judin$secondin[judin$second<=judin$gridlock60 & judin$second>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$secondin[judin$second<=judin$gridlock67 & judin$second>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$thirdin <- 0
judin$thirdin[judin$third<=judin$gridlock60 & judin$third>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$thirdin[judin$third<=judin$gridlock67 & judin$third>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$fourthin <- 0
judin$fourthin[judin$fourth<=judin$gridlock60 & judin$fourth>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$fourthin[judin$fourth<=judin$gridlock67 & judin$fourth>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$fifthin <- 0
judin$fifthin[judin$fifth<=judin$gridlock60 & judin$fifth>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$fifthin[judin$fifth<=judin$gridlock67 & judin$fifth>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$sixthin <- 0
judin$sixthin[judin$sixth<=judin$gridlock60 & judin$sixth>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$sixthin[judin$sixth<=judin$gridlock67 & judin$sixth>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$seventhin <- 0
judin$seventhin[judin$seventh<=judin$gridlock60 & judin$seventh>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$seventhin[judin$seventh<=judin$gridlock67 & judin$seventh>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$eighthin <- 0
judin$eighthin[judin$eighth<=judin$gridlock60 & judin$eighth>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$eighthin[judin$eighth<=judin$gridlock67 & judin$eighth>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$ninthin <- 0
judin$ninthin[judin$ninth<=judin$gridlock60 & judin$ninth>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$ninthin[judin$ninth<=judin$gridlock67 & judin$ninth>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$tenthin <- 0
judin$tenthin[judin$tenth<=judin$gridlock60 & judin$tenth>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$tenthin[judin$tenth<=judin$gridlock67 & judin$tenth>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$eleventhin <- 0
judin$eleventhin[judin$eleventh<=judin$gridlock60 & judin$eleventh>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$eleventhin[judin$eleventh<=judin$gridlock67 & judin$eleventh>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$dcin <- 0
judin$dcin[judin$DC<=judin$gridlock60 & judin$DC>=judin$gridlock40 & judin$nomdatenum>=5492] <- 1
judin$dcin[judin$DC<=judin$gridlock67 & judin$DC>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$allin <- 0
judin$allin[judin$circuit==1] <- judin$firstin[judin$circuit==1]
judin$allin[judin$circuit==2] <- judin$secondin[judin$circuit==2]
judin$allin[judin$circuit==3] <- judin$thirdin[judin$circuit==3]
judin$allin[judin$circuit==4] <- judin$fourthin[judin$circuit==4]
judin$allin[judin$circuit==5] <- judin$fifthin[judin$circuit==5]
judin$allin[judin$circuit==6] <- judin$sixthin[judin$circuit==6]
judin$allin[judin$circuit==7] <- judin$seventhin[judin$circuit==7]
judin$allin[judin$circuit==8] <- judin$eighthin[judin$circuit==8]
judin$allin[judin$circuit==9] <- judin$ninthin[judin$circuit==9]
judin$allin[judin$circuit==10] <- judin$tenthin[judin$circuit==10]
judin$allin[judin$circuit==11] <- judin$eleventhin[judin$circuit==11]
judin$allin[judin$circuit==12] <- judin$dcin[judin$circuit==12]

#presidential ideology scores in our data set are incomplete. I will use voteview numbers
#from House scores, because they have pres scores for our whole period
#just in case the old numbers mean something else I'll create a new variable
#presparty is exactly what it sounds like
#there are 13 judges who don't have a president entered, I will hand edit them
#If it means anything, all 13 were DC judges missing some other data
fix(judin)
#after these hand edits, I want to save this as a distinct data file
write.csv(judin,file="judinhandedit.csv")
judin <- read.csv("judinhandedit.csv")

judin$presnom <-NA
judin$presparty <-NA
judin$presnom[judin$pres=="FDR"] <- -.505
judin$presparty[judin$pres=="FDR"] <- 100
judin$presnom[judin$pres=="HST"] <- -.276
judin$presparty[judin$pres=="HST"] <- 100
judin$presnom[judin$pres=="DDE"] <- .127
judin$presparty[judin$pres=="DDE"] <- 200
judin$presnom[judin$pres=="JFK"] <- -.63
judin$presparty[judin$pres=="JFK"] <- 100
judin$presnom[judin$pres=="LBJ"] <- -.521
judin$presparty[judin$pres=="LBJ"] <- 100
judin$presnom[judin$pres=="RMN"] <- .461
judin$presparty[judin$pres=="RMN"] <- 200
judin$presnom[judin$pres=="GF"] <- .468
judin$presparty[judin$pres=="GF"] <- 200
judin$presnom[judin$pres=="JC"] <- -.729
judin$presparty[judin$pres=="JC"] <- 100
judin$presnom[judin$pres=="RWR"] <- .738
judin$presparty[judin$pres=="RWR"] <- 200
judin$presnom[judin$pres=="GHWB"] <- .649
judin$presparty[judin$pres=="GHWB"] <- 200
judin$presnom[judin$pres=="WJC"] <- -.48
judin$presparty[judin$pres=="WJC"] <- 100
judin$presnom[judin$pres=="GWB"] <- .981
judin$presparty[judin$pres=="GWB"] <- 200
judin$presnom[judin$pres=="BHO"] <- -.378
judin$presparty[judin$pres=="BHO"] <- 100

#Now I make gridlock windows with pres and opposite party filibuster
#Another repetetive, ugly mass of code to do that
#I overwrite the district-in variables, so if I need to redo the earlier part,
#run that whole part again

####This whole part is now obsolete. We have more accurate medians
judin$firstin <- 0
judin$firstin[judin$first<=judin$gridlock60 & judin$first>=judin$presnom & judin$presparty=="100"] <- 1
judin$secondin <- 0
judin$secondin[judin$second<=judin$gridlock60 & judin$second>=judin$presnom & judin$presparty==100] <- 1
judin$thirdin <- 0
judin$thirdin[judin$third<=judin$gridlock60 & judin$third>=judin$presnom & judin$presparty==100] <- 1
judin$fourthin <- 0
judin$fourthin[judin$fourth<=judin$gridlock60 & judin$fourth>=judin$presnom & judin$presparty==100] <- 1
judin$fifthin <- 0
judin$fifthin[judin$fifth<=judin$gridlock60 & judin$fifth>=judin$presnom & judin$presparty==100] <- 1
judin$sixthin <- 0
judin$sixthin[judin$sixth<=judin$gridlock60 & judin$sixth>=judin$presnom & judin$presparty==100] <- 1
judin$seventhin <- 0
judin$seventhin[judin$seventh<=judin$gridlock60 & judin$seventh>=judin$presnom & judin$presparty==100] <- 1
judin$eighthin <- 0
judin$eighthin[judin$eighth<=judin$gridlock60 & judin$eighth>=judin$presnom & judin$presparty==100] <- 1
judin$ninthin <- 0
judin$ninthin[judin$ninth<=judin$gridlock60 & judin$ninth>=judin$presnom & judin$presparty==100] <- 1
judin$tenthin <- 0
judin$tenthin[judin$tenth<=judin$gridlock60 & judin$tenth>=judin$presnom & judin$presparty==100] <- 1
judin$eleventhin <- 0
judin$eleventhin[judin$eleventh<=judin$gridlock60 & judin$eleventh>=judin$presnom & judin$presparty==100] <- 1
judin$dcin <- 0
judin$dcin[judin$DC<=judin$gridlock60 & judin$DC>=judin$presnom & judin$presparty==100] <- 1
judin$firstin[judin$first>=judin$gridlock40 & judin$first<=judin$presnom & judin$presparty==200] <- 1
judin$secondin[judin$second>=judin$gridlock40 & judin$second<=judin$presnom & judin$presparty==200] <- 1
judin$thirdin[judin$third>=judin$gridlock40 & judin$third<=judin$presnom & judin$presparty==200] <- 1
judin$fourthin[judin$fourth>=judin$gridlock40 & judin$fourth<=judin$presnom & judin$presparty==200] <- 1
judin$fifthin[judin$fifth>=judin$gridlock40 & judin$fifth<=judin$presnom & judin$presparty==200] <- 1
judin$sixthin[judin$sixth>=judin$gridlock40 & judin$sixth<=judin$presnom & judin$presparty==200] <- 1
judin$seventhin[judin$seventh>=judin$gridlock40 & judin$seventh<=judin$presnom & judin$presparty==200] <- 1
judin$eighthin[judin$eighth>=judin$gridlock40 & judin$eighth<=judin$presnom & judin$presparty==200] <- 1
judin$ninthin[judin$ninth>=judin$gridlock40 & judin$ninth<=judin$presnom & judin$presparty==200] <- 1
judin$tenthin[judin$tenth>=judin$gridlock40 & judin$tenth<=judin$presnom & judin$presparty==200] <- 1
judin$eleventhin[judin$eleventh>=judin$gridlock40 & judin$eleventh<=judin$presnom & judin$presparty==200] <- 1
judin$dcin[judin$DC>=judin$gridlock40 & judin$DC<=judin$presnom & judin$presparty==200] <- 1
judin$allinpres[judin$circuit==1] <- judin$firstin[judin$circuit==1]
judin$allinpres[judin$circuit==2] <- judin$secondin[judin$circuit==2]
judin$allinpres[judin$circuit==3] <- judin$thirdin[judin$circuit==3]
judin$allinpres[judin$circuit==4] <- judin$fourthin[judin$circuit==4]
judin$allinpres[judin$circuit==5] <- judin$fifthin[judin$circuit==5]
judin$allinpres[judin$circuit==6] <- judin$sixthin[judin$circuit==6]
judin$allinpres[judin$circuit==7] <- judin$seventhin[judin$circuit==7]
judin$allinpres[judin$circuit==8] <- judin$eighthin[judin$circuit==8]
judin$allinpres[judin$circuit==9] <- judin$ninthin[judin$circuit==9]
judin$allinpres[judin$circuit==10] <- judin$tenthin[judin$circuit==10]
judin$allinpres[judin$circuit==11] <- judin$eleventhin[judin$circuit==11]
judin$allinpres[judin$circuit==12] <- judin$dcin[judin$circuit==12]

#then find some way to represent uncertainty
#below I just use a normal dist with jcs medians and SDs
#that's less than ideal obviously
#this is an outstanding part that needs to be improved at some point
#Another even bigger, repetetive mass of code to do this

####Also obsolete

judin$firstinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$first,judin$firstSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$first,judin$firstSD)
judin$secondinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$second,judin$secondSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$second,judin$secondSD)
judin$thirdinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$third,judin$thirdSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$third,judin$thirdSD)
judin$fourthinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$fourth,judin$fourthSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$fourth,judin$fourthSD)
judin$fifthinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$fifth,judin$fifthSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$fifth,judin$fifthSD)
judin$sixthinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$sixth,judin$sixthSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$sixth,judin$sixthSD)
judin$seventhinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$seventh,judin$seventhSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$seventh,judin$seventhSD)
judin$eighthinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$eighth,judin$eighthSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$eighth,judin$eighthSD)
judin$ninthinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$ninth,judin$ninthSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$ninth,judin$ninthSD)
judin$tenthinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$tenth,judin$tenthSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$tenth,judin$tenthSD)
judin$eleventhinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$eleventh,judin$eleventhSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$eleventh,judin$eleventhSD)
judin$DCinP[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],judin$DC,judin$DCSD)-
  pnorm(judin$presnom[judin$presparty==100],judin$DC,judin$DCSD)

judin$firstinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$first,judin$firstSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$first,judin$firstSD)
judin$secondinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$second,judin$secondSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$second,judin$secondSD)
judin$thirdinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$third,judin$thirdSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$third,judin$thirdSD)
judin$fourthinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$fourth,judin$fourthSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$fourth,judin$fourthSD)
judin$fifthinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$fifth,judin$fifthSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$fifth,judin$fifthSD)
judin$sixthinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$sixth,judin$sixthSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$sixth,judin$sixthSD)
judin$seventhinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$seventh,judin$seventhSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$seventh,judin$seventhSD)
judin$eighthinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$eighth,judin$eighthSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$eighth,judin$eighthSD)
judin$ninthinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$ninth,judin$ninthSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$ninth,judin$ninthSD)
judin$tenthinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$tenth,judin$tenthSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$tenth,judin$tenthSD)
judin$eleventhinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$eleventh,judin$eleventhSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$eleventh,judin$eleventhSD)
judin$DCinP[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$DC,judin$DCSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$DC,judin$DCSD)

judin$allinP[judin$circuit==1] <- judin$firstinP[judin$circuit==1]
judin$allinP[judin$circuit==2] <- judin$secondinP[judin$circuit==2]
judin$allinP[judin$circuit==3] <- judin$thirdinP[judin$circuit==3]
judin$allinP[judin$circuit==4] <- judin$fourthinP[judin$circuit==4]
judin$allinP[judin$circuit==5] <- judin$fifthinP[judin$circuit==5]
judin$allinP[judin$circuit==6] <- judin$sixthinP[judin$circuit==6]
judin$allinP[judin$circuit==7] <- judin$seventhinP[judin$circuit==7]
judin$allinP[judin$circuit==8] <- judin$eighthinP[judin$circuit==8]
judin$allinP[judin$circuit==9] <- judin$ninthinP[judin$circuit==9]
judin$allinP[judin$circuit==10] <- judin$tenthinP[judin$circuit==10]
judin$allinP[judin$circuit==11] <- judin$eleventhinP[judin$circuit==11]
judin$allinP[judin$circuit==12] <- judin$DCinP[judin$circuit==12]


#since I now have medians at the time of nomination I can easilly do this next part
#this will replace the allin, allinpres, and allinP variables, which I'm keeping 
#around for record keeping
judin$allin2 <- 0
judin$allin2[as.numeric(judin$jcsmedian)<=judin$gridlock60 & as.numeric(judin$jcsmedian)>=judin$gridlock40 & judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$allin2[as.numeric(judin$jcsmedian)<=judin$gridlock67 & as.numeric(judin$jcsmedian)>=judin$gridlock33 & judin$nomdatenum<5492] <- 1
judin$allin2[as.numeric(judin$jcsmedian)<=judin$gridlockMed & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100" & judin$nomdatenum>=19382] <- 1
judin$allin2[as.numeric(judin$jcsmedian)>=judin$gridlockMed & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200" & judin$nomdatenum>=19382] <- 1

judin$allinpres2 <- 0
judin$allinpres2[as.numeric(judin$jcsmedian)<=judin$gridlock60 & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100"& judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$allinpres2[as.numeric(judin$jcsmedian)>=judin$gridlock40 & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200"& judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$allinpres2[as.numeric(judin$jcsmedian)<=judin$gridlock67 & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100"& judin$nomdatenum<5492] <- 1
judin$allinpres2[as.numeric(judin$jcsmedian)>=judin$gridlock33 & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200"& judin$nomdatenum<5492] <- 1
judin$allinpres2[as.numeric(judin$jcsmedian)<=judin$gridlockMed & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100" & judin$nomdatenum>=19382] <- 1
judin$allinpres2[as.numeric(judin$jcsmedian)>=judin$gridlockMed & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200" & judin$nomdatenum>=19382] <- 1


#recoding aba variable, because apparently I've been using it wrong for a while
judin$WQ <- NA
judin$WQ[judin$aba==3 |judin$aba==4 |judin$aba==5 |judin$aba==7] <- 0
judin$WQ[judin$aba==1 |judin$aba==2] <- 1

#Makeshift probability inside pres window. Replaces the earlier one
judin$jcsmedian <- as.numeric(judin$jcsmedian)
#I should have done that part immediately
medianSD <- sqrt(var(judin$jcsmedian, na.rm=T))
medianSD<-as.numeric(medianSD)

judin$allinP2[judin$presparty==100] <- pnorm(judin$gridlock60[judin$presparty==100],
  judin$jcsmedian[judin$presparty==100],medianSD)-pnorm(judin$presnom[judin$presparty==100],judin$jcsmedian[judin$presparty==100],medianSD)
judin$allinP2[judin$presparty==200] <- pnorm(judin$presnom[judin$presparty==200],judin$jcsmedian[judin$presparty==200],medianSD)-
  pnorm(judin$gridlock40[judin$presparty==200],judin$jcsmedian[judin$presparty==200],medianSD)

models <- cbind()
coefs <- cbind()
stderrors <- cbind()
for (i in 1940:2010) {
j <- i+20
model <-glm(unsuccessful~as.numeric(allinpres2)+as.numeric(year)+
                   as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                 family=binomial(link="logit"))
models <- cbind(models, model)
coefs <- cbind(coefs,coef(model))
stderrors <- cbind(stderrors,summary(model)$coefficients[, 2])
}

models <- t(models)
coefs <- t(coefs)
stderrors <-t(stderrors)
plot(1940:2010,coefs[,2])
segments(x0=1940:2010,,x1=1940:2010,y0=coefs[,2]-1.96*stderrors[,2],y1=coefs[,2]+1.96*stderrors[,2])
abline(a=0,b=0)
#coefs <- cbind(models[,1],c(1940:2010), c())
#coefs[,2]<- 1940:2010
#coefs[,3]<- coefs[,1][2]
#newvar<-NA
#for (i in 1:length(coefs[,2])) {
#newvar <- coefs[i,1]
#coefs[i,3] <- newvar[2]
#}

#Plot check of earlier piece
plot(as.numeric(judgrid[,2]),as.numeric(judgrid[,8]))

x <- senators[senators$cong == judin$congress.x[250],]
x <- x[judin$nomdatenum[250] >= x$datebeginnum & judin$nomdatenum[250] <= x$dateendnum,]
x <- x[is.na(x$name)==F,]

plot(as.numeric(y[,3]),as.numeric(y[,1]),xlab="Congress",ylab="Number of Senators")

#Models, first with gridlock window size. This is roughly a replication of Primo, Binder,
#and Maltzman

full <-glm(unsuccessful~as.numeric(gridlockFiliPres)+as.numeric(year)+
            as.numeric(aba), data=judin[judin$nomdatenum<19382,], 
            family=binomial(link="logit"))
fullnoyear <-glm(unsuccessful~as.numeric(gridlockFiliPres)+
              as.numeric(aba), data=judin[judin$nomdatenum<19382,], 
            family=binomial(link="logit"))
onlyaba <-glm(unsuccessful~as.numeric(aba), data=judin[judin$nomdatenum<19382,], 
              family=binomial(link="logit"))
onlygrid <-glm(unsuccessful~as.numeric(gridlockFiliPres), data=judin[judin$nomdatenum<19382,], 
                family=binomial(link="logit"))
nogrid <-glm(unsuccessful~as.numeric(year)+
               as.numeric(aba), data=judin[judin$nomdatenum<19382,], 
             family=binomial(link="logit"))
stargazer(full,nogrid,fullnoyear,onlyaba,onlygrid)

#Same since 1975

full <-glm(unsuccessful~as.numeric(gridlockFiliPres)+as.numeric(year)+
             as.numeric(aba), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
           family=binomial(link="logit"))
fullnoyear <-glm(unsuccessful~as.numeric(gridlockFiliPres)+
                   as.numeric(aba), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                 family=binomial(link="logit"))
onlyaba <-glm(unsuccessful~as.numeric(aba), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
              family=binomial(link="logit"))
onlygrid <-glm(unsuccessful~as.numeric(gridlockFiliPres), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
               family=binomial(link="logit"))
nogrid <-glm(unsuccessful~as.numeric(year)+
             as.numeric(aba), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
           family=binomial(link="logit"))

stargazer(full,nogrid,fullnoyear,onlyaba,onlygrid)

#next whether the JCS score is in the window, still 40-60/33-67 style
fullin <-glm(unsuccessful~as.numeric(allin2)+as.numeric(year)+
             as.numeric(WQ), data=judin, 
           family=binomial(link="logit"))
fullnoyearin <-glm(unsuccessful~as.numeric(allin2)+
                   as.numeric(WQ), data=judin, 
                 family=binomial(link="logit"))
onlygridin <-glm(unsuccessful~as.numeric(allin2), data=judin, 
               family=binomial(link="logit"))
onlyaba <-glm(unsuccessful~as.numeric(WQ), data=judin, 
              family=binomial(link="logit"))
stargazer(fullin,fullnoyearin,onlyaba,onlygridin)

#whether JCS score is in pres/opposite filibuster window after 70s fili reform
fullinpres1<-glm(unsuccessful~as.numeric(allinpres)+as.numeric(year)+
                   as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                 family=binomial(link="logit"))
fullinpres <-glm(unsuccessful~as.numeric(allinpres2)+as.numeric(year)+
               as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
             family=binomial(link="logit"))
fullnoyearinpres <-glm(unsuccessful~as.numeric(allinpres2)+
                     as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                   family=binomial(link="logit"))
onlygridinpres <-glm(unsuccessful~as.numeric(allinpres2), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                 family=binomial(link="logit"))
nogrid <-glm(unsuccessful~as.numeric(year)+
               as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
             family=binomial(link="logit"))
onlyaba <-glm(unsuccessful~as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
              family=binomial(link="logit"))
stargazer(fullinpres1,fullinpres,nogrid,fullnoyearinpres,onlyaba,onlygridinpres)

#same thing over whole data set
fullinpres1<-glm(unsuccessful~as.numeric(allinpres)+as.numeric(year)+
                   as.numeric(WQ), data=judin, 
                 family=binomial(link="logit"))
fullinpres <-glm(unsuccessful~as.numeric(allinpres2)+as.numeric(year)+
                   as.numeric(WQ), data=judin, 
                 family=binomial(link="logit"))
fullnoyearinpres <-glm(unsuccessful~as.numeric(allinpres2)+
                         as.numeric(WQ), data=judin, 
                       family=binomial(link="logit"))
onlygridinpres <-glm(unsuccessful~as.numeric(allinpres2), data=judin, 
                     family=binomial(link="logit"))
nogrid <-glm(unsuccessful~as.numeric(year)+
               as.numeric(WQ), data=judin, 
             family=binomial(link="logit"))
onlyaba <-glm(unsuccessful~as.numeric(WQ), data=judin, 
              family=binomial(link="logit"))
stargazer(fullinpres1,fullinpres,nogrid,fullnoyearinpres,onlyaba,onlygridinpres)
#Same thing broken down by different time periods
starttofili1 <-glm(unsuccessful~as.numeric(allinpres2)+
                   as.numeric(WQ), data=judin[judin$nomdatenum<5492,], 
                 family=binomial(link="logit"))
fili1tobork <-glm(unsuccessful~as.numeric(allinpres2)+
                   as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<10157,], 
                 family=binomial(link="logit"))
borktofili2 <-glm(unsuccessful~as.numeric(allinpres2)+
                   as.numeric(WQ), data=judin[judin$nomdatenum>=10157 & judin$nomdatenum<19382,], 
                 family=binomial(link="logit"))
fili2tonow <-glm(unsuccessful~as.numeric(allinpres2)+
                    as.numeric(WQ), data=judin[judin$nomdatenum>=19382,], 
                  family=binomial(link="logit"))
stargazer(starttofili1,fili1tobork,borktofili2,fili2tonow)



#makeshift probability in window with pres
fullinpresP <-glm(unsuccessful~as.numeric(allinP2)+as.numeric(year)+
                   as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                 family=binomial(link="logit"))
fullnoyearinpresP <-glm(unsuccessful~as.numeric(allinP2)+
                         as.numeric(WQ), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                       family=binomial(link="logit"))
onlygridinpresP <-glm(unsuccessful~as.numeric(allinP2), data=judin[judin$nomdatenum>=5492 & judin$nomdatenum<19382,], 
                     family=binomial(link="logit"))
stargazer(fullinpresP,fullnoyearinpresP,onlyaba,onlygridinpresP)



seq1 <- seq(from=-1,to=1,by=.05)
norm1 <- dnorm(seq1,judin$tenth[795],judin$tenthSD[795])
plot(seq1,norm1,type="l",xlab="",ylab="")
segments(x0=c(judin$gridlock40[795],judin$gridlock60[795]),y0=c(0,0),
        x1=c(judin$gridlock40[795],judin$gridlock60[795]), 
        y1=c(dnorm(judin$gridlock40[795],judin$tenth[795],judin$tenthSD[795]),
             dnorm(judin$gridlock60[795],judin$tenth[795],judin$tenthSD[795])),lwd=2)

plot(judin$nomdatenum,judin$gridlock4060,xlab="Numeric Date", ylab="40-60 Window")
plot(judin$nomdatenum,judin$gridlockFili,xlab="Numeric Date", ylab="33-67/40-60 Window")
plot(judin$nomdatenum,judin$gridlockFiliPres,xlab="Numeric Date", ylab="33-67/40-60/Pres Window")


#judge selection issue
judin$aba[judin$aba==9999] <- NA
meanaba <-aggregate(judin, by=list(judin$year), FUN=mean, na.rm=TRUE)
meanaba$windowF1F2change <- 0
meanaba$abachange <- 0
meanaba$WQchange <- 0
for (i in 2:length(meanaba$year)) {
  meanaba$windowMJchange[i] <- meanaba$windowMJ[i] - meanaba$windowMJ[i-1]
  meanaba$windowF1F2change[i] <- meanaba$windowF1F2[i] - meanaba$windowF1F2[i-1]
  meanaba$abachange[i] <- meanaba$aba[i] - meanaba$aba[i-1]
  meanaba$WQchange[i] <- meanaba$WQ[i] - meanaba$WQ[i-1]
}

submeanaba<-meanaba[meanaba$year%%2 == 1,]
submeanaba$lastyear <-submeanaba$year - 1
submeanaba<-submeanaba[submeanaba$lastyear%%4 != 0,]

pdf("WQChange.pdf",width=5,height=4)
par(mar=c(4,4,4,4))
plot(submeanaba$windowF1F2change,submeanaba$WQchange, col="white", xlab="Filibuster Window Change", 
     ylab="Well Qualified Change", main="Midterm Elections")
text(submeanaba$windowF1F2change,submeanaba$WQchange,labels=as.character(submeanaba$lastyear))
abline(lm(submeanaba$WQchange~submeanaba$windowF1F2change))
dev.off()

xtable(summary(lm(submeanaba$WQchange~submeanaba$windowF1F2change)))

#plot(submeanaba$gridlockchange,submeanaba$WQchange, xlab="FiliPres Change", col="white",
#     ylab="ABA Change", main="Midterm Elections" , xlim=c(-.1,.4), ylim=c(-.5,.3))
#text(submeanaba$gridlockchange,submeanaba$WQchange,labels=as.character(submeanaba$lastyear))
#abline(lm(submeanaba$WQchange~submeanaba$gridlockchange))

plot(meanaba$year, meanaba$WQ, xlab="year", ylab="Mean ABA Score")
points(submeanaba$year,submeanaba$WQ, col="red", pch=16)


library(foreign)
write.dta(data=judin, file="judinfeb1.dta")
