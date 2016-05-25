#This is an updated version of our project code. I've taken out obsolete parts,
#reordered things and added some annotation.

### Loading Required Packages
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
library(rms)
library(Zelig)

### Setting the working directory
setwd("~/Dropbox/Gridlock Nominations/Data Updates")

rm(list=ls())

#DW Nominate data from voteview.com:
nom <- read.dta("SenDWNom113.dta")
#List of judges with characteristics from our coders. Merged in Stata:
#jud <- read.dta("CtAppWide10-17-2015.dta")
#jud <- jud[c(1:45,88)]

#We don't use JCS medians any more, now that we've calculated our own medians,
#but replication or extension using that data might be useful later.
#JCS median data:
#jcs <- read.dta("JCS medians.dta")

#Membership of the Senate Judiciary Committee and their DW Nominate scores:
committee <- read.dta("judiciaryCommNom.dta")

#Merged list of judges with nomination/retirement dates, and JCS scores
#named "old" only because it is a Stata12 file not Stata13:
newjcs <- read.dta("V2_mergedjudgesold.dta")

#Merged version of our lists of Senators, with Nominate Scores hand-imputed from voteview 
#data for those who served insufficient time in the relevant
#Congress. The "Notes" variable includes where that Nominate score came from.
senators <- read.dta("allsen1.dta")

senators$party[senators$party==112] <- 200
senators$party[senators$party==370] <- 200
senators$party[senators$party==537] <- 100
senators$party[senators$party==328 & senators$name=="NORRIS"] <- 100
#Wayne Morse (I-OR) was an independent in the 83rd Congress, which was apparently
#important to party control, which switched several times
senators$party[senators$party==328 & senators$statenm=="VIRGINI"] <- 100
senators$party[senators$party==328 & senators$name=="JEFFORDS"] <- 100
senators$party[senators$party==328 & senators$name=="SANDERS"] <- 100
senators$party[senators$party==328 & senators$name=="KING"] <- 100
senators$party[senators$party=="NA" & senators$name=="SCOTT"] <- 200
senators$party[senators$party=="NA" & senators$name=="BARKLEY"] <- 328
#I am still going to call Barkley an independent. He caucused with neither party,
#was appointed by an independent and only served this term. He was the Nominate
#fix that was most ad-hoc. I ended up using the average of Wellstone and Coleman


#Primo, Binder, and Maltzman 2008 replication data
pbm <- read.dta("ajps2008appellate.dta")


#jcs<-rename(jcs,c(Year="year"))
#judin <- merge(jud,jcs,by="year")

#Millet Patricia Anne in 113th congress not 112
#presidential ideology scores in our data set are incomplete. I use voteview numbers
#from House scores, because they have pres scores for our whole period
#there are 13 judges who don't have a president entered, I will hand edit them
#If it means anything, all 13 were DC judges missing some other data
#fix(judin)
#after these hand edits, I want to save this as a distinct data file
#write.csv(judin,file="judinhandedit.csv")
judin <- read.csv("judinhandedit.csv")

judin$presnom <-NA
judin$presparty <-NA
judin$pres[judin$nomdatenum>=-9799 & judin$nomdatenum<=-5377] <- "FDR"
judin$presnom[judin$pres=="FDR"] <- -.505
judin$presparty[judin$pres=="FDR"] <- 100
judin$pres[judin$nomdatenum>=-5376 & judin$nomdatenum<=-2538] <- "HST"
judin$presnom[judin$pres=="HST"] <- -.276
judin$presparty[judin$pres=="HST"] <- 100
judin$pres[judin$nomdatenum>=-2537 & judin$nomdatenum<=384] <- "DDE"
judin$presnom[judin$pres=="DDE"] <- .313
judin$presparty[judin$pres=="DDE"] <- 200
judin$pres[judin$nomdatenum>=385 & judin$nomdatenum<=1421] <- "JFK"
judin$presnom[judin$pres=="JFK"] <- -.501
judin$presparty[judin$pres=="JFK"] <- 100
judin$pres[judin$nomdatenum>=1422 & judin$nomdatenum<=3306] <- "LBJ"
judin$presnom[judin$pres=="LBJ"] <- -.239
judin$presparty[judin$pres=="LBJ"] <- 100
judin$pres[judin$nomdatenum>=3307 & judin$nomdatenum<=5334] <- "RMN"
judin$presnom[judin$pres=="RMN"] <- .466
judin$presparty[judin$pres=="RMN"] <- 200
judin$pres[judin$nomdatenum>=5335 & judin$nomdatenum<=6226] <- "GF"
judin$presnom[judin$pres=="GF"] <- .363
judin$presparty[judin$pres=="GF"] <- 200
judin$pres[judin$nomdatenum>=6229 & judin$nomdatenum<=7689] <- "JC"
judin$presnom[judin$pres=="JC"] <- -.402
judin$presparty[judin$pres=="JC"] <- 100
judin$pres[judin$nomdatenum>=7690 & judin$nomdatenum<=10611] <- "RWR"
judin$presnom[judin$pres=="RWR"] <- .49
judin$presparty[judin$pres=="RWR"] <- 200
judin$pres[judin$nomdatenum>=10612 & judin$nomdatenum<=12072] <- "GHWB"
judin$presnom[judin$pres=="GHWB"] <- .431
judin$presparty[judin$pres=="GHWB"] <- 200
judin$pres[judin$nomdatenum>=12073 & judin$nomdatenum<=14994] <- "WJC"
judin$presnom[judin$pres=="WJC"] <- -.399
judin$presparty[judin$pres=="WJC"] <- 100
judin$pres[judin$nomdatenum>=14995 & judin$nomdatenum<=17550] <- "GWB"
judin$presnom[judin$pres=="GWB"] <- .489
judin$presparty[judin$pres=="GWB"] <- 200
judin$pres[judin$nomdatenum>=17551 & judin$nomdatenum<=20838] <- "BHO"
judin$presnom[judin$pres=="BHO"] <- -.364
judin$presparty[judin$pres=="BHO"] <- 100


#the above loop gives you 795 lists of Senators, their id numbers and their 1st dim score
#some 1st dim scores were hand imputed to "senators" in STATA
#It also separates senators our by majority and minority party

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

#Below generates lists of judges on the relevant court at the time of nomination
#that is, the status quo arrangement of the court
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

#plot(judjcs[,2], b[,1], xlab="year", ylab="number of judges")

judjcs[,6]<- lapply(judjcs[,5],quantile,probs= c(.5),  na.rm = TRUE)
judin$jcsmedian <- judjcs[,6]


#Below gives Judiciary Committee members at time of nomination
#The Judiciary committee is sometimes officially appointed late Jan-late Feb, meaning
#a small number of judges were nominated in times where the Jud committee for that Cong
#had not yet been appointed. We assume that approximate membership is known before
#are made official. That is accounted for in the if statement in the below loop


d<- cbind(c())
e<- cbind(c(),c())
judcomm <- array(list(), dim=c(795, 5))

for (i in 1:length(judin$nomdate)){
  d <- committee[committee$cong == judin$congress.x[i],]
  d <- d[judin$nomdatenum[i] >= d$DateofAppointment & judin$nomdatenum[i] <= d$DateofTermination,]
  d <- d[is.na(d$name)==F,]
  if (length(d$name)==0) {
    d <- committee[committee$cong == judin$congress.x[i],]
    d <- d[judin$nomdatenum[i] >= d$DateofAppointment-51 & judin$nomdatenum[i] <= d$DateofTermination,]
    d <- d[is.na(d$name)==F,]
  }  
  judcomm[i,1] <- judin$name[i]
  judcomm[i,2] <- judin$year[i]
  judcomm[i,3] <- list(d$name)
  judcomm[i,4] <- list(d$dwnom1)
  e <- cbind(c(e[,1],length(d$name)),c(e[,2],judin$name[i]),c(e[,3],judin$year[i]))
}
judcomm[,5]<- lapply(judcomm[,4],quantile,probs= c(0.5),  na.rm = TRUE)


#Below gives the 40th and 60th Sens, the difference, the 33rd, 67th and median Sens
#and then puts them back into the main dataset

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

#At this point we have all the relevant pivots in on of the matrices made by the above
#loops. Next we put them back in the main dataset
#The syntax is "gridlock[pivot abreviation]" for a pivot. 4060 and 3367 are for checks of
#naive filibuster windows.
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
#5492 is the numeric date in 1975 when the cloture requirement changed from 2/3 to 3/5
judin$gridlockLowFili[judin$nomdatenum < 5492] <- judin$gridlock33[judin$nomdatenum < 5492]
judin$gridlockLowFili[judin$nomdatenum >= 5492] <- judin$gridlock40[judin$nomdatenum >= 5492]
judin$gridlockHighFili <- 0
judin$gridlockHighFili[judin$nomdatenum < 5492] <- judin$gridlock67[judin$nomdatenum < 5492]
judin$gridlockHighFili[judin$nomdatenum >= 5492] <- judin$gridlock60[judin$nomdatenum >= 5492]
judin$divided<- 0
judin$divided[judin$majParty!=judin$presparty] <-1

#With all the pivots coded we replicate PBM. Windows are just the raw size of the interval
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
  judin$windowJF1F2H1H2C[i] <- max(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockMajMed[i],judin$gridlockComm,na.rm=T) -
    min(judin$presnom[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],judin$gridlockMajMed[i],judin$gridlockComm,na.rm=T)    
}
#The next step is to take all of those windows and replicate PBM into a table
modelwindowM <-glm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                   family=binomial(link="logit"))

#robustM <- lrm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,])

modelwindowMJ <-glm(unsuccessful~as.numeric(windowMJ), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                    family=binomial(link="logit"))
modelwindowF1F2 <-glm(unsuccessful~as.numeric(windowF1F2), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                      family=binomial(link="logit"))
modelwindowH1H2M <-glm(unsuccessful~as.numeric(windowH1H2M), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                       family=binomial(link="logit"))
modelwindowCM <-glm(unsuccessful~as.numeric(windowCM), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                    family=binomial(link="logit"))
modelwindowJH1H2M <-glm(unsuccessful~as.numeric(windowJH1H2M), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                        family=binomial(link="logit"))
modelwindowJF1F2 <-glm(unsuccessful~as.numeric(windowJF1F2), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                       family=binomial(link="logit"))
modelwindowJCM <-glm(unsuccessful~as.numeric(windowJCM), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                     family=binomial(link="logit"))
modelwindowJF1F2H1H2 <-glm(unsuccessful~as.numeric(windowJF1F2H1H2), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                           family=binomial(link="logit"))
modelwindowJH1H2CM <-glm(unsuccessful~as.numeric(windowJH1H2CM), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                         family=binomial(link="logit"))
modelwindowJF1F2C <-glm(unsuccessful~as.numeric(windowJF1F2C), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                        family=binomial(link="logit"))                  
modelwindowF1F2H1H2 <-glm(unsuccessful~as.numeric(windowF1F2H1H2), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                          family=binomial(link="logit"))
modelwindowF1F2C<-glm(unsuccessful~as.numeric(windowF1F2C), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                      family=binomial(link="logit"))
modelwindowF1F2H1H2C<-glm(unsuccessful~as.numeric(windowF1F2H1H2C), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                          family=binomial(link="logit"))
modelwindowH1H2CM<-glm(unsuccessful~as.numeric(windowH1H2CM), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                       family=binomial(link="logit"))
modelwindowJF1F2H1H2C<-glm(unsuccessful~as.numeric(windowJF1F2H1H2C), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                       family=binomial(link="logit"))
modelwindowdivided <- glm(unsuccessful~as.numeric(divided), data=judin[judin$year>=1975 & judin$year<=2006 & judin$circuit<12,], 
                    family=binomial(link="logit"))


outtable <- cbind(c(coef(modelwindowM)[2], coef(modelwindowMJ)[2], coef(modelwindowF1F2)[2],
                    coef(modelwindowH1H2M)[2],coef(modelwindowCM)[2],coef(modelwindowJH1H2M)[2],
                    coef(modelwindowJF1F2)[2],coef(modelwindowJCM)[2],coef(modelwindowJF1F2H1H2)[2],
                    coef(modelwindowJH1H2CM)[2],coef(modelwindowJF1F2C)[2],
                    coef(modelwindowF1F2H1H2)[2],coef(modelwindowF1F2C)[2],                    
                    coef(modelwindowF1F2H1H2C)[2],coef(modelwindowH1H2CM)[2],coef(modelwindowJF1F2H1H2C)[2],
                    coef(modelwindowdivided)[2]), 
                  c(coef(summary(modelwindowM))[2, 2],coef(summary(modelwindowMJ))[2, 2],coef(summary(modelwindowF1F2))[2, 2],
                    coef(summary(modelwindowH1H2M))[2, 2],coef(summary(modelwindowCM))[2, 2],coef(summary(modelwindowJH1H2M))[2, 2],
                    coef(summary(modelwindowJF1F2))[2, 2],coef(summary(modelwindowJCM))[2, 2],coef(summary(modelwindowJF1F2H1H2))[2, 2],
                    coef(summary(modelwindowJH1H2CM))[2, 2],coef(summary(modelwindowJF1F2C))[2, 2]
                    ,coef(summary(modelwindowF1F2H1H2))[2, 2],coef(summary(modelwindowF1F2C))[2, 2]
                    ,coef(summary(modelwindowF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowH1H2CM))[2, 2],coef(summary(modelwindowJF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowdivided))[2, 2]),
                  c(BIC(modelwindowM),BIC(modelwindowMJ),BIC(modelwindowF1F2),
                    BIC(modelwindowH1H2M),BIC(modelwindowCM),BIC(modelwindowJH1H2M),
                    BIC(modelwindowJF1F2),BIC(modelwindowJCM),BIC(modelwindowJF1F2H1H2),
                    BIC(modelwindowJH1H2CM),BIC(modelwindowJF1F2C),
                    BIC(modelwindowF1F2H1H2),BIC(modelwindowF1F2C),
                    BIC(modelwindowF1F2H1H2C),
                    BIC(modelwindowH1H2CM),BIC(modelwindowJF1F2H1H2C),
                    BIC(modelwindowdivided)))

### Window size using our data, no JCS scores (i.e. no status quo)
### Coefficient on main variable, Standard Error on main variable, and BIC

rownames(outtable)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtable<-outtable[order(outtable[,3]),]
xtable(outtable)

### Taking that same approach (window size using our data, no JCS scores) but using the 1975-1988 time period 

modelwindowM <-glm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1975 & judin$year<=1988,], 
                   family=binomial(link="logit"))

#robustM <- lrm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1975 & judin$year<=1988,])

modelwindowMJ <-glm(unsuccessful~as.numeric(windowMJ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelwindowF1F2 <-glm(unsuccessful~as.numeric(windowF1F2), data=judin[judin$year>=1975 & judin$year<=1988,], 
                      family=binomial(link="logit"))
modelwindowH1H2M <-glm(unsuccessful~as.numeric(windowH1H2M), data=judin[judin$year>=1975 & judin$year<=1988,], 
                       family=binomial(link="logit"))
modelwindowCM <-glm(unsuccessful~as.numeric(windowCM), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelwindowJH1H2M <-glm(unsuccessful~as.numeric(windowJH1H2M), data=judin[judin$year>=1975 & judin$year<=1988,], 
                        family=binomial(link="logit"))
modelwindowJF1F2 <-glm(unsuccessful~as.numeric(windowJF1F2), data=judin[judin$year>=1975 & judin$year<=1988,], 
                       family=binomial(link="logit"))
modelwindowJCM <-glm(unsuccessful~as.numeric(windowJCM), data=judin[judin$year>=1975 & judin$year<=1988,], 
                     family=binomial(link="logit"))
modelwindowJF1F2H1H2 <-glm(unsuccessful~as.numeric(windowJF1F2H1H2), data=judin[judin$year>=1975 & judin$year<=1988,], 
                           family=binomial(link="logit"))
modelwindowJH1H2CM <-glm(unsuccessful~as.numeric(windowJH1H2CM), data=judin[judin$year>=1975 & judin$year<=1988,], 
                         family=binomial(link="logit"))
modelwindowJF1F2C <-glm(unsuccessful~as.numeric(windowJF1F2C), data=judin[judin$year>=1975 & judin$year<=1988,], 
                        family=binomial(link="logit"))                  
modelwindowF1F2H1H2 <-glm(unsuccessful~as.numeric(windowF1F2H1H2), data=judin[judin$year>=1975 & judin$year<=1988,], 
                          family=binomial(link="logit"))
modelwindowF1F2C<-glm(unsuccessful~as.numeric(windowF1F2C), data=judin[judin$year>=1975 & judin$year<=1988,], 
                      family=binomial(link="logit"))
modelwindowF1F2H1H2C<-glm(unsuccessful~as.numeric(windowF1F2H1H2C), data=judin[judin$year>=1975 & judin$year<=1988,], 
                          family=binomial(link="logit"))
modelwindowH1H2CM<-glm(unsuccessful~as.numeric(windowH1H2CM), data=judin[judin$year>=1975 & judin$year<=1988,], 
                       family=binomial(link="logit"))
modelwindowJF1F2H1H2C<-glm(unsuccessful~as.numeric(windowJF1F2H1H2C), data=judin[judin$year>=1975 & judin$year<=1988,], 
                       family=binomial(link="logit"))
modelwindowdivided <- glm(unsuccessful~as.numeric(divided), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))


outtable <- cbind(c(coef(modelwindowM)[2], coef(modelwindowMJ)[2], coef(modelwindowF1F2)[2],
                    coef(modelwindowH1H2M)[2],coef(modelwindowCM)[2],coef(modelwindowJH1H2M)[2],
                    coef(modelwindowJF1F2)[2],coef(modelwindowJCM)[2],coef(modelwindowJF1F2H1H2)[2],
                    coef(modelwindowJH1H2CM)[2],coef(modelwindowJF1F2C)[2],
                    coef(modelwindowF1F2H1H2)[2],coef(modelwindowF1F2C)[2],                    
                    coef(modelwindowF1F2H1H2C)[2],coef(modelwindowH1H2CM)[2],coef(modelwindowJF1F2H1H2C)[2],
                    coef(modelwindowdivided)[2]), 
                  c(coef(summary(modelwindowM))[2, 2],coef(summary(modelwindowMJ))[2, 2],coef(summary(modelwindowF1F2))[2, 2],
                    coef(summary(modelwindowH1H2M))[2, 2],coef(summary(modelwindowCM))[2, 2],coef(summary(modelwindowJH1H2M))[2, 2],
                    coef(summary(modelwindowJF1F2))[2, 2],coef(summary(modelwindowJCM))[2, 2],coef(summary(modelwindowJF1F2H1H2))[2, 2],
                    coef(summary(modelwindowJH1H2CM))[2, 2],coef(summary(modelwindowJF1F2C))[2, 2]
                    ,coef(summary(modelwindowF1F2H1H2))[2, 2],coef(summary(modelwindowF1F2C))[2, 2]
                    ,coef(summary(modelwindowF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowH1H2CM))[2, 2],coef(summary(modelwindowJF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowdivided))[2, 2]),
                  c(BIC(modelwindowM),BIC(modelwindowMJ),BIC(modelwindowF1F2),
                    BIC(modelwindowH1H2M),BIC(modelwindowCM),BIC(modelwindowJH1H2M),
                    BIC(modelwindowJF1F2),BIC(modelwindowJCM),BIC(modelwindowJF1F2H1H2),
                    BIC(modelwindowJH1H2CM),BIC(modelwindowJF1F2C),
                    BIC(modelwindowF1F2H1H2),BIC(modelwindowF1F2C),
                    BIC(modelwindowF1F2H1H2C),
                    BIC(modelwindowH1H2CM),BIC(modelwindowJF1F2H1H2C),
                    BIC(modelwindowdivided)))

### Window size using our data, no JCS scores (i.e. no status quo)
### Coefficient on main variable, Standard Error on main variable, and BIC

rownames(outtable)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtable19751988<-outtable[order(outtable[,3]),]
xtable(outtable19751988, caption="Window size using our data, no JCS scores (i.e. no status quo).  Time Period 1975-1988")


### Taking that same approach (window size using our data, no JCS scores) but using the 1989-2006 time period 

modelwindowM <-glm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1989 & judin$year<=2006,], 
                   family=binomial(link="logit"))

#robustM <- lrm(unsuccessful~as.numeric(windowM), data=judin[judin$year>=1989 & judin$year<=2006,])

modelwindowMJ <-glm(unsuccessful~as.numeric(windowMJ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelwindowF1F2 <-glm(unsuccessful~as.numeric(windowF1F2), data=judin[judin$year>=1989 & judin$year<=2006,], 
                      family=binomial(link="logit"))
modelwindowH1H2M <-glm(unsuccessful~as.numeric(windowH1H2M), data=judin[judin$year>=1989 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelwindowCM <-glm(unsuccessful~as.numeric(windowCM), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelwindowJH1H2M <-glm(unsuccessful~as.numeric(windowJH1H2M), data=judin[judin$year>=1989 & judin$year<=2006,], 
                        family=binomial(link="logit"))
modelwindowJF1F2 <-glm(unsuccessful~as.numeric(windowJF1F2), data=judin[judin$year>=1989 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelwindowJCM <-glm(unsuccessful~as.numeric(windowJCM), data=judin[judin$year>=1989 & judin$year<=2006,], 
                     family=binomial(link="logit"))
modelwindowJF1F2H1H2 <-glm(unsuccessful~as.numeric(windowJF1F2H1H2), data=judin[judin$year>=1989 & judin$year<=2006,], 
                           family=binomial(link="logit"))
modelwindowJH1H2CM <-glm(unsuccessful~as.numeric(windowJH1H2CM), data=judin[judin$year>=1989 & judin$year<=2006,], 
                         family=binomial(link="logit"))
modelwindowJF1F2C <-glm(unsuccessful~as.numeric(windowJF1F2C), data=judin[judin$year>=1989 & judin$year<=2006,], 
                        family=binomial(link="logit"))                  
modelwindowF1F2H1H2 <-glm(unsuccessful~as.numeric(windowF1F2H1H2), data=judin[judin$year>=1989 & judin$year<=2006,], 
                          family=binomial(link="logit"))
modelwindowF1F2C<-glm(unsuccessful~as.numeric(windowF1F2C), data=judin[judin$year>=1989 & judin$year<=2006,], 
                      family=binomial(link="logit"))
modelwindowF1F2H1H2C<-glm(unsuccessful~as.numeric(windowF1F2H1H2C), data=judin[judin$year>=1989 & judin$year<=2006,], 
                          family=binomial(link="logit"))
modelwindowH1H2CM<-glm(unsuccessful~as.numeric(windowH1H2CM), data=judin[judin$year>=1989 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelwindowJF1F2H1H2C<-glm(unsuccessful~as.numeric(windowJF1F2H1H2C), data=judin[judin$year>=1989 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelwindowdivided <- glm(unsuccessful~as.numeric(divided), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))


outtable19892006 <- cbind(c(coef(modelwindowM)[2], coef(modelwindowMJ)[2], coef(modelwindowF1F2)[2],
                    coef(modelwindowH1H2M)[2],coef(modelwindowCM)[2],coef(modelwindowJH1H2M)[2],
                    coef(modelwindowJF1F2)[2],coef(modelwindowJCM)[2],coef(modelwindowJF1F2H1H2)[2],
                    coef(modelwindowJH1H2CM)[2],coef(modelwindowJF1F2C)[2],
                    coef(modelwindowF1F2H1H2)[2],coef(modelwindowF1F2C)[2],                    
                    coef(modelwindowF1F2H1H2C)[2],coef(modelwindowH1H2CM)[2],coef(modelwindowJF1F2H1H2C)[2],
                    coef(modelwindowdivided)[2]), 
                  c(coef(summary(modelwindowM))[2, 2],coef(summary(modelwindowMJ))[2, 2],coef(summary(modelwindowF1F2))[2, 2],
                    coef(summary(modelwindowH1H2M))[2, 2],coef(summary(modelwindowCM))[2, 2],coef(summary(modelwindowJH1H2M))[2, 2],
                    coef(summary(modelwindowJF1F2))[2, 2],coef(summary(modelwindowJCM))[2, 2],coef(summary(modelwindowJF1F2H1H2))[2, 2],
                    coef(summary(modelwindowJH1H2CM))[2, 2],coef(summary(modelwindowJF1F2C))[2, 2]
                    ,coef(summary(modelwindowF1F2H1H2))[2, 2],coef(summary(modelwindowF1F2C))[2, 2]
                    ,coef(summary(modelwindowF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowH1H2CM))[2, 2],coef(summary(modelwindowJF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowdivided))[2, 2]),
                  c(BIC(modelwindowM),BIC(modelwindowMJ),BIC(modelwindowF1F2),
                    BIC(modelwindowH1H2M),BIC(modelwindowCM),BIC(modelwindowJH1H2M),
                    BIC(modelwindowJF1F2),BIC(modelwindowJCM),BIC(modelwindowJF1F2H1H2),
                    BIC(modelwindowJH1H2CM),BIC(modelwindowJF1F2C),
                    BIC(modelwindowF1F2H1H2),BIC(modelwindowF1F2C),
                    BIC(modelwindowF1F2H1H2C),
                    BIC(modelwindowH1H2CM),BIC(modelwindowJF1F2H1H2C),
                    BIC(modelwindowdivided)))

### Window size using our data, no JCS scores (i.e. no status quo)
### Coefficient on main variable, Standard Error on main variable, and BIC

rownames(outtable19892006)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtable19892006<-outtable19892006[order(outtable19892006[,3]),]
xtable(outtable19892006, caption="Window size using our data, no JCS scores (i.e. no status quo).  Time Period 1989-2006")




#The below code does robust clustered standard errors (very very close to) the way Stata
#does. We need to decide if that's what we want to replicate.
#robustpbmM <-lrm(failed~mediangridlock, data=pbm[pbm$cong>=94 & pbm$cong<=100 pbm$stateid!=55,],x=T,y=T)
#robcov(robustpbmM, cluster=pbm[pbm$cong>=94 & pbm$cong<=100 pbm$stateid!=55,]$cong)

## Replicating PBM 1975-2006 (their whole time period)
##  94th congress to 109th congress


modelpbmM <-glm(failed~mediangridlock, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                   family=binomial(link="logit"))
modelpbmMJ <-glm(failed~weakmajgridlock, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                family=binomial(link="logit"))
modelpbmF1F2 <-glm(failed~purefilibustzone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                 family=binomial(link="logit"))
modelpbmH1H2M <-glm(failed~purebluezone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                   family=binomial(link="logit"))
modelpbmCM <-glm(failed~committeezone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmJH1H2M <-glm(failed~majbluezone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                 family=binomial(link="logit"))
modelpbmJF1F2 <-glm(failed~majfilizone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmJCM <-glm(failed~commpartyzone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmJF1F2H1H2 <-glm(failed~majbluefili, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                  family=binomial(link="logit"))
modelpbmJH1H2CM <-glm(failed~commpartybluezone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                        family=binomial(link="logit"))
modelpbmJF1F2C <-glm(failed~commpartyfilizone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                      family=binomial(link="logit"))
modelpbmF1F2H1H2 <-glm(failed~bsfilibustzone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmF1F2C <-glm(failed~commfilizone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                       family=binomial(link="logit"))
modelpbmF1F2H1H2C <-glm(failed~commfilibluezone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmH1H2CM <-glm(failed~commbluezone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                        family=binomial(link="logit"))
modelpbmJF1F2H1H2C <-glm(failed~newfullmodelzone, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmdivided <-glm(failed~divided, data=pbm[pbm$cong>=94 & pbm$cong<=109 & pbm$stateid!=55,], 
                         family=binomial(link="logit"))


outtablepbm <- cbind(c(coef(modelpbmM)[2], coef(modelpbmMJ)[2], coef(modelpbmF1F2)[2],
                    coef(modelpbmH1H2M)[2],coef(modelpbmCM)[2],coef(modelpbmJH1H2M)[2],
                    coef(modelpbmJF1F2)[2],coef(modelpbmJCM)[2],coef(modelpbmJF1F2H1H2)[2],
                    coef(modelpbmJH1H2CM)[2],coef(modelpbmJF1F2C)[2],
                    coef(modelpbmF1F2H1H2)[2],coef(modelpbmF1F2C)[2],                    
                    coef(modelpbmF1F2H1H2C)[2],coef(modelpbmH1H2CM)[2],
                    coef(modelpbmJF1F2H1H2C)[2],coef(modelpbmdivided)[2]), 
                  c(coef(summary(modelpbmM))[2, 2],coef(summary(modelpbmMJ))[2, 2],coef(summary(modelpbmF1F2))[2, 2],
                    coef(summary(modelpbmH1H2M))[2, 2],coef(summary(modelpbmCM))[2, 2],coef(summary(modelpbmJH1H2M))[2, 2],
                    coef(summary(modelpbmJF1F2))[2, 2],coef(summary(modelpbmJCM))[2, 2],coef(summary(modelpbmJF1F2H1H2))[2, 2],
                    coef(summary(modelpbmJH1H2CM))[2, 2],coef(summary(modelpbmJF1F2C))[2, 2]
                    ,coef(summary(modelpbmF1F2H1H2))[2, 2],coef(summary(modelpbmF1F2C))[2, 2]
                    ,coef(summary(modelpbmF1F2H1H2C))[2, 2],
                    coef(summary(modelpbmH1H2CM))[2, 2],
                    coef(summary(modelpbmJF1F2H1H2C))[2, 2], coef(summary(modelpbmdivided))[2, 2]),
                  c(BIC(modelpbmM),BIC(modelpbmMJ),BIC(modelpbmF1F2),
                    BIC(modelpbmH1H2M),BIC(modelpbmCM),BIC(modelpbmJH1H2M),
                    BIC(modelpbmJF1F2),BIC(modelpbmJCM),BIC(modelpbmJF1F2H1H2),
                    BIC(modelpbmJH1H2CM),BIC(modelpbmJF1F2C)
                    ,BIC(modelpbmF1F2H1H2),BIC(modelpbmF1F2C)
                    ,BIC(modelpbmF1F2H1H2C),
                    BIC(modelpbmH1H2CM),BIC(modelpbmJF1F2H1H2C), BIC(modelpbmdivided)))

outtablepbm<-outtablepbm[order(outtablepbm[,3]),]
xtable(outtablepbm, caption="Our Replication of Binder et al.  (These have been hand cross-checked against their article and linear transformation of their results.  Ordering same)")

## Replicating PBM 1975-1988
##  94th congress to 100th congress

modelpbmM <-glm(failed~mediangridlock, data=pbm[pbm$cong>=94 & pbm$cong<=100 & pbm$stateid!=55,], 
                   family=binomial(link="logit"))
modelpbmMJ <-glm(failed~weakmajgridlock, data=pbm[pbm$cong>=94 & pbm$cong<=100 & pbm$stateid!=55,], 
                family=binomial(link="logit"))
modelpbmF1F2 <-glm(failed~purefilibustzone, data=pbm[pbm$cong>=94 & pbm$cong<=100 &pbm$stateid!=55,], 
                 family=binomial(link="logit"))
modelpbmH1H2M <-glm(failed~purebluezone, data=pbm[pbm$cong>=94 & pbm$cong<=100 &pbm$stateid!=55,], 
                   family=binomial(link="logit"))
modelpbmCM <-glm(failed~committeezone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmJH1H2M <-glm(failed~majbluezone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                 family=binomial(link="logit"))
modelpbmJF1F2 <-glm(failed~majfilizone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmJCM <-glm(failed~commpartyzone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmJF1F2H1H2 <-glm(failed~majbluefili, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                  family=binomial(link="logit"))
modelpbmJH1H2CM <-glm(failed~commpartybluezone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                        family=binomial(link="logit"))
modelpbmJF1F2C <-glm(failed~commpartyfilizone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                      family=binomial(link="logit"))
modelpbmF1F2H1H2 <-glm(failed~bsfilibustzone, data=pbm[pbm$cong>=94 & pbm$cong<=100 &pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmF1F2C <-glm(failed~commfilizone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                       family=binomial(link="logit"))
modelpbmF1F2H1H2C <-glm(failed~commfilibluezone, data=pbm[pbm$cong>=94 & pbm$cong<=100& pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmH1H2CM <-glm(failed~commbluezone, data=pbm[pbm$cong>=94 & pbm$cong<=100 &pbm$stateid!=55,], 
                        family=binomial(link="logit"))
modelpbmJF1F2H1H2C <-glm(failed~newfullmodelzone, data=pbm[pbm$cong>=94 & pbm$cong<=100 &pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmdivided <-glm(failed~divided, data=pbm[pbm$cong>=94 & pbm$cong<=100 &pbm$stateid!=55,], 
                         family=binomial(link="logit"))


outtablepbm <- cbind(c(coef(modelpbmM)[2], coef(modelpbmMJ)[2], coef(modelpbmF1F2)[2],
                    coef(modelpbmH1H2M)[2],coef(modelpbmCM)[2],coef(modelpbmJH1H2M)[2],
                    coef(modelpbmJF1F2)[2],coef(modelpbmJCM)[2],coef(modelpbmJF1F2H1H2)[2],
                    coef(modelpbmJH1H2CM)[2],coef(modelpbmJF1F2C)[2],
                    coef(modelpbmF1F2H1H2)[2],coef(modelpbmF1F2C)[2],                    
                    coef(modelpbmF1F2H1H2C)[2],coef(modelpbmH1H2CM)[2],
                    coef(modelpbmJF1F2H1H2C)[2],coef(modelpbmdivided)[2]), 
                  c(coef(summary(modelpbmM))[2, 2],coef(summary(modelpbmMJ))[2, 2],coef(summary(modelpbmF1F2))[2, 2],
                    coef(summary(modelpbmH1H2M))[2, 2],coef(summary(modelpbmCM))[2, 2],coef(summary(modelpbmJH1H2M))[2, 2],
                    coef(summary(modelpbmJF1F2))[2, 2],coef(summary(modelpbmJCM))[2, 2],coef(summary(modelpbmJF1F2H1H2))[2, 2],
                    coef(summary(modelpbmJH1H2CM))[2, 2],coef(summary(modelpbmJF1F2C))[2, 2]
                    ,coef(summary(modelpbmF1F2H1H2))[2, 2],coef(summary(modelpbmF1F2C))[2, 2]
                    ,coef(summary(modelpbmF1F2H1H2C))[2, 2],
                    coef(summary(modelpbmH1H2CM))[2, 2],
                    coef(summary(modelpbmJF1F2H1H2C))[2, 2], coef(summary(modelpbmdivided))[2, 2]),
                  c(BIC(modelpbmM),BIC(modelpbmMJ),BIC(modelpbmF1F2),
                    BIC(modelpbmH1H2M),BIC(modelpbmCM),BIC(modelpbmJH1H2M),
                    BIC(modelpbmJF1F2),BIC(modelpbmJCM),BIC(modelpbmJF1F2H1H2),
                    BIC(modelpbmJH1H2CM),BIC(modelpbmJF1F2C)
                    ,BIC(modelpbmF1F2H1H2),BIC(modelpbmF1F2C)
                    ,BIC(modelpbmF1F2H1H2C),
                    BIC(modelpbmH1H2CM),BIC(modelpbmJF1F2H1H2C), BIC(modelpbmdivided)))

outtablepbm94100<-outtablepbm[order(outtablepbm[,3]),]
xtable(outtablepbm94100, caption="Our Replication of Binder et al. 1975-1988 (94th to 100th congresses)")

## Replicating PBM 1989-2006
##  101th congress to 109th congress

modelpbmM <-glm(failed~mediangridlock, data=pbm[pbm$cong>=101 & pbm$cong<=109 & pbm$stateid!=55,], 
                   family=binomial(link="logit"))
modelpbmMJ <-glm(failed~weakmajgridlock, data=pbm[pbm$cong>=101 & pbm$cong<=109 & pbm$stateid!=55,], 
                family=binomial(link="logit"))
modelpbmF1F2 <-glm(failed~purefilibustzone, data=pbm[pbm$cong>=101 & pbm$cong<=109 &pbm$stateid!=55,], 
                 family=binomial(link="logit"))
modelpbmH1H2M <-glm(failed~purebluezone, data=pbm[pbm$cong>=101 & pbm$cong<=109 &pbm$stateid!=55,], 
                   family=binomial(link="logit"))
modelpbmCM <-glm(failed~committeezone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmJH1H2M <-glm(failed~majbluezone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                 family=binomial(link="logit"))
modelpbmJF1F2 <-glm(failed~majfilizone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmJCM <-glm(failed~commpartyzone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmJF1F2H1H2 <-glm(failed~majbluefili, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                  family=binomial(link="logit"))
modelpbmJH1H2CM <-glm(failed~commpartybluezone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                        family=binomial(link="logit"))
modelpbmJF1F2C <-glm(failed~commpartyfilizone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                      family=binomial(link="logit"))
modelpbmF1F2H1H2 <-glm(failed~bsfilibustzone, data=pbm[pbm$cong>=101 & pbm$cong<=109 &pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmF1F2C <-glm(failed~commfilizone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                       family=binomial(link="logit"))
modelpbmF1F2H1H2C <-glm(failed~commfilibluezone, data=pbm[pbm$cong>=101 & pbm$cong<=109& pbm$stateid!=55,], 
                    family=binomial(link="logit"))
modelpbmH1H2CM <-glm(failed~commbluezone, data=pbm[pbm$cong>=101 & pbm$cong<=109 &pbm$stateid!=55,], 
                        family=binomial(link="logit"))
modelpbmJF1F2H1H2C <-glm(failed~newfullmodelzone, data=pbm[pbm$cong>=101 & pbm$cong<=109 &pbm$stateid!=55,], 
                     family=binomial(link="logit"))
modelpbmdivided <-glm(failed~divided, data=pbm[pbm$cong>=101 & pbm$cong<=109 &pbm$stateid!=55,], 
                         family=binomial(link="logit"))


outtablepbm <- cbind(c(coef(modelpbmM)[2], coef(modelpbmMJ)[2], coef(modelpbmF1F2)[2],
                    coef(modelpbmH1H2M)[2],coef(modelpbmCM)[2],coef(modelpbmJH1H2M)[2],
                    coef(modelpbmJF1F2)[2],coef(modelpbmJCM)[2],coef(modelpbmJF1F2H1H2)[2],
                    coef(modelpbmJH1H2CM)[2],coef(modelpbmJF1F2C)[2],
                    coef(modelpbmF1F2H1H2)[2],coef(modelpbmF1F2C)[2],                    
                    coef(modelpbmF1F2H1H2C)[2],coef(modelpbmH1H2CM)[2],
                    coef(modelpbmJF1F2H1H2C)[2],coef(modelpbmdivided)[2]), 
                  c(coef(summary(modelpbmM))[2, 2],coef(summary(modelpbmMJ))[2, 2],coef(summary(modelpbmF1F2))[2, 2],
                    coef(summary(modelpbmH1H2M))[2, 2],coef(summary(modelpbmCM))[2, 2],coef(summary(modelpbmJH1H2M))[2, 2],
                    coef(summary(modelpbmJF1F2))[2, 2],coef(summary(modelpbmJCM))[2, 2],coef(summary(modelpbmJF1F2H1H2))[2, 2],
                    coef(summary(modelpbmJH1H2CM))[2, 2],coef(summary(modelpbmJF1F2C))[2, 2]
                    ,coef(summary(modelpbmF1F2H1H2))[2, 2],coef(summary(modelpbmF1F2C))[2, 2]
                    ,coef(summary(modelpbmF1F2H1H2C))[2, 2],
                    coef(summary(modelpbmH1H2CM))[2, 2],
                    coef(summary(modelpbmJF1F2H1H2C))[2, 2], coef(summary(modelpbmdivided))[2, 2]),
                  c(BIC(modelpbmM),BIC(modelpbmMJ),BIC(modelpbmF1F2),
                    BIC(modelpbmH1H2M),BIC(modelpbmCM),BIC(modelpbmJH1H2M),
                    BIC(modelpbmJF1F2),BIC(modelpbmJCM),BIC(modelpbmJF1F2H1H2),
                    BIC(modelpbmJH1H2CM),BIC(modelpbmJF1F2C)
                    ,BIC(modelpbmF1F2H1H2),BIC(modelpbmF1F2C)
                    ,BIC(modelpbmF1F2H1H2C),
                    BIC(modelpbmH1H2CM),BIC(modelpbmJF1F2H1H2C), BIC(modelpbmdivided)))

outtablepbm101109<-outtablepbm[order(outtablepbm[,3]),]
xtable(outtablepbm101109, caption="Our Replication of Binder et al. 1989-2006 (101th to 109th congresses)")



#Next we do the same thing, except instead of using window size, we use whether or not
#the status quo JCS median is inside or outside of the window.

#First we use this loop to get the right and left pivot for each window.
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
  judin$maxJF1F2H1H2C[i] <- max(judin$presnom[i],judin$gridlockComm[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
  judin$minJF1F2H1H2C[i] <- min(judin$presnom[i],judin$gridlockComm[i],judin$gridlockBS1[i],judin$gridlockBS2[i],judin$gridlockMajMed[i],judin$gridlockLowFili[i],judin$gridlockHighFili[i],na.rm=T)
}

#The create variables for each window type coded 0 for outside the window and 1 for inside

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
judin$inJF1F2H1H2C <- 0
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
judin$inJF1F2H1H2C[judin$jcsmedian<=judin$maxJF1F2H1H2C & judin$jcsmedian>=judin$minJF1F2H1H2C] <- 1
judin$inFiliNoPres[judin$jcsmedian<=judin$gridlockHighFili & judin$jcsmedian>=judin$gridlockLowFili] <- 1
#The pure filibuster model (F1F2) allows the president's party to reject his nominee
#We alter that as below
judin$inFili <- 0
judin$inFili[as.numeric(judin$jcsmedian)<=judin$gridlock60 & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100"& judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$inFili[as.numeric(judin$jcsmedian)>=judin$gridlock40 & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200"& judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$inFili[as.numeric(judin$jcsmedian)<=judin$gridlock67 & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100"& judin$nomdatenum<5492] <- 1
judin$inFili[as.numeric(judin$jcsmedian)>=judin$gridlock33 & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200"& judin$nomdatenum<5492] <- 1
judin$inFili[as.numeric(judin$jcsmedian)<=judin$gridlockMed & as.numeric(judin$jcsmedian)>=judin$presnom & judin$presparty=="100" & judin$nomdatenum>=19382] <- 1
judin$inFili[as.numeric(judin$jcsmedian)>=judin$gridlockMed & as.numeric(judin$jcsmedian)<=judin$presnom & judin$presparty=="200" & judin$nomdatenum>=19382] <- 1

#recoding aba variable
judin$WQ <- NA
judin$WQ[judin$aba==3 |judin$aba==4 |judin$aba==5 |judin$aba==7] <- 0
judin$WQ[judin$aba==1 |judin$aba==2] <- 1

#Fit logit models using in-window vairables
modelinM <-glm(unsuccessful~as.numeric(inM)+as.numeric(WQ), data=judin, 
               family=binomial(link="logit"))
modelinMJ <-glm(unsuccessful~as.numeric(inMJ)+as.numeric(WQ), data=judin, 
                family=binomial(link="logit"))
modelinF1F2 <-glm(unsuccessful~as.numeric(inF1F2)+as.numeric(WQ), data=judin, 
                  family=binomial(link="logit"))
modelinFili<-glm(unsuccessful~as.numeric(inFili)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelinH1H2M <-glm(unsuccessful~as.numeric(inH1H2M)+as.numeric(WQ), data=judin, 
                   family=binomial(link="logit"))
modelinCM <-glm(unsuccessful~as.numeric(inCM)+as.numeric(WQ), data=judin, 
                family=binomial(link="logit"))
modelinJH1H2M <-glm(unsuccessful~as.numeric(inJH1H2M)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelinJF1F2 <-glm(unsuccessful~as.numeric(inJF1F2)+as.numeric(WQ), data=judin, 
                   family=binomial(link="logit"))
modelinJCM <-glm(unsuccessful~as.numeric(inJCM)+as.numeric(WQ), data=judin, 
                 family=binomial(link="logit"))
modelinJF1F2H1H2 <-glm(unsuccessful~as.numeric(inJF1F2H1H2)+as.numeric(WQ), data=judin, 
                       family=binomial(link="logit"))
modelinJH1H2CM <-glm(unsuccessful~as.numeric(inJH1H2CM)+as.numeric(WQ), data=judin, 
                     family=binomial(link="logit"))
modelinJF1F2C <-glm(unsuccessful~as.numeric(inJF1F2C)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelinF1F2H1H2 <-glm(unsuccessful~as.numeric(inF1F2H1H2)+as.numeric(WQ), data=judin, 
                      family=binomial(link="logit"))
modelinF1F2C <-glm(unsuccessful~as.numeric(inF1F2C)+as.numeric(WQ), data=judin, 
                   family=binomial(link="logit"))
modelinF1F2H1H2C <-glm(unsuccessful~as.numeric(inF1F2H1H2C)+as.numeric(WQ), data=judin, 
                       family=binomial(link="logit"))
modelinH1H2CM <-glm(unsuccessful~as.numeric(inH1H2CM)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelinJF1F2H1H2C <-glm(unsuccessful~as.numeric(inJF1F2H1H2C)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelinFiliNoPres <-glm(unsuccessful~as.numeric(inFiliNoPres)+as.numeric(WQ), data=judin, 
                        family=binomial(link="logit"))
modelindivided <-glm(unsuccessful~as.numeric(divided)+as.numeric(WQ), data=judin, 
                        family=binomial(link="logit"))

outtablein <- cbind(c(coef(modelinM)[2], coef(modelinMJ)[2], coef(modelinF1F2)[2],
                       coef(modelinH1H2M)[2],coef(modelinCM)[2],coef(modelinJH1H2M)[2],
                       coef(modelinJF1F2)[2],coef(modelinJCM)[2],coef(modelinJF1F2H1H2)[2],
                       coef(modelinJH1H2CM)[2],coef(modelinJF1F2C)[2],
                       coef(modelinF1F2H1H2)[2],coef(modelinF1F2C)[2],                    
                       coef(modelinF1F2H1H2C)[2],coef(modelinH1H2CM)[2],coef(modelinJF1F2H1H2C)[2],
                       coef(modelindivided)[2]), 
                     c(coef(summary(modelinM))[2, 2],coef(summary(modelinMJ))[2, 2],coef(summary(modelinF1F2))[2, 2],
                       coef(summary(modelinH1H2M))[2, 2],coef(summary(modelinCM))[2, 2],coef(summary(modelinJH1H2M))[2, 2],
                       coef(summary(modelinJF1F2))[2, 2],coef(summary(modelinJCM))[2, 2],coef(summary(modelinJF1F2H1H2))[2, 2],
                       coef(summary(modelinJH1H2CM))[2, 2],coef(summary(modelinJF1F2C))[2, 2]
                       ,coef(summary(modelinF1F2H1H2))[2, 2],coef(summary(modelinF1F2C))[2, 2]
                       ,coef(summary(modelinF1F2H1H2C))[2, 2],
                       coef(summary(modelinH1H2CM))[2, 2],coef(summary(modelinJF1F2H1H2C))[2, 2],
                       coef(summary(modelindivided))[2, 2]),
                    
#                   c(coef(summary(modelinM))[2, 4],coef(summary(modelinMJ))[2, 4],coef(summary(modelinF1F2))[2, 4],
#                     coef(summary(modelinH1H2M))[2, 4],coef(summary(modelinCM))[2, 4],coef(summary(modelinJH1H2M))[2, 4],
#                     coef(summary(modelinJF1F2))[2, 4],coef(summary(modelinJCM))[2, 4],coef(summary(modelinJF1F2H1H2))[2, 4],
#                     coef(summary(modelinJH1H2CM))[2, 4],coef(summary(modelinJF1F2C))[2, 4],coef(summary(modelinF1F2H1H2C))[2, 4],
#                     coef(summary(modelinH1H2CM))[2, 4],coef(summary(modelinFili))[2, 4]),
#                    commented out portion currently missing 3 models
                    
                     c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                       BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                       BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                       BIC(modelinJH1H2CM),BIC(modelinJF1F2C)
                       ,BIC(modelinF1F2H1H2),BIC(modelinF1F2C)
                       ,BIC(modelinF1F2H1H2C),
                       BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                       BIC(modelindivided)))

rownames(outtablein)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablein<-outtablein[order(outtablein[,3]),]
#This is our main results table in the paper.
xtable(outtablein, caption="Our Model (in-window) with the Status Quo (JCS), Correct Senators, and Entire Time Period")


## On the 1975-2006
#Fit logit models using in-window vairables
modelinM <-glm(unsuccessful~as.numeric(inM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
               family=binomial(link="logit"))
modelinMJ <-glm(unsuccessful~as.numeric(inMJ)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                family=binomial(link="logit"))
modelinF1F2 <-glm(unsuccessful~as.numeric(inF1F2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                  family=binomial(link="logit"))
modelinFili<-glm(unsuccessful~as.numeric(inFili)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                 family=binomial(link="logit"))
modelinH1H2M <-glm(unsuccessful~as.numeric(inH1H2M)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelinCM <-glm(unsuccessful~as.numeric(inCM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                family=binomial(link="logit"))
modelinJH1H2M <-glm(unsuccessful~as.numeric(inJH1H2M)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinJF1F2 <-glm(unsuccessful~as.numeric(inJF1F2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelinJCM <-glm(unsuccessful~as.numeric(inJCM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                 family=binomial(link="logit"))
modelinJF1F2H1H2 <-glm(unsuccessful~as.numeric(inJF1F2H1H2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelinJH1H2CM <-glm(unsuccessful~as.numeric(inJH1H2CM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                     family=binomial(link="logit"))
modelinJF1F2C <-glm(unsuccessful~as.numeric(inJF1F2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinF1F2H1H2 <-glm(unsuccessful~as.numeric(inF1F2H1H2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                      family=binomial(link="logit"))
modelinF1F2C <-glm(unsuccessful~as.numeric(inF1F2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelinF1F2H1H2C <-glm(unsuccessful~as.numeric(inF1F2H1H2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelinH1H2CM <-glm(unsuccessful~as.numeric(inH1H2CM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinJF1F2H1H2C <-glm(unsuccessful~as.numeric(inJF1F2H1H2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                        family=binomial(link="logit"))
modelinFiliNoPres <-glm(unsuccessful~as.numeric(inFiliNoPres)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                        family=binomial(link="logit"))
modelindivided <-glm(unsuccessful~as.numeric(divided)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=2006,], 
                     family=binomial(link="logit"))

outtablein <- cbind(c(coef(modelinM)[2], coef(modelinMJ)[2], coef(modelinF1F2)[2],
                      coef(modelinH1H2M)[2],coef(modelinCM)[2],coef(modelinJH1H2M)[2],
                      coef(modelinJF1F2)[2],coef(modelinJCM)[2],coef(modelinJF1F2H1H2)[2],
                      coef(modelinJH1H2CM)[2],coef(modelinJF1F2C)[2],
                      coef(modelinF1F2H1H2)[2],coef(modelinF1F2C)[2],                    
                      coef(modelinF1F2H1H2C)[2],coef(modelinH1H2CM)[2],coef(modelinJF1F2H1H2C)[2],
                      coef(modelindivided)[2]), 
                    c(coef(summary(modelinM))[2, 2],coef(summary(modelinMJ))[2, 2],coef(summary(modelinF1F2))[2, 2],
                      coef(summary(modelinH1H2M))[2, 2],coef(summary(modelinCM))[2, 2],coef(summary(modelinJH1H2M))[2, 2],
                      coef(summary(modelinJF1F2))[2, 2],coef(summary(modelinJCM))[2, 2],coef(summary(modelinJF1F2H1H2))[2, 2],
                      coef(summary(modelinJH1H2CM))[2, 2],coef(summary(modelinJF1F2C))[2, 2]
                      ,coef(summary(modelinF1F2H1H2))[2, 2],coef(summary(modelinF1F2C))[2, 2]
                      ,coef(summary(modelinF1F2H1H2C))[2, 2],
                      coef(summary(modelinH1H2CM))[2, 2],coef(summary(modelinJF1F2H1H2C))[2, 2],
                      coef(summary(modelindivided))[2, 2]),
                    
                    #                   c(coef(summary(modelinM))[2, 4],coef(summary(modelinMJ))[2, 4],coef(summary(modelinF1F2))[2, 4],
                    #                     coef(summary(modelinH1H2M))[2, 4],coef(summary(modelinCM))[2, 4],coef(summary(modelinJH1H2M))[2, 4],
                    #                     coef(summary(modelinJF1F2))[2, 4],coef(summary(modelinJCM))[2, 4],coef(summary(modelinJF1F2H1H2))[2, 4],
                    #                     coef(summary(modelinJH1H2CM))[2, 4],coef(summary(modelinJF1F2C))[2, 4],coef(summary(modelinF1F2H1H2C))[2, 4],
                    #                     coef(summary(modelinH1H2CM))[2, 4],coef(summary(modelinFili))[2, 4]),
                    #                    commented out portion currently missing 3 models
                    
                    c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                      BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                      BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                      BIC(modelinJH1H2CM),BIC(modelinJF1F2C)
                      ,BIC(modelinF1F2H1H2),BIC(modelinF1F2C)
                      ,BIC(modelinF1F2H1H2C),
                      BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                      BIC(modelindivided)))

rownames(outtablein)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablein19752006<-outtablein[order(outtablein[,3]),]
xtable(outtablein19752006, caption="Our Model (in-window) with the Status Quo (JCS), Correct Senators, and 1975-2006")



#Next we do the same thing, except instead of using window size, we use whether or not
#the status quo JCS median is inside or outside of the window.
## On the 1975-1988
#Fit logit models using in-window vairables
modelinM <-glm(unsuccessful~as.numeric(inM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
               family=binomial(link="logit"))
modelinMJ <-glm(unsuccessful~as.numeric(inMJ)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                family=binomial(link="logit"))
modelinF1F2 <-glm(unsuccessful~as.numeric(inF1F2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                  family=binomial(link="logit"))
modelinFili<-glm(unsuccessful~as.numeric(inFili)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelinH1H2M <-glm(unsuccessful~as.numeric(inH1H2M)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                   family=binomial(link="logit"))
modelinCM <-glm(unsuccessful~as.numeric(inCM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                family=binomial(link="logit"))
modelinJH1H2M <-glm(unsuccessful~as.numeric(inJH1H2M)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelinJF1F2 <-glm(unsuccessful~as.numeric(inJF1F2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                   family=binomial(link="logit"))
modelinJCM <-glm(unsuccessful~as.numeric(inJCM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                 family=binomial(link="logit"))
modelinJF1F2H1H2 <-glm(unsuccessful~as.numeric(inJF1F2H1H2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                       family=binomial(link="logit"))
modelinJH1H2CM <-glm(unsuccessful~as.numeric(inJH1H2CM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                     family=binomial(link="logit"))
modelinJF1F2C <-glm(unsuccessful~as.numeric(inJF1F2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelinF1F2H1H2 <-glm(unsuccessful~as.numeric(inF1F2H1H2)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                      family=binomial(link="logit"))
modelinF1F2C <-glm(unsuccessful~as.numeric(inF1F2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                   family=binomial(link="logit"))
modelinF1F2H1H2C <-glm(unsuccessful~as.numeric(inF1F2H1H2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                       family=binomial(link="logit"))
modelinH1H2CM <-glm(unsuccessful~as.numeric(inH1H2CM)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelinJF1F2H1H2C <-glm(unsuccessful~as.numeric(inJF1F2H1H2C)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                    family=binomial(link="logit"))
modelinFiliNoPres <-glm(unsuccessful~as.numeric(inFiliNoPres)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                        family=binomial(link="logit"))
modelindivided <-glm(unsuccessful~as.numeric(divided)+as.numeric(WQ), data=judin[judin$year>=1975 & judin$year<=1988,], 
                        family=binomial(link="logit"))

outtablein <- cbind(c(coef(modelinM)[2], coef(modelinMJ)[2], coef(modelinF1F2)[2],
                       coef(modelinH1H2M)[2],coef(modelinCM)[2],coef(modelinJH1H2M)[2],
                       coef(modelinJF1F2)[2],coef(modelinJCM)[2],coef(modelinJF1F2H1H2)[2],
                       coef(modelinJH1H2CM)[2],coef(modelinJF1F2C)[2],
                       coef(modelinF1F2H1H2)[2],coef(modelinF1F2C)[2],                    
                       coef(modelinF1F2H1H2C)[2],coef(modelinH1H2CM)[2],coef(modelinJF1F2H1H2C)[2],
                       coef(modelindivided)[2]), 
                     c(coef(summary(modelinM))[2, 2],coef(summary(modelinMJ))[2, 2],coef(summary(modelinF1F2))[2, 2],
                       coef(summary(modelinH1H2M))[2, 2],coef(summary(modelinCM))[2, 2],coef(summary(modelinJH1H2M))[2, 2],
                       coef(summary(modelinJF1F2))[2, 2],coef(summary(modelinJCM))[2, 2],coef(summary(modelinJF1F2H1H2))[2, 2],
                       coef(summary(modelinJH1H2CM))[2, 2],coef(summary(modelinJF1F2C))[2, 2]
                       ,coef(summary(modelinF1F2H1H2))[2, 2],coef(summary(modelinF1F2C))[2, 2]
                       ,coef(summary(modelinF1F2H1H2C))[2, 2],
                       coef(summary(modelinH1H2CM))[2, 2],coef(summary(modelinJF1F2H1H2C))[2, 2],
                       coef(summary(modelindivided))[2, 2]),
                    
#                   c(coef(summary(modelinM))[2, 4],coef(summary(modelinMJ))[2, 4],coef(summary(modelinF1F2))[2, 4],
#                     coef(summary(modelinH1H2M))[2, 4],coef(summary(modelinCM))[2, 4],coef(summary(modelinJH1H2M))[2, 4],
#                     coef(summary(modelinJF1F2))[2, 4],coef(summary(modelinJCM))[2, 4],coef(summary(modelinJF1F2H1H2))[2, 4],
#                     coef(summary(modelinJH1H2CM))[2, 4],coef(summary(modelinJF1F2C))[2, 4],coef(summary(modelinF1F2H1H2C))[2, 4],
#                     coef(summary(modelinH1H2CM))[2, 4],coef(summary(modelinFili))[2, 4]),
#                    commented out portion currently missing 3 models
                    
                     c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                       BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                       BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                       BIC(modelinJH1H2CM),BIC(modelinJF1F2C)
                       ,BIC(modelinF1F2H1H2),BIC(modelinF1F2C)
                       ,BIC(modelinF1F2H1H2C),
                       BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                       BIC(modelindivided)))

rownames(outtablein)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablein19751988<-outtablein[order(outtablein[,3]),]
xtable(outtablein19751988, caption="Our Model (in-window) with the Status Quo (JCS), Correct Senators, and 1975-1988")

#Next we do the same thing, except instead of using window size, we use whether or not
#the status quo JCS median is inside or outside of the window.
## On the 1989-2006
#Fit logit models using in-window vairables
modelinM <-glm(unsuccessful~as.numeric(inM)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
               family=binomial(link="logit"))
modelinMJ <-glm(unsuccessful~as.numeric(inMJ)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                family=binomial(link="logit"))
modelinF1F2 <-glm(unsuccessful~as.numeric(inF1F2)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                  family=binomial(link="logit"))
modelinFili<-glm(unsuccessful~as.numeric(inFili)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinH1H2M <-glm(unsuccessful~as.numeric(inH1H2M)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelinCM <-glm(unsuccessful~as.numeric(inCM)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                family=binomial(link="logit"))
modelinJH1H2M <-glm(unsuccessful~as.numeric(inJH1H2M)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinJF1F2 <-glm(unsuccessful~as.numeric(inJF1F2)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelinJCM <-glm(unsuccessful~as.numeric(inJCM)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                 family=binomial(link="logit"))
modelinJF1F2H1H2 <-glm(unsuccessful~as.numeric(inJF1F2H1H2)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelinJH1H2CM <-glm(unsuccessful~as.numeric(inJH1H2CM)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                     family=binomial(link="logit"))
modelinJF1F2C <-glm(unsuccessful~as.numeric(inJF1F2C)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinF1F2H1H2 <-glm(unsuccessful~as.numeric(inF1F2H1H2)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                      family=binomial(link="logit"))
modelinF1F2C <-glm(unsuccessful~as.numeric(inF1F2C)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                   family=binomial(link="logit"))
modelinF1F2H1H2C <-glm(unsuccessful~as.numeric(inF1F2H1H2C)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                       family=binomial(link="logit"))
modelinH1H2CM <-glm(unsuccessful~as.numeric(inH1H2CM)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinJF1F2H1H2C <-glm(unsuccessful~as.numeric(inJF1F2H1H2C)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                    family=binomial(link="logit"))
modelinFiliNoPres <-glm(unsuccessful~as.numeric(inFiliNoPres)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                        family=binomial(link="logit"))
modelindivided <-glm(unsuccessful~as.numeric(divided)+as.numeric(WQ), data=judin[judin$year>=1989 & judin$year<=2006,], 
                        family=binomial(link="logit"))

outtablein <- cbind(c(coef(modelinM)[2], coef(modelinMJ)[2], coef(modelinF1F2)[2],
                       coef(modelinH1H2M)[2],coef(modelinCM)[2],coef(modelinJH1H2M)[2],
                       coef(modelinJF1F2)[2],coef(modelinJCM)[2],coef(modelinJF1F2H1H2)[2],
                       coef(modelinJH1H2CM)[2],coef(modelinJF1F2C)[2],
                       coef(modelinF1F2H1H2)[2],coef(modelinF1F2C)[2],                    
                       coef(modelinF1F2H1H2C)[2],coef(modelinH1H2CM)[2],coef(modelinJF1F2H1H2C)[2],
                       coef(modelindivided)[2]), 
                     c(coef(summary(modelinM))[2, 2],coef(summary(modelinMJ))[2, 2],coef(summary(modelinF1F2))[2, 2],
                       coef(summary(modelinH1H2M))[2, 2],coef(summary(modelinCM))[2, 2],coef(summary(modelinJH1H2M))[2, 2],
                       coef(summary(modelinJF1F2))[2, 2],coef(summary(modelinJCM))[2, 2],coef(summary(modelinJF1F2H1H2))[2, 2],
                       coef(summary(modelinJH1H2CM))[2, 2],coef(summary(modelinJF1F2C))[2, 2]
                       ,coef(summary(modelinF1F2H1H2))[2, 2],coef(summary(modelinF1F2C))[2, 2]
                       ,coef(summary(modelinF1F2H1H2C))[2, 2],
                       coef(summary(modelinH1H2CM))[2, 2],coef(summary(modelinJF1F2H1H2C))[2, 2],
                       coef(summary(modelindivided))[2, 2]),
                    
#                   c(coef(summary(modelinM))[2, 4],coef(summary(modelinMJ))[2, 4],coef(summary(modelinF1F2))[2, 4],
#                     coef(summary(modelinH1H2M))[2, 4],coef(summary(modelinCM))[2, 4],coef(summary(modelinJH1H2M))[2, 4],
#                     coef(summary(modelinJF1F2))[2, 4],coef(summary(modelinJCM))[2, 4],coef(summary(modelinJF1F2H1H2))[2, 4],
#                     coef(summary(modelinJH1H2CM))[2, 4],coef(summary(modelinJF1F2C))[2, 4],coef(summary(modelinF1F2H1H2C))[2, 4],
#                     coef(summary(modelinH1H2CM))[2, 4],coef(summary(modelinFili))[2, 4]),
#                    commented out portion currently missing 3 models
                    
                     c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                       BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                       BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                       BIC(modelinJH1H2CM),BIC(modelinJF1F2C)
                       ,BIC(modelinF1F2H1H2),BIC(modelinF1F2C)
                       ,BIC(modelinF1F2H1H2C),
                       BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                       BIC(modelindivided)))

rownames(outtablein)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablein19892006<-outtablein[order(outtablein[,3]),]
xtable(outtablein19892006, caption="Our Model (in-window) with the Status Quo (JCS), Correct Senators, and 1989-2006")



#Marginal effects:
inv.logit(coef(modelinMJ) %*% c(1,1,1))-inv.logit(coef(modelinMJ) %*% c(1,0,1))
inv.logit(coef(modelinMJ) %*% c(1,1,0))-inv.logit(coef(modelinMJ) %*% c(1,1,1))



#The following uses a moving time frame to rerun the theoretically important models
#in time subsets. It the lists the ranks in BIC
BICs <-c()
ranks <- cbind(c())
for (i in 1940:2010) {
  j <- i+10
  rankH1H2CM <- glm(unsuccessful~as.numeric(inH1H2CM)+
                      as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                    family=binomial(link="logit"))
  rankMJ <- glm(unsuccessful~as.numeric(inMJ)+
                  as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                family=binomial(link="logit"))
  rankFili <- glm(unsuccessful~as.numeric(inFili)+
                       as.numeric(WQ), data=judin[judin$year>=i & judin$year<j,], 
                     family=binomial(link="logit"))
  BICs <- c(BIC(rankH1H2CM),BIC(rankMJ),BIC(rankFili))
  ranks <- cbind(ranks,c(i+5,rank(BICs)))
}
#Two different ways of plotting the ranks. The second one is more attractive
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
           BIC(modelinF1F2H1H2),BIC(modelinF1F2C),
           BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
           BIC(modelinFili),BIC(modelindivided))
bicStarttoFili <- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                    BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                    BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                    BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
                    BIC(modelinF1F2H1H2),BIC(modelinF1F2C),
                    BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                    BIC(modelinFili),BIC(modelindivided))
bicFilitoBork <- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                   BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                   BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                   BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
                   BIC(modelinF1F2H1H2),BIC(modelinF1F2C),
                   BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                   BIC(modelinFili),BIC(modelindivided))
bicBorktoFili2 <- c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                    BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                    BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                    BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
                    BIC(modelinF1F2H1H2),BIC(modelinF1F2C),
                    BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                    BIC(modelinFili),BIC(modelindivided))
bicFili2toNow <-c(BIC(modelinM),BIC(modelinMJ),BIC(modelinF1F2),
                  BIC(modelinH1H2M),BIC(modelinCM),BIC(modelinJH1H2M),
                  BIC(modelinJF1F2),BIC(modelinJCM),BIC(modelinJF1F2H1H2),
                  BIC(modelinJH1H2CM),BIC(modelinJF1F2C),BIC(modelinF1F2H1H2C),
                  BIC(modelinF1F2H1H2),BIC(modelinF1F2C),
                  BIC(modelinH1H2CM),BIC(modelinJF1F2H1H2C),
                  BIC(modelinFili),BIC(modelindivided))

xtable(cbind(bicStarttoFili,bicFilitoBork,bicBorktoFili2,bicFili2toNow))



#Strategic selection issue: This will look at the change in the proportion well qualified
#over midterm elections, against change in the gridlock window
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

 ### Whether Presidents are doing strategic selection
 
xtable(summary(lm(submeanaba$WQchange~submeanaba$windowF1F2change)))


#The below section uses annual JCS medians to fit the in-window variables. This is
#a way to test naive versions of our models against the better ones above.

judin$naiveMedian[judin$circuit==1] <- judin$first[judin$circuit==1]
judin$naiveMedian[judin$circuit==2] <- judin$second[judin$circuit==2]
judin$naiveMedian[judin$circuit==3] <- judin$third[judin$circuit==3]
judin$naiveMedian[judin$circuit==4] <- judin$fourth[judin$circuit==4]
judin$naiveMedian[judin$circuit==5] <- judin$fifth[judin$circuit==5]
judin$naiveMedian[judin$circuit==6] <- judin$sixth[judin$circuit==6]
judin$naiveMedian[judin$circuit==7] <- judin$seventh[judin$circuit==7]
judin$naiveMedian[judin$circuit==8] <- judin$eighth[judin$circuit==8]
judin$naiveMedian[judin$circuit==9] <- judin$ninth[judin$circuit==9]
judin$naiveMedian[judin$circuit==10] <- judin$tenth[judin$circuit==10]
judin$naiveMedian[judin$circuit==11] <- judin$eleventh[judin$circuit==11]
judin$naiveMedian[judin$circuit==12] <- judin$DC[judin$circuit==12]

judin$naiveinM <- 0
judin$naiveinMJ <- 0
judin$naiveinF1F2 <- 0
judin$naiveinH1H2M <- 0
judin$naiveinCM <- 0
judin$naiveinJH1H2M <- 0
judin$naiveinJF1F2 <- 0
judin$naiveinJCM <- 0
judin$naiveinJF1F2H1H2 <- 0
judin$naiveinJH1H2CM <- 0
judin$naiveinJF1F2C <- 0
judin$naiveinF1F2H1H2 <- 0
judin$naiveinF1F2C <- 0
judin$naiveinF1F2H1H2C <- 0
judin$naiveinH1H2CM <- 0
judin$naiveinJF1F2H1H2C <- 0
judin$naiveinFiliNoPres <- 0

judin$naiveinM[judin$naiveMedian<=judin$maxM & judin$naiveMedian>=judin$minM] <- 1
judin$naiveinMJ[judin$naiveMedian<=judin$maxMJ & judin$naiveMedian>=judin$minMJ] <- 1
judin$naiveinF1F2[judin$naiveMedian<=judin$maxF1F2 & judin$naiveMedian>=judin$minF1F2] <- 1
judin$naiveinH1H2M[judin$naiveMedian<=judin$maxH1H2M & judin$naiveMedian>=judin$minH1H2M] <- 1
judin$naiveinCM[judin$naiveMedian<=judin$maxCM & judin$naiveMedian>=judin$minCM] <- 1
judin$naiveinJH1H2M[judin$naiveMedian<=judin$maxJH1H2M & judin$naiveMedian>=judin$minJH1H2M] <- 1
judin$naiveinJF1F2[judin$naiveMedian<=judin$maxJF1F2 & judin$naiveMedian>=judin$minJF1F2] <- 1
judin$naiveinJCM[judin$naiveMedian<=judin$maxJCM & judin$naiveMedian>=judin$minJCM] <- 1
judin$naiveinJF1F2H1H2[judin$naiveMedian<=judin$maxJF1F2H1H2 & judin$naiveMedian>=judin$minJF1F2H1H2] <- 1
judin$naiveinJH1H2CM[judin$naiveMedian<=judin$maxJH1H2CM & judin$naiveMedian>=judin$minJH1H2CM] <- 1
judin$naiveinJF1F2C[judin$naiveMedian<=judin$maxJF1F2C & judin$naiveMedian>=judin$minJF1F2C] <- 1
judin$naiveinF1F2H1H2[judin$naiveMedian<=judin$maxF1F2H1H2 & judin$naiveMedian>=judin$minF1F2H1H2] <- 1
judin$naiveinF1F2C[judin$naiveMedian<=judin$maxF1F2C & judin$naiveMedian>=judin$minF1F2C] <- 1
judin$naiveinF1F2H1H2C[judin$naiveMedian<=judin$maxF1F2H1H2C & judin$naiveMedian>=judin$minF1F2H1H2C] <- 1
judin$naiveinH1H2CM[judin$naiveMedian<=judin$maxH1H2CM & judin$naiveMedian>=judin$minH1H2CM] <- 1
judin$naiveinJF1F2H1H2C[judin$naiveMedian<=judin$maxJF1F2H1H2C & judin$naiveMedian>=judin$minJF1F2H1H2C] <- 1
judin$naiveinFiliNoPres[judin$naiveMedian<=judin$gridlockHighFili & judin$naiveMedian>=judin$gridlockLowFili] <- 1
judin$naiveinFili <- 0
judin$naiveinFili[as.numeric(judin$naiveMedian)<=judin$gridlock60 & as.numeric(judin$naiveMedian)>=judin$presnom & judin$presparty=="100"& judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$naiveinFili[as.numeric(judin$naiveMedian)>=judin$gridlock40 & as.numeric(judin$naiveMedian)<=judin$presnom & judin$presparty=="200"& judin$nomdatenum>=5492 & judin$nomdatenum<19382] <- 1
judin$naiveinFili[as.numeric(judin$naiveMedian)<=judin$gridlock67 & as.numeric(judin$naiveMedian)>=judin$presnom & judin$presparty=="100"& judin$nomdatenum<5492] <- 1
judin$naiveinFili[as.numeric(judin$naiveMedian)>=judin$gridlock33 & as.numeric(judin$naiveMedian)<=judin$presnom & judin$presparty=="200"& judin$nomdatenum<5492] <- 1
judin$naiveinFili[as.numeric(judin$naiveMedian)<=judin$gridlockMed & as.numeric(judin$naiveMedian)>=judin$presnom & judin$presparty=="100" & judin$nomdatenum>=19382] <- 1
judin$naiveinFili[as.numeric(judin$naiveMedian)>=judin$gridlockMed & as.numeric(judin$naiveMedian)<=judin$presnom & judin$presparty=="200" & judin$nomdatenum>=19382] <- 1



modelNaiveinM <-glm(unsuccessful~as.numeric(naiveinM)+as.numeric(WQ), data=judin, 
               family=binomial(link="logit"))
modelNaiveinMJ <-glm(unsuccessful~as.numeric(naiveinMJ)+as.numeric(WQ), data=judin, 
                family=binomial(link="logit"))
modelNaiveinF1F2 <-glm(unsuccessful~as.numeric(naiveinF1F2)+as.numeric(WQ), data=judin, 
                  family=binomial(link="logit"))
modelNaiveinFili<-glm(unsuccessful~as.numeric(naiveinFili)+as.numeric(WQ), data=judin, 
                 family=binomial(link="logit"))
modelNaiveinH1H2M <-glm(unsuccessful~as.numeric(naiveinH1H2M)+as.numeric(WQ), data=judin, 
                   family=binomial(link="logit"))
modelNaiveinCM <-glm(unsuccessful~as.numeric(naiveinCM)+as.numeric(WQ), data=judin, 
                family=binomial(link="logit"))
modelNaiveinJH1H2M <-glm(unsuccessful~as.numeric(naiveinJH1H2M)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelNaiveinJF1F2 <-glm(unsuccessful~as.numeric(naiveinJF1F2)+as.numeric(WQ), data=judin, 
                   family=binomial(link="logit"))
modelNaiveinJCM <-glm(unsuccessful~as.numeric(naiveinJCM)+as.numeric(WQ), data=judin, 
                 family=binomial(link="logit"))
modelNaiveinJF1F2H1H2 <-glm(unsuccessful~as.numeric(naiveinJF1F2H1H2)+as.numeric(WQ), data=judin, 
                       family=binomial(link="logit"))
modelNaiveinJH1H2CM <-glm(unsuccessful~as.numeric(naiveinJH1H2CM)+as.numeric(WQ), data=judin, 
                     family=binomial(link="logit"))
modelNaiveinJF1F2C <-glm(unsuccessful~as.numeric(naiveinJF1F2C)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelNaiveinF1F2H1H2 <-glm(unsuccessful~as.numeric(naiveinF1F2H1H2)+as.numeric(WQ), data=judin, 
                      family=binomial(link="logit"))
modelNaiveinF1F2C <-glm(unsuccessful~as.numeric(naiveinF1F2C)+as.numeric(WQ), data=judin, 
                   family=binomial(link="logit"))
modelNaiveinF1F2H1H2C <-glm(unsuccessful~as.numeric(naiveinF1F2H1H2C)+as.numeric(WQ), data=judin, 
                       family=binomial(link="logit"))
modelNaiveinH1H2CM <-glm(unsuccessful~as.numeric(naiveinH1H2CM)+as.numeric(WQ), data=judin, 
                    family=binomial(link="logit"))
modelNaiveinJF1F2H1H2C <-glm(unsuccessful~as.numeric(naiveinJF1F2H1H2C)+as.numeric(WQ), data=judin, 
                         family=binomial(link="logit"))
modelNaiveinFiliNoPres <-glm(unsuccessful~as.numeric(naiveinFiliNoPres)+as.numeric(WQ), data=judin, 
                        family=binomial(link="logit"))
modelNaiveindivided <-glm(unsuccessful~as.numeric(divided)+as.numeric(WQ), data=judin, 
                             family=binomial(link="logit"))

outtableNaivein <- cbind(c(coef(modelNaiveinM)[2], coef(modelNaiveinMJ)[2], coef(modelNaiveinF1F2)[2],
                       coef(modelNaiveinH1H2M)[2],coef(modelNaiveinCM)[2],coef(modelNaiveinJH1H2M)[2],
                       coef(modelNaiveinJF1F2)[2],coef(modelNaiveinJCM)[2],coef(modelNaiveinJF1F2H1H2)[2],
                       coef(modelNaiveinJH1H2CM)[2],coef(modelNaiveinJF1F2C)[2],
                       coef(modelNaiveinF1F2H1H2)[2],coef(modelNaiveinF1F2C)[2],                    
                       coef(modelNaiveinF1F2H1H2C)[2],coef(modelNaiveinH1H2CM)[2],
                       coef(modelNaiveinJF1F2H1H2C)[2],coef(modelNaiveindivided)[2]), 
                     c(coef(summary(modelNaiveinM))[2, 2],coef(summary(modelNaiveinMJ))[2, 2],coef(summary(modelNaiveinF1F2))[2, 2],
                       coef(summary(modelNaiveinH1H2M))[2, 2],coef(summary(modelNaiveinCM))[2, 2],coef(summary(modelNaiveinJH1H2M))[2, 2],
                       coef(summary(modelNaiveinJF1F2))[2, 2],coef(summary(modelNaiveinJCM))[2, 2],coef(summary(modelNaiveinJF1F2H1H2))[2, 2],
                       coef(summary(modelNaiveinJH1H2CM))[2, 2],coef(summary(modelNaiveinJF1F2C))[2, 2]
                       ,coef(summary(modelNaiveinF1F2H1H2))[2, 2],coef(summary(modelNaiveinF1F2C))[2, 2]
                       ,coef(summary(modelNaiveinF1F2H1H2C))[2, 2],
                       coef(summary(modelNaiveinH1H2CM))[2, 2],coef(summary(modelNaiveinJF1F2H1H2C))[2, 2],
                       coef(summary(modelNaiveindivided))[2, 2]),
                     c(BIC(modelNaiveinM),BIC(modelNaiveinMJ),BIC(modelNaiveinF1F2),
                       BIC(modelNaiveinH1H2M),BIC(modelNaiveinCM),BIC(modelNaiveinJH1H2M),
                       BIC(modelNaiveinJF1F2),BIC(modelNaiveinJCM),BIC(modelNaiveinJF1F2H1H2),
                       BIC(modelNaiveinJH1H2CM),BIC(modelNaiveinJF1F2C)
                       ,BIC(modelNaiveinF1F2H1H2),BIC(modelNaiveinF1F2C)
                       ,BIC(modelNaiveinF1F2H1H2C),BIC(modelNaiveinH1H2CM),
                       BIC(modelNaiveinJF1F2H1H2C),BIC(modelNaiveindivided)))
                       
#### Whether or not the annual median JCS score is inside the gridlock interval
#### With Updated nominate but not updated JCS
xtable(outtableNaivein)  