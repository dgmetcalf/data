pbm2 <- read.dta("pbmNomID.dta")

x<- cbind(c())
repmatrix <- array(list(), dim=c(580, 17))

for (i in 1:length(pbm2$nomdate)){
  x <- senators[senators$cong == pbm2$congress[i],]
  x <- x[pbm2$nomdatenum[i] >= x$datebeginnum & pbm2$nomdatenum[i] <= x$dateendnum,]
  x <- x[is.na(x$name)==F,]
  z <- x[pbm2$stateid[i] == x$state,]
  if (nrow(x[x$party==100,])>nrow(x[x$party==200,])){
    repmatrix[i,6] <- 100
  }
  repmatrix[i,1] <- pbm2$lastname[i]
  repmatrix[i,2] <- pbm2$year[i]
  repmatrix[i,3] <- list(x$name)
  repmatrix[i,4] <- list(x$idno)
  repmatrix[i,5] <- list(x$dwnom1)
  if (nrow(x[x$party==100,])>nrow(x[x$party==200,])){
    repmatrix[i,6] <- 100
  } else if (nrow(x[x$party==100,])<nrow(x[x$party==200,])) {
    repmatrix[i,6] <- 200
  } else {
    repmatrix[i,6] <- 200
  }
  #I handchecked. All the ties in our dataset happened under repmatrixublican Presidents
  #Meaning they're the de facto majority party
  repmatrix[i,7] <- list(x[x$party==100,]$dwnom1)
  repmatrix[i,8] <- list(x[x$party==200,]$dwnom1)
  if (length(z$name)!=0) {
    repmatrix[i,16] <- z[1,]$dwnom1
    repmatrix[i,17] <- z[2,]$dwnom1
  } else {
    repmatrix[i,16] <- NA
    repmatrix[i,17] <- NA
  }
}

pbm2$majParty <- as.numeric(repmatrix[,6])

pbm2$pres[pbm2$nomdatenum>=-9799 & pbm2$nomdatenum<=-5377] <- "FDR"
pbm2$presnom[pbm2$pres=="FDR"] <- -.505
pbm2$presparty[pbm2$pres=="FDR"] <- 100
pbm2$pres[pbm2$nomdatenum>=-5376 & pbm2$nomdatenum<=-2538] <- "HST"
pbm2$presnom[pbm2$pres=="HST"] <- -.276
pbm2$presparty[pbm2$pres=="HST"] <- 100
pbm2$pres[pbm2$nomdatenum>=-2537 & pbm2$nomdatenum<=384] <- "DDE"
pbm2$presnom[pbm2$pres=="DDE"] <- .313
pbm2$presparty[pbm2$pres=="DDE"] <- 200
pbm2$pres[pbm2$nomdatenum>=385 & pbm2$nomdatenum<=1421] <- "JFK"
pbm2$presnom[pbm2$pres=="JFK"] <- -.501
pbm2$presparty[pbm2$pres=="JFK"] <- 100
pbm2$pres[pbm2$nomdatenum>=1422 & pbm2$nomdatenum<=3306] <- "LBJ"
pbm2$presnom[pbm2$pres=="LBJ"] <- -.239
pbm2$presparty[pbm2$pres=="LBJ"] <- 100
pbm2$pres[pbm2$nomdatenum>=3307 & pbm2$nomdatenum<=5334] <- "RMN"
pbm2$presnom[pbm2$pres=="RMN"] <- .466
pbm2$presparty[pbm2$pres=="RMN"] <- 200
pbm2$pres[pbm2$nomdatenum>=5335 & pbm2$nomdatenum<=6226] <- "GF"
pbm2$presnom[pbm2$pres=="GF"] <- .363
pbm2$presparty[pbm2$pres=="GF"] <- 200
pbm2$pres[pbm2$nomdatenum>=6229 & pbm2$nomdatenum<=7689] <- "JC"
pbm2$presnom[pbm2$pres=="JC"] <- -.402
pbm2$presparty[pbm2$pres=="JC"] <- 100
pbm2$pres[pbm2$nomdatenum>=7690 & pbm2$nomdatenum<=10611] <- "RWR"
pbm2$presnom[pbm2$pres=="RWR"] <- .49
pbm2$presparty[pbm2$pres=="RWR"] <- 200
pbm2$pres[pbm2$nomdatenum>=10612 & pbm2$nomdatenum<=12072] <- "GHWB"
pbm2$presnom[pbm2$pres=="GHWB"] <- .431
pbm2$presparty[pbm2$pres=="GHWB"] <- 200
pbm2$pres[pbm2$nomdatenum>=12073 & pbm2$nomdatenum<=14994] <- "WJC"
pbm2$presnom[pbm2$pres=="WJC"] <- -.399
pbm2$presparty[pbm2$pres=="WJC"] <- 100
pbm2$pres[pbm2$nomdatenum>=14995 & pbm2$nomdatenum<=17550] <- "GWB"
pbm2$presnom[pbm2$pres=="GWB"] <- .489
pbm2$presparty[pbm2$pres=="GWB"] <- 200
pbm2$pres[pbm2$nomdatenum>=17551 & pbm2$nomdatenum<=20838] <- "BHO"
pbm2$presnom[pbm2$pres=="BHO"] <- -.364
pbm2$presparty[pbm2$pres=="BHO"] <- 100

d<- cbind(c())
commrep <- array(list(), dim=c(580, 5))

for (i in 1:length(pbm2$nomdatenum)){
  d <- committee[committee$cong == pbm2$congress[i],]
  d <- d[pbm2$nomdatenum[i] >= d$DateofAppointment & pbm2$nomdatenum[i] <= d$DateofTermination,]
  d <- d[is.na(d$name)==F,]
  if (length(d$name)==0) {
    d <- committee[committee$cong == pbm2$congress[i],]
    d <- d[pbm2$nomdatenum[i] >= d$DateofAppointment-51 & pbm2$nomdatenum[i] <= d$DateofTermination,]
    d <- d[is.na(d$name)==F,]
  }  
  commrep[i,1] <- pbm2$lastname[i]
  commrep[i,2] <- pbm2$year[i]
  commrep[i,3] <- list(d$name)
  commrep[i,4] <- list(d$dwnom1)
}
commrep[,5]<- lapply(commrep[,4],quantile,probs= c(0.5),  na.rm = TRUE)


repmatrix[,9]<- lapply(repmatrix[,5],quantile,probs= c(0.4),  na.rm = TRUE)
repmatrix[,10]<- lapply(repmatrix[,5],quantile,probs= c(0.6),  na.rm = TRUE)
repmatrix[,11]<- as.numeric(repmatrix[,10])-as.numeric(repmatrix[,9])
repmatrix[,12]<- lapply(repmatrix[,5],quantile,probs= c(0.33),  na.rm = TRUE)
repmatrix[,13]<- lapply(repmatrix[,5],quantile,probs= c(0.67),  na.rm = TRUE)
repmatrix[,14]<- lapply(repmatrix[,5],quantile,probs= c(0.5),  na.rm = TRUE)
repmatrix[,15]<- lapply(repmatrix[,7],quantile,probs= c(0.5),  na.rm = TRUE)
for (i in 1:length(repmatrix[,15])){
  if (repmatrix[i,6]==100){
    repmatrix[i,15]<- lapply(repmatrix[i,7],quantile,probs= c(0.5),  na.rm = TRUE)
  } else {
    repmatrix[i,15]<- lapply(repmatrix[i,8],quantile,probs= c(0.5),  na.rm = TRUE)
  }
}

pbm2$gridlock4060 <- repmatrix[,11]
pbm2$gridlock4060<-as.numeric(pbm2$gridlock4060)
pbm2$gridlock40 <- repmatrix[,9]
pbm2$gridlock40<-as.numeric(pbm2$gridlock40)
pbm2$gridlock60 <- repmatrix[,10]
pbm2$gridlock60<-as.numeric(pbm2$gridlock60)
pbm2$gridlock33 <- repmatrix[,12]
pbm2$gridlock33<-as.numeric(pbm2$gridlock33)
pbm2$gridlock67 <- repmatrix[,13]
pbm2$gridlock67<-as.numeric(pbm2$gridlock67)
pbm2$gridlockMed <- repmatrix[,14]
pbm2$gridlockMed<-as.numeric(pbm2$gridlockMed)
pbm2$gridlockBS1 <- repmatrix[,16]
pbm2$gridlockBS1<-as.numeric(pbm2$gridlockBS1)
pbm2$gridlockBS2 <- repmatrix[,17]
pbm2$gridlockBS2<-as.numeric(pbm2$gridlockBS2)
pbm2$gridlock3367 <- as.numeric(pbm2$gridlock67) - as.numeric(pbm2$gridlock33)
pbm2$gridlock3367<-as.numeric(pbm2$gridlock3367)
pbm2$majParty <- as.numeric(repmatrix[,6])
pbm2$gridlockMajMed <- as.numeric(repmatrix[,15])
pbm2$gridlockComm <- as.numeric(commrep[,5])
pbm2$gridlockLowFili <- 0
#5492 is the numeric date in 1975 when the cloture requirement changed from 2/3 to 3/5
pbm2$gridlockLowFili[pbm2$nomdatenum < 5492] <- pbm2$gridlock33[pbm2$nomdatenum < 5492]
pbm2$gridlockLowFili[pbm2$nomdatenum >= 5492] <- pbm2$gridlock40[pbm2$nomdatenum >= 5492]
pbm2$gridlockHighFili <- 0
pbm2$gridlockHighFili[pbm2$nomdatenum < 5492] <- pbm2$gridlock67[pbm2$nomdatenum < 5492]
pbm2$gridlockHighFili[pbm2$nomdatenum >= 5492] <- pbm2$gridlock60[pbm2$nomdatenum >= 5492]
pbm2$divided2<- 0
pbm2$divided2[pbm2$majParty!=pbm2$presparty] <-1

for (i in 1:length(pbm2$lastname)){
  pbm2$windowrepM[i] <- max(pbm2$presnom[i],pbm2$gridlockMed[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMed[i],na.rm=T)
  pbm2$windowrepMJ[i] <- max(pbm2$presnom[i],pbm2$gridlockMed[i],pbm2$gridlockMajMed[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMed[i],pbm2$gridlockMajMed[i],na.rm=T)
  pbm2$windowrepF1F2[i] <- max(pbm2$presnom[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T)
  pbm2$windowrepH1H2M[i] <- max(pbm2$presnom[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T)
  pbm2$windowrepCM[i] <- max(pbm2$presnom[i],pbm2$gridlockMed[i],pbm2$gridlockComm[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMed[i],pbm2$gridlockComm[i],na.rm=T)
  pbm2$windowrepJH1H2M[i] <- max(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T)
  pbm2$windowrepJF1F2[i] <- max(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T)
  pbm2$windowrepJCM[i] <- max(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockMed[i],pbm2$gridlockComm[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockMed[i],pbm2$gridlockComm[i],na.rm=T)
  pbm2$windowrepJF1F2H1H2[i] <- max(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockMajMed[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],na.rm=T)
  pbm2$windowrepJH1H2CM[i] <- max(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockMajMed[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockMajMed[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T)
  pbm2$windowrepJF1F2C[i] <- max(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockMajMed[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockMajMed[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T)
  pbm2$windowrepF1F2H1H2[i] <- max(pbm2$presnom[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],na.rm=T)
  pbm2$windowrepF1F2C[i] <- max(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],na.rm=T)
  pbm2$windowrepF1F2H1H2C[i] <- max(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],na.rm=T)
  pbm2$windowrepH1H2CM[i] <- max(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockComm[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockMed[i],na.rm=T)
  pbm2$windowrepJF1F2H1H2C[i] <- max(pbm2$presnom[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockMajMed[i],pbm2$gridlockComm[i],na.rm=T) -
    min(pbm2$presnom[i],pbm2$gridlockBS1[i],pbm2$gridlockBS2[i],pbm2$gridlockLowFili[i],pbm2$gridlockHighFili[i],pbm2$gridlockMajMed[i],pbm2$gridlockComm[i],na.rm=T)    
}



modelwindowrepM <-glm(failed~as.numeric(windowrepM), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                   family=binomial(link="logit"))
modelwindowrepMJ <-glm(failed~as.numeric(windowrepMJ), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                    family=binomial(link="logit"))
modelwindowrepF1F2 <-glm(failed~as.numeric(windowrepF1F2), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                      family=binomial(link="logit"))
modelwindowrepH1H2M <-glm(failed~as.numeric(windowrepH1H2M), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepCM <-glm(failed~as.numeric(windowrepCM), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                    family=binomial(link="logit"))
modelwindowrepJH1H2M <-glm(failed~as.numeric(windowrepJH1H2M), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                        family=binomial(link="logit"))
modelwindowrepJF1F2 <-glm(failed~as.numeric(windowrepJF1F2), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepJCM <-glm(failed~as.numeric(windowrepJCM), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                     family=binomial(link="logit"))
modelwindowrepJF1F2H1H2 <-glm(failed~as.numeric(windowrepJF1F2H1H2), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                           family=binomial(link="logit"))
modelwindowrepJH1H2CM <-glm(failed~as.numeric(windowrepJH1H2CM), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                         family=binomial(link="logit"))
modelwindowrepJF1F2C <-glm(failed~as.numeric(windowrepJF1F2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                        family=binomial(link="logit"))                  
modelwindowrepF1F2H1H2 <-glm(failed~as.numeric(windowrepF1F2H1H2), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepF1F2C<-glm(failed~as.numeric(windowrepF1F2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                      family=binomial(link="logit"))
modelwindowrepF1F2H1H2C<-glm(failed~as.numeric(windowrepF1F2H1H2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepH1H2CM<-glm(failed~as.numeric(windowrepH1H2CM), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepJF1F2H1H2C<-glm(failed~as.numeric(windowrepJF1F2H1H2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                           family=binomial(link="logit"))
modelwindowrepdivided <- glm(failed~as.numeric(divided2), data=pbm2[pbm2$year>=1975 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))


outtablerep <- cbind(c(coef(modelwindowrepM)[2], coef(modelwindowrepMJ)[2], coef(modelwindowrepF1F2)[2],
                    coef(modelwindowrepH1H2M)[2],coef(modelwindowrepCM)[2],coef(modelwindowrepJH1H2M)[2],
                    coef(modelwindowrepJF1F2)[2],coef(modelwindowrepJCM)[2],coef(modelwindowrepJF1F2H1H2)[2],
                    coef(modelwindowrepJH1H2CM)[2],coef(modelwindowrepJF1F2C)[2],
                    coef(modelwindowrepF1F2H1H2)[2],coef(modelwindowrepF1F2C)[2],                    
                    coef(modelwindowrepF1F2H1H2C)[2],coef(modelwindowrepH1H2CM)[2],coef(modelwindowrepJF1F2H1H2C)[2],
                    coef(modelwindowrepdivided)[2]), 
                  c(coef(summary(modelwindowrepM))[2, 2],coef(summary(modelwindowrepMJ))[2, 2],coef(summary(modelwindowrepF1F2))[2, 2],
                    coef(summary(modelwindowrepH1H2M))[2, 2],coef(summary(modelwindowrepCM))[2, 2],coef(summary(modelwindowrepJH1H2M))[2, 2],
                    coef(summary(modelwindowrepJF1F2))[2, 2],coef(summary(modelwindowrepJCM))[2, 2],coef(summary(modelwindowrepJF1F2H1H2))[2, 2],
                    coef(summary(modelwindowrepJH1H2CM))[2, 2],coef(summary(modelwindowrepJF1F2C))[2, 2]
                    ,coef(summary(modelwindowrepF1F2H1H2))[2, 2],coef(summary(modelwindowrepF1F2C))[2, 2]
                    ,coef(summary(modelwindowrepF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowrepH1H2CM))[2, 2],coef(summary(modelwindowrepJF1F2H1H2C))[2, 2],
                    coef(summary(modelwindowrepdivided))[2, 2]),
                  c(BIC(modelwindowrepM),BIC(modelwindowrepMJ),BIC(modelwindowrepF1F2),
                    BIC(modelwindowrepH1H2M),BIC(modelwindowrepCM),BIC(modelwindowrepJH1H2M),
                    BIC(modelwindowrepJF1F2),BIC(modelwindowrepJCM),BIC(modelwindowrepJF1F2H1H2),
                    BIC(modelwindowrepJH1H2CM),BIC(modelwindowrepJF1F2C),
                    BIC(modelwindowrepF1F2H1H2),BIC(modelwindowrepF1F2C),
                    BIC(modelwindowrepF1F2H1H2C),
                    BIC(modelwindowrepH1H2CM),BIC(modelwindowrepJF1F2H1H2C),
                    BIC(modelwindowrepdivided)))

### windowrep size using our data, no JCS scores (i.e. no status quo)
### Coefficient on main variable, Standard Error on main variable, and BIC

rownames(outtablerep)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablerep<-outtablerep[order(outtablerep[,3]),]
xtable(outtablerep, caption="1975-2006 Our Senates, PBM Judges")



1975-1988

modelwindowrepM <-glm(failed~as.numeric(windowrepM), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                      family=binomial(link="logit"))
modelwindowrepMJ <-glm(failed~as.numeric(windowrepMJ), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepF1F2 <-glm(failed~as.numeric(windowrepF1F2), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                         family=binomial(link="logit"))
modelwindowrepH1H2M <-glm(failed~as.numeric(windowrepH1H2M), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepCM <-glm(failed~as.numeric(windowrepCM), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepJH1H2M <-glm(failed~as.numeric(windowrepJH1H2M), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                           family=binomial(link="logit"))
modelwindowrepJF1F2 <-glm(failed~as.numeric(windowrepJF1F2), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepJCM <-glm(failed~as.numeric(windowrepJCM), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                        family=binomial(link="logit"))
modelwindowrepJF1F2H1H2 <-glm(failed~as.numeric(windowrepJF1F2H1H2), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                              family=binomial(link="logit"))
modelwindowrepJH1H2CM <-glm(failed~as.numeric(windowrepJH1H2CM), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                            family=binomial(link="logit"))
modelwindowrepJF1F2C <-glm(failed~as.numeric(windowrepJF1F2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                           family=binomial(link="logit"))                  
modelwindowrepF1F2H1H2 <-glm(failed~as.numeric(windowrepF1F2H1H2), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                             family=binomial(link="logit"))
modelwindowrepF1F2C<-glm(failed~as.numeric(windowrepF1F2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                         family=binomial(link="logit"))
modelwindowrepF1F2H1H2C<-glm(failed~as.numeric(windowrepF1F2H1H2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                             family=binomial(link="logit"))
modelwindowrepH1H2CM<-glm(failed~as.numeric(windowrepH1H2CM), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepJF1F2H1H2C<-glm(failed~as.numeric(windowrepJF1F2H1H2C), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                              family=binomial(link="logit"))
modelwindowrepdivided <- glm(failed~as.numeric(divided2), data=pbm2[pbm2$year>=1975 & pbm2$year<=1988 & pbm2$circuit!=0,], 
                             family=binomial(link="logit"))


outtablerep7588 <- cbind(c(coef(modelwindowrepM)[2], coef(modelwindowrepMJ)[2], coef(modelwindowrepF1F2)[2],
                       coef(modelwindowrepH1H2M)[2],coef(modelwindowrepCM)[2],coef(modelwindowrepJH1H2M)[2],
                       coef(modelwindowrepJF1F2)[2],coef(modelwindowrepJCM)[2],coef(modelwindowrepJF1F2H1H2)[2],
                       coef(modelwindowrepJH1H2CM)[2],coef(modelwindowrepJF1F2C)[2],
                       coef(modelwindowrepF1F2H1H2)[2],coef(modelwindowrepF1F2C)[2],                    
                       coef(modelwindowrepF1F2H1H2C)[2],coef(modelwindowrepH1H2CM)[2],coef(modelwindowrepJF1F2H1H2C)[2],
                       coef(modelwindowrepdivided)[2]), 
                     c(coef(summary(modelwindowrepM))[2, 2],coef(summary(modelwindowrepMJ))[2, 2],coef(summary(modelwindowrepF1F2))[2, 2],
                       coef(summary(modelwindowrepH1H2M))[2, 2],coef(summary(modelwindowrepCM))[2, 2],coef(summary(modelwindowrepJH1H2M))[2, 2],
                       coef(summary(modelwindowrepJF1F2))[2, 2],coef(summary(modelwindowrepJCM))[2, 2],coef(summary(modelwindowrepJF1F2H1H2))[2, 2],
                       coef(summary(modelwindowrepJH1H2CM))[2, 2],coef(summary(modelwindowrepJF1F2C))[2, 2]
                       ,coef(summary(modelwindowrepF1F2H1H2))[2, 2],coef(summary(modelwindowrepF1F2C))[2, 2]
                       ,coef(summary(modelwindowrepF1F2H1H2C))[2, 2],
                       coef(summary(modelwindowrepH1H2CM))[2, 2],coef(summary(modelwindowrepJF1F2H1H2C))[2, 2],
                       coef(summary(modelwindowrepdivided))[2, 2]),
                     c(BIC(modelwindowrepM),BIC(modelwindowrepMJ),BIC(modelwindowrepF1F2),
                       BIC(modelwindowrepH1H2M),BIC(modelwindowrepCM),BIC(modelwindowrepJH1H2M),
                       BIC(modelwindowrepJF1F2),BIC(modelwindowrepJCM),BIC(modelwindowrepJF1F2H1H2),
                       BIC(modelwindowrepJH1H2CM),BIC(modelwindowrepJF1F2C),
                       BIC(modelwindowrepF1F2H1H2),BIC(modelwindowrepF1F2C),
                       BIC(modelwindowrepF1F2H1H2C),
                       BIC(modelwindowrepH1H2CM),BIC(modelwindowrepJF1F2H1H2C),
                       BIC(modelwindowrepdivided)))

### windowrep size using our data, no JCS scores (i.e. no status quo)
### Coefficient on main variable, Standard Error on main variable, and BIC

rownames(outtablerep7588)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablerep7588<-outtablerep7588[order(outtablerep7588[,3]),]
xtable(outtablerep7588, caption="1975-1988 Our Senates, PBM Judges")


1989-2006

modelwindowrepM <-glm(failed~as.numeric(windowrepM), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                      family=binomial(link="logit"))
modelwindowrepMJ <-glm(failed~as.numeric(windowrepMJ), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepF1F2 <-glm(failed~as.numeric(windowrepF1F2), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                         family=binomial(link="logit"))
modelwindowrepH1H2M <-glm(failed~as.numeric(windowrepH1H2M), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepCM <-glm(failed~as.numeric(windowrepCM), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                       family=binomial(link="logit"))
modelwindowrepJH1H2M <-glm(failed~as.numeric(windowrepJH1H2M), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                           family=binomial(link="logit"))
modelwindowrepJF1F2 <-glm(failed~as.numeric(windowrepJF1F2), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepJCM <-glm(failed~as.numeric(windowrepJCM), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                        family=binomial(link="logit"))
modelwindowrepJF1F2H1H2 <-glm(failed~as.numeric(windowrepJF1F2H1H2), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                              family=binomial(link="logit"))
modelwindowrepJH1H2CM <-glm(failed~as.numeric(windowrepJH1H2CM), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                            family=binomial(link="logit"))
modelwindowrepJF1F2C <-glm(failed~as.numeric(windowrepJF1F2C), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                           family=binomial(link="logit"))                  
modelwindowrepF1F2H1H2 <-glm(failed~as.numeric(windowrepF1F2H1H2), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                             family=binomial(link="logit"))
modelwindowrepF1F2C<-glm(failed~as.numeric(windowrepF1F2C), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                         family=binomial(link="logit"))
modelwindowrepF1F2H1H2C<-glm(failed~as.numeric(windowrepF1F2H1H2C), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                             family=binomial(link="logit"))
modelwindowrepH1H2CM<-glm(failed~as.numeric(windowrepH1H2CM), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                          family=binomial(link="logit"))
modelwindowrepJF1F2H1H2C<-glm(failed~as.numeric(windowrepJF1F2H1H2C), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                              family=binomial(link="logit"))
modelwindowrepdivided <- glm(failed~as.numeric(divided2), data=pbm2[pbm2$year>=1989 & pbm2$year<=2006 & pbm2$circuit!=0,], 
                             family=binomial(link="logit"))


outtablerep8906 <- cbind(c(coef(modelwindowrepM)[2], coef(modelwindowrepMJ)[2], coef(modelwindowrepF1F2)[2],
                           coef(modelwindowrepH1H2M)[2],coef(modelwindowrepCM)[2],coef(modelwindowrepJH1H2M)[2],
                           coef(modelwindowrepJF1F2)[2],coef(modelwindowrepJCM)[2],coef(modelwindowrepJF1F2H1H2)[2],
                           coef(modelwindowrepJH1H2CM)[2],coef(modelwindowrepJF1F2C)[2],
                           coef(modelwindowrepF1F2H1H2)[2],coef(modelwindowrepF1F2C)[2],                    
                           coef(modelwindowrepF1F2H1H2C)[2],coef(modelwindowrepH1H2CM)[2],coef(modelwindowrepJF1F2H1H2C)[2],
                           coef(modelwindowrepdivided)[2]), 
                         c(coef(summary(modelwindowrepM))[2, 2],coef(summary(modelwindowrepMJ))[2, 2],coef(summary(modelwindowrepF1F2))[2, 2],
                           coef(summary(modelwindowrepH1H2M))[2, 2],coef(summary(modelwindowrepCM))[2, 2],coef(summary(modelwindowrepJH1H2M))[2, 2],
                           coef(summary(modelwindowrepJF1F2))[2, 2],coef(summary(modelwindowrepJCM))[2, 2],coef(summary(modelwindowrepJF1F2H1H2))[2, 2],
                           coef(summary(modelwindowrepJH1H2CM))[2, 2],coef(summary(modelwindowrepJF1F2C))[2, 2]
                           ,coef(summary(modelwindowrepF1F2H1H2))[2, 2],coef(summary(modelwindowrepF1F2C))[2, 2]
                           ,coef(summary(modelwindowrepF1F2H1H2C))[2, 2],
                           coef(summary(modelwindowrepH1H2CM))[2, 2],coef(summary(modelwindowrepJF1F2H1H2C))[2, 2],
                           coef(summary(modelwindowrepdivided))[2, 2]),
                         c(BIC(modelwindowrepM),BIC(modelwindowrepMJ),BIC(modelwindowrepF1F2),
                           BIC(modelwindowrepH1H2M),BIC(modelwindowrepCM),BIC(modelwindowrepJH1H2M),
                           BIC(modelwindowrepJF1F2),BIC(modelwindowrepJCM),BIC(modelwindowrepJF1F2H1H2),
                           BIC(modelwindowrepJH1H2CM),BIC(modelwindowrepJF1F2C),
                           BIC(modelwindowrepF1F2H1H2),BIC(modelwindowrepF1F2C),
                           BIC(modelwindowrepF1F2H1H2C),
                           BIC(modelwindowrepH1H2CM),BIC(modelwindowrepJF1F2H1H2C),
                           BIC(modelwindowrepdivided)))

### windowrep size using our data, no JCS scores (i.e. no status quo)
### Coefficient on main variable, Standard Error on main variable, and BIC

rownames(outtablerep8906)<-c("mediangridlock","weakmajgridlock",   "purefilibustzone",  "purebluezone","committeezone",     "majbluezone",       "majfilizone",       "commpartyzone","majbluefili", "commpartybluezone","commpartyfilizone", "bsfilibustzone","commfilizone","commfilibluezone","commbluezone" ,     "newfullmodelzone","divided")
outtablerep8906<-outtablerep8906[order(outtablerep8906[,3]),]
xtable(outtablerep8906, caption="1989-2006 Our Senates, PBM Judges")



summary(lm(commpartybluezone~windowrepJH1H2CM, data=pbm2))

table(pbm2$congress[pbm$congress>105], pbm2$mediangridlock[pbm$congress>105])


pdf("majWindowDiff.pdf",width=5,height=4)
plot(weakmajgridlock~windowrepMJ, data=pbm2, ylim=c(0,1.2))
points(weakmajgridlock~windowrepMJ, data=pbm2[pbm2$divided!=pbm2$divided2,], col="red", pch=16)
dev.off()

pdf("blueWindowDiff.pdf",width=5,height=4)
plot(purebluezone~windowrepH1H2M, data=pbm2, ylim=c(0,1.2))
points(purebluezone~windowrepH1H2M, data=pbm2[pbm2$divided!=pbm2$divided2,], col="red", pch=16)
dev.off()

pdf("commWindowDiff.pdf",width=5,height=4)
plot(committeezone~windowrepCM, data=pbm2, ylim=c(0,1.2))
points(committeezone~windowrepCM, data=pbm2[pbm2$divided!=pbm2$divided2,], col="red", pch=16)
dev.off()

plot(weakmajgridlock~windowrepMJ, data=pbm2, ylim=c(0,1.2))
points(weakmajgridlock~windowrepMJ, data=pbm2[pbm2$year==1955|pbm2$year==1956|
          pbm2$year==1969|pbm2$year==2001,], col="red")


