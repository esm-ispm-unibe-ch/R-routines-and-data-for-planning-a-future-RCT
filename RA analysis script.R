
#load the neccessary libraries
library(meta)
library(netmeta)
library(magic)

##### Set up a data base to collect results ########
SScpPower=cbind.data.frame(80:1200)
names(SScpPower)=c("Sample Size")


###################################################
### Power of a single RCT                      ###
###################################################

pC=0.49
OR=0.71
oddsT=pC/(OR*(1-pC))
pT=oddsT/(1+oddsT)
kappa<-1 #allocation ratio 1:1
alpha<-0.05 #type I error 

PowerRCT<-c()
for (i in 80:1200){
  nT=i/2
  z=log(OR)*sqrt(nT)/sqrt(1/(kappa*pC*(1-pC))+1/(pT*(1-pT)))
  PowerRCT=c(PowerRCT,pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))
}

min(c(80:1200)[PowerRCT>=0.8])
#collect results
SScpPower<-cbind.data.frame(SScpPower,PowerRCT=PowerRCT)



###################################################
### Conditional power in pairwise meta-analysis ###
###################################################

#Load the TEAR2012 study data
TEAR2012 <- read.csv("Adrianis routines/TEAR2012.csv", sep=";")
                     
#data=the existing studies (here only TEAR 2012)
#level="arm"; specifis that we have arm-level data
#type="binary" ; type of the outcome
#effsize="OR" ; define the relative treatment effect measure, here OR
#p.c= the event rate ; here we input the event rate in the reference intervention
# alternative.ES= the sought difference between the groups (measured on the same scale as in the effect size); here OR=0.71
#the function estimates the conditional power in an updated meta-analysis when one new study with a given sample size is added assuming 1:1 randomisation. 
#the relative treatment effect sought is as estimated by the existing meta-analysis. Default type one error rate is 5%. 

cp.pma(data=TEAR2012,level="arm",type="binary",effsize="OR",sample.size=790,p.c=0.49, alternative.ES=0.71 )

conditionalPowerPairwise=c()
for (i in 80:1200){
  conditionalPowerPairwise=c(conditionalPowerPairwise,cp.pma(data=TEAR2012,level="arm",type="binary",effsize="OR",sample.size=i,p.c=0.49, alternative.ES=0.71 )$cp)
}

SScpPower=cbind.data.frame(SScpPower,conditionalPowerPairwise=conditionalPowerPairwise)


###################################################
### Conditional power in network meta-analysis ##
###################################################

hazlewoodperarm <- read.csv("Adrianis routines/hazlewoodperarm.csv", sep=";")

hazlewoodCP<-cp.nma.setup(data=hazlewoodperarm, level="arm", type="binary", effsize="OR")

conditionalPowerNMA<-cp.nma(hazlewoodCP,design="MTX and ETN:triple",
            sample.size.range=80:1200,p.c=0.49, alternative.ES=0.71)


SScpPower<-cbind(SScpPower,conditionalPowerNMA=conditionalPowerNMA$conditional.power)

###################################################
### Plotting the power curves                  ###
###################################################

plot(80:1200,seq(0,1,length.out =1200-79),ylim=c(0,1),type="n", ylab="Sample Size",xlab="Power")

lines(SScpPower[,1],SScpPower[,2],col="gray",lty=3, lwd=3)
lines(SScpPower[,1],SScpPower[,3],col="black",lty=2,lwd=3)
lines(SScpPower[,1],SScpPower[,4],col="blue",lwd=3)
abline(h=0.8, lwd=2,lty=3,col="red")

#Sample size for power 80%
min(SScpPower[SScpPower[,2]>=0.8,1])#for RCT
min(SScpPower[SScpPower[,3]>=0.8,1])#for pairwise meta
min(SScpPower[SScpPower[,4]>=0.8,1])#for NMA


