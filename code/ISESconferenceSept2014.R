## Data analysis for ISES 2014 presentation
# uses urine and phthalate data that JOU is processing

#RED 9-24-14
#Note: there may be some modifications to the data files since processing happened quickly in preparation for CDC clearance of the presentation slides
#double check data files before re-running code

setwd("S:/Central_Files/Projects/HUD Healthy Homes/data analysis/")

library(plyr)
library(nlme)
source("R code/stripchart_c.txt")

#still having issues with some of the air processing, 
#not considered final air data because HOBO data not fully integrated for all
#JOU (poor Julia!) still working issues
#we will drop the incomplete cases for this analysis for ISES but will track down for reportback and subsequent analysis



##############################################################################################################################
###  AIR
##############################################################################################################################
#read in air data -- only need to pull in the EI data since all of the phthalates should be in there
air<-read.csv("data/final/air/GHSEIdata.csv", stringsAsFactors=FALSE)


#read in chemlist for air
chemlist_air<-read.csv("data/raw/chemlist_air.csv", stringsAsFactors=FALSE)
chemlist_phthalate_air<-chemlist_air[grep("phthalate", chemlist_air$PrettyName),]
#drop BEHTBP and d4.di.n.butyl.phthalate
chemlist_phthalate_air<-subset(chemlist_phthalate_air, !(Analyte=="BEHTBP"|Analyte=="d4.di.n.butyl.phthalate"))
chemlist_phthalate_air$Order<-NULL
chemlist_phthalate_air$Abbreviation<-chemlist_phthalate_air$PrettyAbbrev
chemlist_phthalate_air$PrettyAbbrev<-NULL


#subset out phthalates
airphthalates<-air[grep("phthalate",air$Analyte),]
#drop BEHTBP and d4.di.n.butyl.phthalate
airphthalates<-subset(airphthalates, !(Analyte=="BEHTBP"|Analyte=="d4.di.n.butyl.phthalate"))
#should be six
unique(airphthalates$Analyte)


#sort order to phthalates
sorting.air<-data.frame(Abbreviation=c("DEP", "DBP", "BBP", "DEHP", "DCHP", "DINP"), Sort.Order.Air=1:6)
chemlist_phthalate_air<-merge(chemlist_phthalate_air, sorting.air, by="Abbreviation")

airphthalates<-merge(airphthalates, chemlist_phthalate_air, by="Analyte", all.x=T)
airphthalates$VisitType<-gsub(" ","",airphthalates$VisitType) #remove weird space
airphthalates$VisitType<-ifelse(airphthalates$IDwithvisit=="C1017-V4","Green",airphthalates$VisitType) #missing VisitType
airphthalates$City<-ifelse(airphthalates$IDwithvisit=="C1017-V4","Cincinnati",airphthalates$City) #missing City

##
## GREEN VS CONTROL
####
#let's start to quickly look at some summary
air.summary<-ddply(airphthalates, c("Abbreviation","City","VisitType"), summarise,
                     N=sum(!is.na(conc_ngperm3)), 
                     detfreq=signif((sum(!(conc_ngperm3==0))/sum(!is.na(conc_ngperm3)))*100,2),
                     min=signif(min(conc_ngperm3),3),
                     mean=signif(mean(conc_ngperm3),3),
                     median=signif(median(conc_ngperm3),3),
                     P95=signif(quantile(conc_ngperm3, p=0.95),3),
                     max=signif(max(conc_ngperm3),3))
air.summary<-merge(air.summary,sorting.air[,c("Abbreviation","Sort.Order.Air")], by="Abbreviation", all.x=T)
air.summary<-air.summary[order(air.summary$Sort.Order),]
#write.csv(air.summary, "results/air_summarytable.csv", row.names=FALSE)


airphthalates<-airphthalates[order(airphthalates$Sort.Order.Air),]


#nondetects have to be substituted with some non-zero value for visualizations
#since we don't have a LOD yet, I am going to use 1/2 the minimum detect
halfmins<-ddply(airphthalates[airphthalates$conc_ngperm3>0,], "Abbreviation", summarise,
                halfmin=min(conc_ngperm3)/2)
airphthalates<-merge(airphthalates, halfmins, by="Abbreviation")
airphthalates$conc_ngperm3<-ifelse(airphthalates$conc_ngperm3==0,airphthalates$halfmin,airphthalates$conc_ngperm3) #####SUBSTITUTING



jpeg("results/airphthalates_greenvcontrolvcity.jpg", 
     units="in", width=15, height=7.5, res=300)
par(mar=c(5,5,2,2))
fstripchart.c(airphthalates$conc_ngperm3~airphthalates$Sort.Order.Air, 
              vertical=T, method="jitter", jitter=0.05, pch=NA, log="y",
              ylab="Concentration (ng/m3)")
fstripchart.c(airphthalates$conc_ngperm3[airphthalates$City=="Boston"&airphthalates$VisitType=="Green"]~airphthalates$Sort.Order.Air[airphthalates$City=="Boston"&airphthalates$VisitType=="Green"], 
              add=TRUE, at=1:length(unique(airphthalates$Abbreviation))-0.2, log="y",
              vertical=T, method="jitter", jitter=0.05, pch=1,  col="green")
fstripchart.c(airphthalates$conc_ngperm3[airphthalates$City=="Boston"&airphthalates$VisitType=="Control"]~airphthalates$Sort.Order.Air[airphthalates$City=="Boston"&airphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(airphthalates$Abbreviation))-0.1, log="y",
              vertical=T, method="jitter", jitter=0.05, pch=1,  col="black")
fstripchart.c(airphthalates$conc_ngperm3[airphthalates$City=="Cincinnati"&airphthalates$VisitType=="Green"]~airphthalates$Sort.Order.Air[airphthalates$City=="Cincinnati"&airphthalates$VisitType=="Green"], 
              add=TRUE, at=1:length(unique(airphthalates$Abbreviation))+0.1, log="y",
              vertical=T, method="jitter", jitter=0.05, pch=0,  col="green")
fstripchart.c(airphthalates$conc_ngperm3[airphthalates$City=="Cincinnati"&airphthalates$VisitType=="Control"]~airphthalates$Sort.Order.Air[airphthalates$City=="Cincinnati"&airphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(airphthalates$Abbreviation))+0.2, log="y",
              vertical=T, method="jitter", jitter=0.05, pch=0,  col="black")
axis(2)
axis(1, at=1:length(unique(airphthalates$Abbreviation)), labels=unique(airphthalates$Abbreviation), las=2, cex.axis=0.6)
legend("top", ncol=4, legend=c("Boston Green","Boston Control","Cincinnati Green","Cincinnati Control"), pch=c(1,1,0,0), col=c("green","black","green","black"))
dev.off()

airphthalates<-airphthalates[order(airphthalates$Sort.Order.Air),]

jpeg("results/airphthalates_greenvcontrol.jpg", 
     units="in", width=15, height=7.5, res=300)
par(mar=c(5,5,2,2))
fstripchart.c(airphthalates$conc_ngperm3~airphthalates$Sort.Order.Air, 
              vertical=T, method="jitter", jitter=0.05, pch=NA, log="y",
              ylab="Concentration (ng/m3)")
fstripchart.c(airphthalates$conc_ngperm3[airphthalates$VisitType=="Green"]~airphthalates$Sort.Order.Air[airphthalates$VisitType=="Green"], 
              add=TRUE, at=1:length(unique(airphthalates$Abbreviation))-0.2,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="green")
fstripchart.c(airphthalates$conc_ngperm3[airphthalates$VisitType=="Control"]~airphthalates$Sort.Order.Air[airphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(airphthalates$Abbreviation))-0.1,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="black")
axis(2)
axis(1, at=1:length(unique(airphthalates$Abbreviation)), labels=unique(airphthalates$Abbreviation), las=2, cex.axis=0.6)
legend("top", ncol=2, legend=c("Green","Control"), pch=c(1,1), col=c("green","black"))
dev.off()
#these don't really look that different
#quick testing
greenvcontrol_wilcox<-ddply(airphthalates, "Abbreviation", summarise,
                            median_green=median(conc_ngperm3[VisitType=="Green"]),
                            median_control=median(conc_ngperm3[VisitType=="Control"]),
                            wilcox_p=wilcox.test(conc_ngperm3[VisitType=="Green"], conc_ngperm3[VisitType=="Control"])$p.value)
#marginally significant differences

airphthalates<-airphthalates[order(airphthalates$Sort.Order.Air),]

jpeg("results/airphthalates_greenvcontrol_boxplot.jpg", 
     units="in", width=15, height=7.5, res=300)
boxplot(airphthalates$conc_ngperm3[airphthalates$VisitType=="Green"]~airphthalates$Sort.Order.Air[airphthalates$VisitType=="Green"], 
        at=1:length(unique(airphthalates$Abbreviation))-0.2, log="y", col="green", boxwex=0.3,
        ylab="Concentration (ng/m3)", cex.lab=1.5, axes=F)
boxplot(airphthalates$conc_ngperm3[airphthalates$VisitType=="Control"]~airphthalates$Sort.Order.Air[airphthalates$VisitType=="Control"], 
        add=TRUE, at=1:length(unique(airphthalates$Abbreviation))+0.2, log="y", col="gray", boxwex=0.3, axes=F)
axis(1, at=1:length(unique(airphthalates$Sort.Order.Air)), 
     labels=unique(as.character(airphthalates$Abbreviation)), 
     mgp=c(3,0.7,0), cex.axis=1.2)
axis(2, at=c(0.1,1,10,100,1000,10000), 
     labels=c(0.1,1,10,100,1000,10000), 
     las=2, mgp=c(2.5,0.7,0), cex.axis=1.2)
legend("top", ncol=2, legend=c("Green","Control"), col=c("green","gray"), pch=15, inset=0.02)
box()
dev.off()


air_greenvcontrol_wilcox<-ddply(airphthalates, "Abbreviation", summarise,
                            median_green=median(conc_ngperm3[VisitType=="Green"]),
                            median_control=median(conc_ngperm3[VisitType=="Control"]),
                            wilcox_p=wilcox.test(conc_ngperm3[VisitType=="Green"], conc_ngperm3[VisitType=="Control"])$p.value)


#####
# GREEN V CONTROL WITH CITY
#########


####
#   LINEAR MODELING
############
## start linear modeling - first with City in model
chemicals<-unique(airphthalates$Abbreviation)
airmodel<-data.frame(Abbreviation=chemicals,VisitType_est=NA,VisitType_p=NA,City_est=NA,City_p=NA, AIC=NA)
for (i in 1:length(unique(airphthalates$Abbreviation))) {
  xx<-subset(airphthalates, Abbreviation==unique(airphthalates$Abbreviation)[i])
  airmodel$metabolites[i]<-unique(xx$Abbreviation)
  airmodel$VisitType_est[i]<-summary(lme(log(conc_ngperm3)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[2,1]
  airmodel$VisitType_p[i]<-summary(lme(log(conc_ngperm3)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[2,5]
  airmodel$City_est[i]<-summary(lme(log(conc_ngperm3)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[3,1]
  airmodel$City_p[i]<-summary(lme(log(conc_ngperm3)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[3,5]
  airmodel$AIC[i]<-summary(lme(log(conc_ngperm3)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$AIC
}

## now without City as fixed but as nested
chemicals<-unique(airphthalates$Abbreviation)
airmodel2<-data.frame(Abbreviations=chemicals,VisitType_est=NA,VisitType_p=NA,AIC=NA)
for (i in 1:length(unique(airphthalates$Abbreviation))) {
  xx<-subset(airphthalates, Abbreviation==unique(airphthalates$Abbreviation)[i])
  airmodel2$metabolites[i]<-unique(xx$Abbreviation)
  airmodel2$VisitType_est[i]<-summary(lme(log(conc_ngperm3)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,1]
  airmodel2$VisitType_p[i]<-summary(lme(log(conc_ngperm3)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,5]
  airmodel2$AIC[i]<-summary(lme(log(conc_ngperm3)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$AIC
}
#very different results, let's compare AICs

## now without City at all in model
chemicals<-unique(airphthalates$Abbreviation)
airmodel3<-data.frame(Abbreviation=chemicals,VisitType_est=NA,VisitType_p=NA,AIC=NA)
for (i in 1:length(unique(airphthalates$Abbreviation))) {
  xx<-subset(airphthalates, Abbreviation==unique(airphthalates$Abbreviation)[i])
  airmodel3$metabolites[i]<-unique(xx$Abbreviation)
  airmodel3$VisitType_est[i]<-summary(lme(log(conc_ngperm3)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,1]
  airmodel3$VisitType_p[i]<-summary(lme(log(conc_ngperm3)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,5]
  airmodel3$AIC[i]<-summary(lme(log(conc_ngperm3)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$AIC
}


#merge in parents and metabolites 
parentmetabs<-data.frame(parent=c("DINCH","DBP","BBP","BBP","DiDP","DINP","DOP","DEHP","DEHP","DEHP","DEHP","DEP","DMP","DIBP"),
                         metab=c("MHiNCH","mBP","mBP","mBzP2","mCNP","mCOP","mCPP","mECPP","mEHHP","mEHP","mEOHP","mEP2","mMP","miBP"))




##############################################################################################################################
###  URINE
##############################################################################################################################
#reading in the urine data -- phthalates only 
urinaryphthalates<-read.csv("data/final/urine/phthalatedata.csv", stringsAsFactors=FALSE, strip.white=TRUE)
urinaryphthalates$VisitType<-gsub(" ","",urinaryphthalates$VisitType)

unique(urinaryphthalates$Abbreviation)
unique(urinaryphthalates$Analyte)

chemlist_urinaryphthalates<-data.frame(Abbreviation=c("mBP","mBzP2","mCNP","mCOP","mCPP","mECPP","mEHHP","mEHP","mEOHP","mEP2","MHiNCH","miBP","mMP"),
                                       PrettyAbbreviation=c("MBP","MBzP","MCNP","MCOP","MCPP","MECPP","MEHHP","MEHP","MEOHP","MEP","MHiNCH","MiBP","MMP"),
                                       Analyte=c("Mono-n-butyl phthalate","Monobenzyl phthalate","Mono carboxyisononyl phthalate","Mono carboxyisooctyl phthalate","Mono-3-carboxypropyl phthalate","Mono-2-ethyl-5-carboxypentyl phthalate","Mono-2-ethyl-5-hydroxyhexyl phthalate","Mono-2-ethylhexyl phthalate","Mono-2-ethyl-5-oxohexyl phthalate","Monoethyl phthalate","Cyclohexane-1 2-dicarboxylic acid monohydroxy isononyl ester","Mono-isobutyl phthalate","Monomethyl phthalate"))
parentmetabs<-data.frame(parent=c("DINCH","DBP","BBP","BBP","DiDP","DINP","DOP","DEHP","DEHP","DEHP","DEHP","DEP","DMP","DIBP"),
                         metab=c("MHiNCH","mBP","mBP","mBzP2","mCNP","mCOP","mCPP","mECPP","mEHHP","mEHP","mEOHP","mEP2","mMP","miBP"))
chemlist_urinaryphthalates<-merge(chemlist_urinaryphthalates, parentmetabs, by.x="Abbreviation", by.y="metab")
chemlist_urinaryphthalates$Sort.Order<-c(3,3,5,12,11,10,9,7,6,8,2,13,4,1)
#dropping the MBP assignment to BBP parent
chemlist_urinaryphthalates<-subset(chemlist_urinaryphthalates, !(Abbreviation=="mBP"&parent=="BBP"))


#let's start to quickly look at some summary
urine.summary<-ddply(urinaryphthalates, c("Abbreviation","City","VisitType"), summarise,
                     N=sum(!is.na(conc_ngml)), 
                     detfreq=signif((sum((LOD==""))/sum(!is.na(conc_ngml)))*100,2),
                     min=signif(min(conc_ngml),3),
                     mean=signif(mean(conc_ngml),3),
                     median=signif(median(conc_ngml),3),
                     P95=signif(quantile(conc_ngml, p=0.95),3),
                     max=signif(max(conc_ngml),3))
urine.summary<-merge(urine.summary,chemlist_urinaryphthalates[,c("Abbreviation","PrettyAbbreviation","Sort.Order")], by="Abbreviation", all.x=T)
urine.summary<-urine.summary[order(urine.summary$Sort.Order),]
write.csv(urine.summary, "results/urine_summarytable.csv", row.names=FALSE)



urinaryphthalates<-merge(urinaryphthalates, chemlist_urinaryphthalates, by="Abbreviation", all.x=T)
urinaryphthalates<-urinaryphthalates[order(urinaryphthalates$Sort.Order),]


jpeg("results/urinaryphthalates_greenvcontrolvcity.jpg", 
     units="in", width=15, height=7.5, res=300)
par(mar=c(5,5,2,2))
fstripchart.c(urinaryphthalates$conc_ngml~urinaryphthalates$Sort.Order, 
              vertical=T, method="jitter", jitter=0.05, pch=NA, log="y",
              ylab="Concentration (ng/ml)")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Boston"&urinaryphthalates$VisitType=="Green"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Boston"&urinaryphthalates$VisitType=="Green"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))-0.2,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="green")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Boston"&urinaryphthalates$VisitType=="Control"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Boston"&urinaryphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))-0.1,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="black")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$VisitType=="Green"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$VisitType=="Green"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))+0.1,
              vertical=T, method="jitter", jitter=0.05, pch=0, log="y", col="green")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$VisitType=="Control"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))+0.2,
              vertical=T, method="jitter", jitter=0.05, pch=0, log="y", col="black")
axis(2)
axis(1, at=1:length(unique(urinaryphthalates$Abbreviation)), labels=unique(urinaryphthalates$PrettyAbbreviation), las=2, cex.axis=0.6)
legend("top", ncol=4, legend=c("Boston Green","Boston Control","Cincinnati Green","Cincinnati Control"), pch=c(1,1,0,0), col=c("green","black","green","black"))
dev.off()




urinaryphthalates<-urinaryphthalates[order(urinaryphthalates$Sort.Order),]

jpeg("results/urinaryphthalates_greenvcontrol.jpg", 
     units="in", width=15, height=7.5, res=300)
par(mar=c(5,5,2,2))
fstripchart.c(urinaryphthalates$conc_ngml~urinaryphthalates$Sort.Order, 
              vertical=T, method="jitter", jitter=0.05, pch=NA, log="y",
              ylab="Concentration (ng/ml)")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$VisitType=="Green"]~urinaryphthalates$Sort.Order[urinaryphthalates$VisitType=="Green"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))-0.2,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="green")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$VisitType=="Control"]~urinaryphthalates$Sort.Order[urinaryphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))-0.1,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="black")
axis(2)
axis(1, at=1:length(unique(urinaryphthalates$Abbreviation)), labels=unique(urinaryphthalates$PrettyAbbreviation), las=2, cex.axis=0.6)
legend("top", ncol=2, legend=c("Green","Control"), pch=c(1,1), col=c("green","black"))
dev.off()
#these don't really look that different
#quick testing
greenvcontrol_wilcox<-ddply(urinaryphthalates, "Abbreviation", summarise,
                            median_green=median(conc_ngml[VisitType=="Green"]),
                            median_control=median(conc_ngml[VisitType=="Control"]),
                            wilcox_est=wilcox.test(conc_ngml[VisitType=="Green"], conc_ngml[VisitType=="Control"])$p.value)


urinaryphthalates<-urinaryphthalates[order(urinaryphthalates$Sort.Order),]

jpeg("results/urinaryphthalates_greenvcontrol_boxplot.jpg", 
     units="in", width=15, height=7.5, res=300)
boxplot(urinaryphthalates$conc_ngml[urinaryphthalates$VisitType=="Green"]~urinaryphthalates$Sort.Order[urinaryphthalates$VisitType=="Green"], 
              at=1:length(unique(urinaryphthalates$Abbreviation))-0.2, log="y", col="green", boxwex=0.3,
              ylab="Concentration (ng/ml)", cex.lab=1.5, axes=F)
boxplot(urinaryphthalates$conc_ngml[urinaryphthalates$VisitType=="Control"]~urinaryphthalates$Sort.Order[urinaryphthalates$VisitType=="Control"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))+0.2, log="y", col="gray", boxwex=0.3, axes=F)
axis(1, at=1:length(unique(urinaryphthalates$Sort.Order)), 
     labels=unique(as.character(urinaryphthalates$PrettyAbbreviation)), 
     mgp=c(3,0.7,0), cex.axis=1.2)
axis(2, at=c(0.1,1,10,100,1000), 
     labels=c(0.1,1,10,100,1000), 
     las=2, mgp=c(2.5,0.7,0), cex.axis=1.2)
legend("top", ncol=2, legend=c("Green","Control"), col=c("green","gray"), pch=15, inset=0.02)
box()
dev.off()


### comparing urinary concentrations to NHANES
#oscar pulled out data for 7-12 year olds, cycle 2009-2010

nhanes<-read.csv("data/final/NHANES comparison data/NHANES.Phthalates.0910.Levels.csv", stringsAsFactors=FALSE)
#need to work on matching names between chemlist_urinaryphthalates and nhanes
nhanes$Analyte<-nhanes$chem
nhanes$Analyte<-gsub(" \\(ng/mL\\)","",nhanes$Analyte)
nhanes$Analyte<-gsub("\\(","",nhanes$Analyte)
nhanes$Analyte<-gsub("\\)","",nhanes$Analyte)
#argh
setdiff(nhanes$Analyte, as.character(chemlist_urinaryphthalates$Analyte))
nhanes$Analyte<-gsub("Monocarboxyoctyl phthalate","Mono carboxyisooctyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Monocarboxynonyl phthalate","Mono carboxyisononyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-ethyl phthalate","Monoethyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-2-ethyl-5-hydroxyhexyl","Mono-2-ethyl-5-hydroxyhexyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-2-ethyl-hexyl phthalate","Mono-2-ethylhexyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-isobutyl pthalate","Mono-isobutyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-n-methyl phthalate","Monomethyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-2-ethyl-5-oxohexyl","Mono-2-ethyl-5-oxohexyl phthalate",nhanes$Analyte)
nhanes$Analyte<-gsub("Mono-benzyl phthalate","Monobenzyl phthalate",nhanes$Analyte)
setdiff(nhanes$Analyte, as.character(chemlist_urinaryphthalates$Analyte))
#there are 3 different phthalates in NHANES, I think they are the "old" metabolites
setdiff(as.character(chemlist_urinaryphthalates$Analyte), unique(nhanes$Analyte)) #MiNCH does not have NHANES
nhanes<-merge(nhanes, chemlist_urinaryphthalates[,c("Analyte","Abbreviation","Sort.Order")], by="Analyte", all.x=T)
#remove extra nhanes
nhanes<-nhanes[complete.cases(nhanes),]
nhanes<-nhanes[order(nhanes$Sort.Order),]
#let's reduce to 7-12 year olds
nhanes.kids<-subset(nhanes, agegroup=="Ages 7-12")


urinaryphthalates<-urinaryphthalates[order(urinaryphthalates$Sort.Order),]
nhanes.kids<-nhanes.kids[order(nhanes.kids$Sort.Order),]

jpeg("Results/urinaryphthalatestoNHANES.jpg",
     units="in", width=15, height=7.5, res=300)
boxplot(urinaryphthalates$conc_ngml~urinaryphthalates$Sort.Order,
        log="y", axes=F,
        ylab="Concentration (ng/ml)", cex.lab=1.2)
axis(1, at=1:length(unique(urinaryphthalates$Sort.Order)), 
     labels=unique(as.character(urinaryphthalates$PrettyAbbreviation)), 
     mgp=c(3,0.7,0), cex.axis=1.2)
axis(2, at=c(0.1,1,10,100,1000), 
     labels=c(0.1,1,10,100,1000), 
     las=2, mgp=c(2.5,0.7,0), cex.axis=1.2)
points(nhanes.kids$gm~nhanes.kids$Sort.Order, pch=0, col="blue")
points(nhanes.kids$X0.5~nhanes.kids$Sort.Order, pch=15, col="blue")
points(nhanes.kids$X0.95~nhanes.kids$Sort.Order, pch=8, col="blue")
box()
legend("top", legend=c("NHANES GM","NHANES P50", "NHANES P95"), pch=c(0,15,8), col="blue", ncol=3, inset=0.02)
dev.off()



#wondering if there is a difference between spot and morning void
#### FOR RIGHT NOW, DROP NAs for urinetype
urinaryphthalates<-urinaryphthalates[complete.cases(urinaryphthalates),]
jpeg("results/urinaryphthalates_morningvsspot.jpg", 
     units="in", width=15, height=7.5, res=300)
par(mar=c(5,5,2,2))
fstripchart.c(urinaryphthalates$conc_ngml~urinaryphthalates$Abbreviation, 
              vertical=T, method="jitter", jitter=0.05, pch=NA, log="y",
              ylab="Concentration (ng/ml)")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Boston"&urinaryphthalates$urinetype=="morning"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Boston"&urinaryphthalates$urinetype=="morning"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))-0.2,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="purple")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Boston"&urinaryphthalates$urinetype=="spot"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Boston"&urinaryphthalates$urinetype=="spot"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))-0.1,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="gray")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$urinetype=="morning"]~urinaryphthalates$Sort.Order[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$urinetype=="morning"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))+0.1,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="purple")
fstripchart.c(urinaryphthalates$conc_ngml[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$urinetype=="spot"]~urinaryphthalates$Abbreviation[urinaryphthalates$City=="Cincinnati"&urinaryphthalates$urinetype=="spot"], 
              add=TRUE, at=1:length(unique(urinaryphthalates$Abbreviation))+0.2,
              vertical=T, method="jitter", jitter=0.05, pch=1, log="y", col="gray")
axis(2)
axis(1, at=1:length(unique(urinaryphthalates$Abbreviation)), labels=unique(urinaryphthalates$Abbreviation), las=2, cex.axis=0.6)
legend("top", ncol=4, legend=c("Boston Morning","Boston Spot","Cincinnati Morning","Cincinnati Spot"), pch=c(1,1,0,0), col=c("purple","gray","purple","gray"))
dev.off()


## start linear modeling - first with City in model
metabolites<-unique(urinaryphthalates$Abbreviation)
urinemodel<-data.frame(metabolites=metabolites,VisitType_est=NA,VisitType_p=NA,City_est=NA,City_p=NA, AIC=NA)
for (i in 1:length(unique(urinaryphthalates$Abbreviation))) {
xx<-subset(urinaryphthalates, Abbreviation==unique(urinaryphthalates$Abbreviation)[i])
urinemodel$metabolites[i]<-unique(xx$Abbreviation)
urinemodel$VisitType_est[i]<-summary(lme(log(conc_ngml)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[2,1]
urinemodel$VisitType_p[i]<-summary(lme(log(conc_ngml)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[2,5]
urinemodel$City_est[i]<-summary(lme(log(conc_ngml)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[3,1]
urinemodel$City_p[i]<-summary(lme(log(conc_ngml)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$tTable[3,5]
urinemodel$AIC[i]<-summary(lme(log(conc_ngml)~VisitType+City, data=xx, random=~1|Household.ID, na.action=na.omit))$AIC
}

## now without City as fixed but as nested
metabolites<-unique(urinaryphthalates$Abbreviation)
urinemodel2<-data.frame(metabolites=metabolites,VisitType_est=NA,VisitType_p=NA,AIC=NA)
for (i in 1:length(unique(urinaryphthalates$Abbreviation))) {
  xx<-subset(urinaryphthalates, Abbreviation==unique(urinaryphthalates$Abbreviation)[i])
  urinemodel2$metabolites[i]<-unique(xx$Abbreviation)
  urinemodel2$VisitType_est[i]<-summary(lme(log(conc_ngml)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,1]
  urinemodel2$VisitType_p[i]<-summary(lme(log(conc_ngml)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,5]
  urinemodel2$AIC[i]<-summary(lme(log(conc_ngml)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$AIC
}
#very different results, let's compare AICs

## now without City at all in model
metabolites<-unique(urinaryphthalates$Abbreviation)
urinemodel3<-data.frame(metabolites=metabolites,VisitType_est=NA,VisitType_p=NA,AIC=NA)
for (i in 1:length(unique(urinaryphthalates$Abbreviation))) {
  xx<-subset(urinaryphthalates, Abbreviation==unique(urinaryphthalates$Abbreviation)[i])
  urinemodel3$metabolites[i]<-unique(xx$Abbreviation)
  urinemodel3$VisitType_est[i]<-summary(lme(log(conc_ngml)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,1]
  urinemodel3$VisitType_p[i]<-summary(lme(log(conc_ngml)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$tTable[2,5]
  urinemodel3$AIC[i]<-summary(lme(log(conc_ngml)~VisitType, data=xx, random=~1|City/Household.ID, na.action=na.omit))$AIC
}



#how many urine samples per Household.ID
table(urinaryphthalates$Household.ID)
table(urinaryphthalates$Household.ID, urinaryphthalates$Visit2)
#need to explore the spot v morning void more


############################################################################################
## AIR AND URINE
############################################################################################

#things may have to go wide for correlation analysis
#need to deal with the fact that may have multiple urine samples for IDwithvisit
#just pick one? pick morning if two, preferrably
#will merge air and urine based on IDwithvisit -- Julia fixed those to match the sample inventories, so includes V4 from Cincy

airIDwithvisit<-unique(airphthalates$IDwithvisit)
#don't think there is an exact match here for urine
#need to strip SampleID
urinaryphthalates$IDwithvisit<-gsub("US2","",urinaryphthalates$SampleID)
urinaryphthalates$IDwithvisit<-gsub("US1","",urinaryphthalates$IDwithvisit)
urinaryphthalates$IDwithvisit<-gsub("XX2","",urinaryphthalates$IDwithvisit)

urineIDwithvisit<-unique(urinaryphthalates$IDwithvisit)
intersect(urineIDwithvisit, airIDwithvisit)
#what to do about multiples for urines
IDsandvisits<-unique(urinaryphthalates[,c("IDwithvisit","SampleID","urinetype")])
IDsandvisits<-IDsandvisits[order(IDsandvisits$IDwithvisit),]
extras<-names(table(IDsandvisits$IDwithvisit)[table(IDsandvisits$IDwithvisit)>1])
singles<-IDsandvisits[!(IDsandvisits$IDwithvisit %in% extras),]
notsingles<-IDsandvisits[IDsandvisits$IDwithvisit %in% extras,]
#something wacky with B1033-V3 urinetype
#going to drop all spots (this will drop B1033-V3 since both spots)
toremove<-notsingles[notsingles$urinetype=="spot",]

#removing extras from urinaryphthalates
urinaryphthalates_noextras<-urinaryphthalates[!(urinaryphthalates$SampleID %in% toremove$SampleID),]

urineIDwithvisit<-unique(urinaryphthalates_noextras$IDwithvisit)
IDintersect<-intersect(urineIDwithvisit, airIDwithvisit)

any(!table(urinaryphthalates_noextras$IDwithvisit)==13)
#ready to go


red.phthalate<-airphthalates[,c("IDwithvisit","conc_ngperm3","Abbreviation")]
red.phthalate<-red.phthalate[red.phthalate$IDwithvisit %in% IDintersect,]
wide.air<-reshape(red.phthalate, v.names="conc_ngperm3", idvar="IDwithvisit", timevar="Abbreviation", direction="wide")


red.urine<-urinaryphthalates_noextras[,c("IDwithvisit","conc_ngml","Abbreviation")]
red.urine<-red.urine[red.urine$IDwithvisit %in% IDintersect,]  #reduce to same HHID_Visit
wide.urine<-reshape(red.urine, v.names="conc_ngml", idvar="IDwithvisit", timevar="Abbreviation", direction="wide")


wide.air<-wide.air[order(wide.air$IDwithvisit),]
wide.urine<-wide.urine[order(wide.urine$IDwithvisit),]


colnames(wide.air)<-gsub("conc_ngperm3.","",colnames(wide.air))
colnames(wide.urine)<-gsub("conc_ngml.","",colnames(wide.urine))
#hopefully sorted the same

cor(wide.air[,-1], wide.urine[,-1], method="spearman", use="complete.obs")

corr.est.matrix<-matrix(NA, 6,13)
for (i in 2:7) {
  for (j in 2:14){
    corr.est.matrix[i-1,j-1]<-cor.test(wide.air[,i], wide.urine[,j], method="spearman")$est
  }
}
colnames(corr.est.matrix)<-colnames(wide.urine[,2:14])
rownames(corr.est.matrix)<-colnames(wide.air[,2:7])
corr.est.matrix<-round(corr.est.matrix,2)

corr.pval.matrix<-matrix(NA, 6,13)
for (i in 2:7) {
  for (j in 2:14){
    corr.pval.matrix[i-1,j-1]<-cor.test(wide.air[,i], wide.urine[,j], method="spearman")$p.value
  }
}
colnames(corr.pval.matrix)<-colnames(wide.urine[,2:14])
rownames(corr.pval.matrix)<-colnames(wide.air[,2:7])
corr.pval.matrix<-round(corr.pval.matrix,2)

sig.corrs<-corr.est.matrix[corr.pval.matrix<0.05]


plot(wide.urine$mEP2~wide.air$DEP, log="xy", xlab="DEP Concentration in Air (ng/m3)", ylab="MEP Concentration in Urine (ng/ml)")
plot(wide.urine$mBP~wide.air$DBP, log="xy", xlab="DBP Concentration in Air (ng/m3)", ylab="MBP Concentration in Urine (ng/ml)")
plot(wide.urine$mBzP2~wide.air$BBP, log="xy", xlab="BBzP Concentration in Air (ng/m3)", ylab="MBzP Concentration in Urine (ng/ml)")
