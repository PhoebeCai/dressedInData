#Started Sept 2014
#L Perovich
#Code to turn GHS data into shirts
#Moving to a stripe print pattern instead of a lace square pattern

#Exploring screen printing, laser cutting, and other options
#Multiple design persepctives.

library(plyr)
library(RColorBrewer)

#################################################################
##################### DATA CLEANING #############################
#################################################################
#air data
air1<-read.csv("Desktop/GHSdataSept2014/GHSCIdata.csv")
air2<-read.csv("Desktop/GHSdataSept2014/GHSEIdata.csv")
chemlist_air<-read.csv("Desktop/GHSdataSept2014/chemlist_air.csv", stringsAsFactors=FALSE)
#urine data
urine1<-read.csv("Desktop/GHSdataSept2014/phenoldata.csv")
urine2<-read.csv("Desktop/GHSdataSept2014/phthalatedata.csv")
#NHANES urine data for kids (7-12?)
nhanes<-read.csv("Desktop/GHSdataSept2014/NHANES.Phthalates.0910.Levels.csv")

###  AIR CLEANING NAMES
#drop BEHTBP and d4.di.n.butyl.phthalate
chemlist_air<-subset(chemlist_air, !(Analyte=="BEHTBP"|Analyte=="d4.di.n.butyl.phthalate"))
chemlist_air$Order<-NULL
chemlist_air$Abbreviation<-chemlist_air$PrettyAbbrev
chemlist_air$PrettyAbbrev<-NULL
#drop BEHTBP and d4.di.n.butyl.phthalate from air
chemlist_air<-subset(chemlist_air, !(Analyte=="BEHTBP"|Analyte=="d4.di.n.butyl.phthalate"))
#sort order to phthalates
sorting.air<-data.frame(Abbreviation=c("DEP", "DBP", "BBP", "DEHP", "DCHP", "DINP"), Sort.Order.Air=1:6)
chemlist_air<-merge(chemlist_air, sorting.air, by="Abbreviation", all.x=TRUE)

#Tack everthing together
colnames(air1)==colnames(air2)
air3<-rbind(air1, air2)
colnames(urine1)==colnames(urine2)
urine3<-rbind(urine1, urine2)

#AIR OTHER CLEANING TO PHTHALATES
air3<-merge(air3, chemlist_air, by="Analyte", all.x=T)
air3$VisitType<-gsub(" ","",air3$VisitType) #remove weird space
#fix cities
air3$City<-as.character(air3$City)
air3$VisitType<-ifelse(air3$IDwithvisit=="C1017-V4","Green",air3$VisitType) #missing VisitType
air3$City<-ifelse(air3$IDwithvisit=="C1017-V4","Cincinnati",air3$City) #missing City

#URINE CLEANING
#### info on parents and metabolites
#merge in parents and metabolites 
parentmetabs<-data.frame(parent=c("DINCH","DBP","BBP","BBP","DiDP","DINP","DOP","DEHP","DEHP","DEHP","DEHP","DEP","DMP","DIBP"),
                         metab=c("MHiNCH","mBP","mBP","mBzP2","mCNP","mCOP","mCPP","mECPP","mEHHP","mEHP","mEOHP","mEP2","mMP","miBP"))

##names for urine data
chemlist_urinaryphthalates<-data.frame(Abbreviation=c("mBP","mBzP2","mCNP","mCOP","mCPP","mECPP","mEHHP","mEHP","mEOHP","mEP2","MHiNCH","miBP","mMP"),
                                       PrettyAbbreviation=c("MBP","MBzP","MCNP","MCOP","MCPP","MECPP","MEHHP","MEHP","MEOHP","MEP","MHiNCH","MiBP","MMP"),
                                       Analyte=c("Mono-n-butyl phthalate","Monobenzyl phthalate","Mono carboxyisononyl phthalate","Mono carboxyisooctyl phthalate","Mono-3-carboxypropyl phthalate","Mono-2-ethyl-5-carboxypentyl phthalate","Mono-2-ethyl-5-hydroxyhexyl phthalate","Mono-2-ethylhexyl phthalate","Mono-2-ethyl-5-oxohexyl phthalate","Monoethyl phthalate","Cyclohexane-1 2-dicarboxylic acid monohydroxy isononyl ester","Mono-isobutyl phthalate","Monomethyl phthalate"))
#keep this somewhere
#write.csv(chemlist_urinaryphthalates, "Desktop/GHSdataSept2014/chemlistUrinePhthalates.csv", row.names=FALSE)
#merge in the names
urine3<-merge(urine3, chemlist_urinaryphthalates, by=c("Abbreviation", "Analyte"), all.x=TRUE)


#################################################################
############## NOTES ABOUT THE DATA #############################
#################################################################
#includes non detects at zero and negatives
#no flags or detection limits yet
#different n for different time points (and media)

#both spot urine and first morning urine: different outcomes?  some people may have both

#multiple time points per person==watch for ID uniqueness and sample count
#unclear what variables are relevant to differents
#e.g. are Cinci and Boston controls the same?
#green buildings
#sampling was done by different people in different cities
#time points might be pretty similar: multiple measurements of the same thing?


#################################################################
##################### DATA SUMMARY ##############################
#################################################################

#AIR SUMMARY
#split by chemical, city, and visit occasion
air3.summary<-ddply(air3, c("Analyte","City","VisitType", "Abbreviation"), summarise,
                    N=sum(!is.na(conc_ngperm3)), 
                    detfreq=signif((sum(!(conc_ngperm3==0))/sum(!is.na(conc_ngperm3)))*100,2),
                    min=signif(min(conc_ngperm3),3),
                    mean=signif(mean(conc_ngperm3),3),
                    median=signif(median(conc_ngperm3),3),
                    P95=signif(quantile(conc_ngperm3, p=0.95),3),
                    max=signif(max(conc_ngperm3),3))
air3.summary<-merge(air3.summary,sorting.air[,c("Abbreviation","Sort.Order.Air")], by="Abbreviation", all.x=T)
#just split by chemical
air3.summary.chem<-ddply(air3, c("Analyte", "Abbreviation"), summarise,
                         Nchem=sum(!is.na(conc_ngperm3)), 
                         detfreqchem=signif((sum(!(conc_ngperm3==0))/sum(!is.na(conc_ngperm3)))*100,2),
                         minchem=signif(min(conc_ngperm3),3),
                         meanchem=signif(mean(conc_ngperm3),3),
                         medianchem=signif(median(conc_ngperm3),3),
                         P95chem=signif(quantile(conc_ngperm3, p=0.95),3),
                         maxchem=signif(max(conc_ngperm3),3))
#put it all together
air3.summary<-merge(air3.summary, air3.summary.chem, by=c("Analyte", "Abbreviation"))

#Put this info into the (now huge) main dataset
air4<-merge(air3, air3.summary, by=c("Analyte", "City","VisitType", "Abbreviation"))
#Get rid of some stuff (can change later!)
air4<-air4[, setdiff(colnames(air4), c("StartDate", "StartTime", "StopDate", "StopTime", "Sort.Order.Air.y"))]


#Use this info to calculate percentiles
#======>ISSUE.  percent of the maximum is not the percentile
#What is best to use here?
#plan: rank them then divide by the number of points in the distribution.
#compare to alternative approaches and results...

#THINK ABOUT WHETHER WE WANT ONLY SOME VISITS OR OTHER SUBSETTING FIRST!!
#Is there weird weighting going on?

air5<-split(air4, air4$Analyte)

for (i in 1:length(air5)){
  air5[[i]]<-air5[[i]][order(air5[[i]]$conc_ngperm3), ]
  #Deal with non-detect ties, but just let other ties go
  air5[[i]]$rank<-1:nrow(air5[[i]])
  air5[[i]]$rank<-ifelse(air5[[i]]$conc_ngperm3==0, 0, air5[[i]]$rank)
}

air6<-unsplit(air5, air4$Analyte)
nrow(air4)==nrow(air6)


air6$percentile<-round(air6$rank/air6$Nchem*100)


#!!!






#urine summary data
urine.summary<-ddply(urine3, c("Analyte", "Abbreviation","City","VisitType"), summarise,
                     N=sum(!is.na(conc_ngml)), 
                     detfreq=signif((sum((LOD==""))/sum(!is.na(conc_ngml)))*100,2),
                     min=signif(min(conc_ngml),3),
                     mean=signif(mean(conc_ngml),3),
                     median=signif(median(conc_ngml),3),
                     P95=signif(quantile(conc_ngml, p=0.95),3),
                     max=signif(max(conc_ngml),3))
urine.summary<-merge(urine.summary,chemlist_urinaryphthalates[,c("Abbreviation","PrettyAbbreviation","Sort.Order")], by="Abbreviation", all.x=T)
urine.summary<-urine.summary[order(urine.summary$Sort.Order),]

urinaryphthalates<-merge(urinaryphthalates, chemlist_urinaryphthalates, by="Abbreviation", all.x=T)

#################################################################
############## SUMMARY DATA OBSERVATIONS ########################
#################################################################
#AIR
#Many chemicals have high detection rates--a few have under 10%
#Not too many chemicals all together in air (41)
#unclear what the structured & well populated classes are beyond phthalates...
#lots in there, including PCBs, parabens, fragrances, antimicrobials, phenols, propanol, FR...


#################################################################
#################### INDIVIDUALIZED DATA ########################
#################################################################
personPick<-function(data, houseID, visit){
  data2<-subset(data, data$Household.ID %in% houseID, drop=TRUE)
  data2<-subset(data2, data2$Visit %in% visit, drop=TRUE)
  data2<-data2[, c("Household.ID", "Analyte", "percentile", "Visit", "conc_ngperm3", "medianchem", "Group")]
  data2<-data2[order(data2$Analyte, data2$Visit), ]
  data3<-split(data2, data2$Group)
  data3<-unique(data3)
  return(data3)
  #returns a list of rankings etc seperated by group
}



#################################################################
############## PICKING A PERSON/APPROACH ########################
#################################################################
#Want a Cincy person(s)
#Use their percentile ranking
#Only use each ranking once (per side?)
#visits are V2, V3, V4
#chemgroups are pcp, flame retardant, phthalate, pcb (and NA)
#IDs are letter plus 4 digit number
IDs<-unique(air6$Household.ID)

#**Look at timeCounts dataset (later in code) to see how many timepoints each household.id has

partA<-personPick(air6, c("C1026"), c("V2", "V3", "V4"))
#has just V3
#write.csv(partA[[1]], "Desktop/partA1.csv", row.names=FALSE)

partB<-personPick(air6, c("C1038"), c("V2", "V3", "V4"))
#has V2 and V3
#has some high FR rankings
#seems like a big difference between V2 and V3 FR--some going way down, others way up (percentile)
#many PCBs are way up at V3 too (ND to D)
#pcp seem pretty consistent; phthalates also consistent, except one chemical spiking
#write.csv(partB[[2]], "Desktop/partB2.csv", row.names=FALSE)


partC<-personPick(air6, c("C1008"), c("V2", "V3", "V4"))
#just has V4
#highish FR, no PCB, highish pcp, highish phthalate

partD<-personPick(air6, c("C1044"), c("V2", "V3", "V4"))
#has V2 and V3
#FR go WAY down V2 to V3!! (many have over 50% detect)
#some PCBs also go way down
#many PCP go way down
#many phthalates go way down

partE<-personPick(air6, c("C1015"), c("V2", "V3", "V4"))
#just V4
#no PCB
#middle/high or ND on FR
#spread on PCP
#middle on phthalates

partJ<-personPick(air6, c("C1032"), c("V2", "V3", "V4"))
#has V2 and V3
#FR generally pretty consistent
#no pcb
#pcp and phthalate also fairly consistent

#Boston person
partF<-personPick(air6, c("B1023"), c("V2", "V3", "V4"))
#V2 and V3
#no PCB
#many FR go from high to ND
#same with PCP
#phthalates are a mixed bag

#Boston person
partG<-personPick(air6, c("B1011"), c("V2", "V3", "V4"))
#only V3
#fairly middle of the road for FR
#no PCBs
#fairly middle of the road for PCP
#highish on phthalates

#Boston person
partH<-personPick(air6, c("B1003"), c("V2", "V3", "V4"))
#only V3
#range in FR
#no PCB
#slightly high PCP
#range in phthalate

#Boston person
partI<-personPick(air6, c("B1033"), c("V2", "V3", "V4"))
#only V3
#some high FR
#no pcb
#high PCP
#high phthalate (some ND)

#################################################################
###################### GRAPHS GENERAL ###########################
#################################################################
#want to group by chemical (e.g only one timepoint)
#Probably want only one chem group

#Max timepoints per person in air
a6<-unique(air6[,c("Household.ID", "Visit")])
timeCheck<-split(a6, a6$Household.ID)
tcount<-c(1:length(timeCheck))
for (i in 1:length(timeCheck))
  tcount[[i]]<-nrow(timeCheck[[i]])
timeCounts<-data.frame(names(timeCheck), tcount)
colnames(timeCounts)<-c("Houshold.ID", "nTime")
maxTime<-max(timeCounts$nTime, na.rm=TRUE)

#Max chemicals per group in air
maxChem<-max(sum(chemlist_air$Group=="flame retardant"), sum(chemlist_air$Group=="pcp"), sum(chemlist_air$Group=="pcb"), 
             sum(chemlist_air$Group=="phthalate"), na.rm=TRUE)


#################################################################
###################### STRIPE GRAPHS ############################
#################################################################
#Series of horizontal or vertical lines
#Each line has a max thickness space
#Small break space between time points
#(how to deal with only one vs two timepoints? ND verse not measured)
#keep the width of the stripes constant between classes (just goes longer on the sleeve)
#max sleeve length is ~22"
#Maximum number of timepoints for one individual in air is: 2
#Maximum number of chemicals is: 18
#so the most stripes you'd have is 36 spread across ~22"....
#VERSION 1: each stripe gets 1/2" plus 1/8" bettween chemicals
#VERSION 2: only one timepoint, each strip gets 1", no gap between chemicals

#Can do some cropping etc in corel draw


#max wdith of section
stripeWidthOneTime<-1
stripeWidthTwoTime<-.5
stripeGapOneTime<-.2
stripeGapTwoTime<-.2
plotInches<-12

#dataset mapping ranks and timepoints to colors
#CHANGE THIS LATER
#four "neural" colors, fade in intensity (grey, blue, purple, pink)
stripeColorData<-data.frame(c(rep("V1", 3), rep("V2", 3), rep("V3", 3), rep("V4", 3)), c("low", "mid", "high"), 
                            c(brewer.pal(3, "BuPu"), brewer.pal(3, "Greys"), brewer.pal(3, "Blues"), brewer.pal(3, "RdPu")))
colnames(stripeColorData)<-c("Visit", "stripeColorRank", "color")


stripePlot<-function(data, file, stripeWidth, stripeGap, colorData, sortType) {
  #data is the dataset (not list) for the individual to make the plot
  #sortType can be byTime, byAmount (not recommended for multiple visit data?), byAmountTimeV2, byAmountTimeV3
  #has percentil data
  #xmax is the 
  #Prep the data
  ntime<-length(unique(data$Visit))
  nchem<-length(unique(data$Analyte))
  #should be ntime* nchem
  nstripes<-nrow(data) 
  person<- unique(as.character(data$Household.ID))
  chemClass<- unique(data$Group)
  
  #get the stripe widths into the dataset
  data$stripeWidth<-signif(stripeWidth*data$percentile/100, 2)
  #get the colors of the stripes into the dataset
  #three colors per timePoint; four sort of theoretical timepoints
  data$stripeColorRank<-ifelse(data$percentile<33, "low", ifelse(data$percentile<66, "mid", "high"))
  data<-merge(data, colorData, by=c("Visit", "stripeColorRank"))
  
  #Resort!!  How do I want these ordered??
  #right now they are grouped by amount---I think we want same order from person to person?????
  if (sortType=="byTime")
    data<-data[order(data$Analyte, data$Visit, -data$percentile), ]
  if (sortType=="byAmount")
    data<-data[order(-data$percentile, data$Analyte, data$Visit), ]
  
  ##### !!!!!!!!!!!!
  if (sortType=="byAmountTimeV2"){
    #Pull out time2s
    V2order<-subset(data, data$Visit=="V2")
    #order them
    V2order<-V2order[order(-V2order$percentile),]
    #rank chemicals
    V2order$chemRank<-1:nrow(V2order)
    V2chemrank<-V2order[,c("Analyte", "chemRank")]
    #merge back into database
    data<-merge(data, V2chemrank, by="Analyte")
    #sort on this column
    data<-data[order(data$chemRank, data$Visit),]
    #drop this column
    data$chemRank<-NULL
  }
  
  if (sortType=="byAmountTimeV3"){
    #Pull out time3s
    V3order<-subset(data, data$Visit=="V3")
    #order them
    V3order<-V2order[order(-V3order$percentile),]
    #rank chemicals
    V3order$chemRank<-1:nrow(V3order)
    V3chemrank<-V3order[,c("Analyte", "chemRank")]
    #merge back into database
    data<-merge(data, V3chemrank, by="Analyte")
    #sort on this column
    data<-data[order(data$chemRank, data$Visit),]
    #drop this column
    data$chemRank<-NULL
  }
  
  jpeg(file, width=plotInches, height=plotInches, units="in", quality=80, res=72)
  #lets make the stripes vertical
  plot(0, 0, xlim=c(0, 21), ylim=c(0, 1), main = paste(person, chemClass), col="white")
  rectXleftedge<-0
  
  #Note: zeros are plotted as lines right now.
  
  if (ntime == 1){
    for (i in 1:nstripes){
      #draw a rectangle, set the fill color appropriately or label it with the right color
      #xleft, ybottom, xright, ytop 
      rect(rectXleftedge, 0, rectXleftedge+data$stripeWidth[[i]], 1, col=as.character(data$color[[i]]))
      #  abline(v= , lwd) #seems like setting line widths accurately with lwd will be tricky, use rectangles instead
      rectXleftedge<-rectXleftedge + stripeWidth + stripeGap
    }
  }
  
  if (ntime == 2){
    #***Make paired stripes touch almost!!  make them one stripe basically with big gap to next one
    #Force them to be sorted so matching chemicals are next to each other---NOPE!
    #  data<-data[order(data$Analyte, data$Visit, -data$percentile), ]
    for (i in seq(1, nstripes, 2)){
      #Take every other point, since we'll draw two at a time
      #Must use skinner stripes
      #Draw the rectangle for the first timepoint  
      rect(rectXleftedge, 0, rectXleftedge+data$stripeWidth[[i]], 1, col=as.character(data$color[[i]]))
      #also draw the next rectangle right next to it (which is the same chemical)
      rect(rectXleftedge+data$stripeWidth[[i]], 0, rectXleftedge+data$stripeWidth[[i]]+data$stripeWidth[[i+1]], 1, col=as.character(data$color[[i+1]]))
      rectXleftedge<-rectXleftedge + stripeWidth*2 + stripeGap
    }
  }
  
  dev.off()
}

#NOTE: all the stripes clumped by amount is an interesting effect...but maybe not good


#Try
for (i in 1:length(partA))
  stripePlot(partA[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partA", i, ".jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byAmount")
for (i in 1:length(partC))  
  stripePlot(partC[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partC", i, ".jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byAmount")
for (i in 1:length(partE))  
  stripePlot(partE[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partE", i, ".jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byAmount")
for (i in 1:length(partG))  
  stripePlot(partG[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partG", i, ".jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byAmount")
for (i in 1:length(partH))  
  stripePlot(partH[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partH", i, ".jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byAmount")
for (i in 1:length(partI))  
  stripePlot(partI[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partI", i, ".jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byAmount")


#These are ordered by analyte name (randomlyish)
for (i in 1:length(partA))
  stripePlot(partA[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partA", i, "A.jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byTime")
for (i in 1:length(partC))  
  stripePlot(partC[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partC", i, "A.jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byTime")
for (i in 1:length(partE))  
  stripePlot(partE[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partE", i, "A.jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byTime")
for (i in 1:length(partG))  
  stripePlot(partG[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partG", i, "A.jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byTime")
for (i in 1:length(partH))  
  stripePlot(partH[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partH", i, "A.jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byTime")
for (i in 1:length(partI))  
  stripePlot(partI[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partI", i, "A.jpg", sep=""), stripeWidthOneTime, stripeGapOneTime, stripeColorData, "byTime")


for (i in 1:length(partB))  
  stripePlot(partB[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partB", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
for (i in 1:length(partJ))  
  stripePlot(partJ[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partJ", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
for (i in 1:length(partD))  
  stripePlot(partD[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partD", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
for (i in 1:length(partF))  
  stripePlot(partF[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partF", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")

#I also need the one-stripe only versions of these!
#mess with function and rename
#for (i in 1:length(partB))  {
#  stripePlot(partJ[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partJ1", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
#  stripePlot(partJ[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partJ2", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
#  stripePlot(partD[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partD1", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
#  stripePlot(partD[[i]], paste("Desktop/GHSdataSept2014/plots/stripePlots/partD2", i, ".jpg", sep=""), stripeWidthTwoTime, stripeGapTwoTime, stripeColorData, "byAmountTimeV2")
#}  

#also cut a dotted line mask

#QUESTIONNAIRE INVESTIGATIONS
#HOW DOES SORT ORDER INFLUENCE ABILITY TO ESTIMATE WHAT IS HAPPENING????



data<-partA[[1]]
stripeWidth<-stripeWidthOneTime
stripeGap<-stripeGapOneTime
colorData<-stripeColorData
sortType<-"byAmount"

#################################################################
############# PHOEBE: LINE/CIRCLE GRAPHS ########################
#################################################################

#make a blank plot with the right dimensions
#stripeGapSm = 1.5, stripeGapLg = 4
linePlot = function(data, file, stripeWidth = 2, outerRad = 1.25, innerRad = 0.5, #stripeGap = 0.5, 
                    #stripeGapSm = .5, stripeGapLg = 1.5,
                    sortType) {
  
  ntime<-length(unique(data$Visit))
  #LP changes a bit: used to be 13 x 18
  plotWidth = 13.4
  plotHeight = 19
  jpeg(file, width=plotWidth, height=plotHeight, units="in", quality=100, res=150)  
  
  
  person<- unique(as.character(data$Household.ID))
  chemClass <- unique(as.character(data$Group))
  #  plot(0, 0, xlim=c(0, 51), ylim=c(0, 101), main = paste(person, chemClass), col="white")
  
  rectXleftedge = 1
  
  starsDims = function(rad, starsDim) {
    starsMatrix = matrix(rep(rad, starsDim), nrow=1, ncol=starsDim)
    return(starsMatrix)
  }
  
  data$stripeColorRank<-ifelse(data$percentile<33, "green", ifelse(data$percentile<66, "yellow", "red"))
  data$visitColor<-ifelse(data$Visit=="V1", "#9EBCDA", ifelse(data$Visit=="V2", "#BDBDBD", ifelse(data$Visit=="V3", "#9ECAE1", "#FA9FB5")))
  data$starsType<- ifelse(data$percentile<33, 4, ifelse(data$percentile<66, 6, 8))
  
  
  
  #Resort!!  How do I want these ordered??
  #right now they are grouped by amount---I think we want same order from person to person?????
  if (sortType=="byTime")
    data<-data[order(data$Analyte, data$Visit, -data$percentile), ]
  if (sortType=="byAmount")
    data<-data[order(-data$percentile, data$Analyte, data$Visit), ]
  
  ##### !!!!!!!!!!!!
  if (sortType=="byAmountTimeV2"){
    #Pull out time2s
    V2order<-subset(data, data$Visit=="V2")
    #order them
    V2order<-V2order[order(-V2order$percentile),]
    #rank chemicals
    V2order$chemRank<-1:nrow(V2order)
    V2chemrank<-V2order[,c("Analyte", "chemRank")]
    #merge back into database
    data<-merge(data, V2chemrank, by="Analyte")
    #sort on this column
    data<-data[order(data$chemRank, data$Visit),]
    #drop this column
    data$chemRank<-NULL
  }
  
  if (sortType=="byAmountTimeV3"){
    #Pull out time3s
    V3order<-subset(data, data$Visit=="V3")
    #order them
    V3order<-V2order[order(-V3order$percentile),]
    #rank chemicals
    V3order$chemRank<-1:nrow(V3order)
    V3chemrank<-V3order[,c("Analyte", "chemRank")]
    #merge back into database
    data<-merge(data, V3chemrank, by="Analyte")
    #sort on this column
    data<-data[order(data$chemRank, data$Visit),]
    #drop this column
    data$chemRank<-NULL
  }
  
  
  
  
  #draw rectangles.  the height of each rectangle is it's percentile.  the color is related to its visit number (or media)
  nlines = nrow(data)
  if(ntime == 1){
    #xlim previously c(0, 51)
    plot(0, 0, xlim=c(0, 78), ylim=c(0, 101), main = paste(person, chemClass), col="white")
    stripeGap = (78 - stripeWidth*nlines)/nlines
    for (i in 1:nlines){
      #xleft, ybottom, xright, ytop 
      rect(rectXleftedge, 0, rectXleftedge+stripeWidth, data$percentile[i], col=as.character(data$visitColor[i]), border = NA)
      x = rectXleftedge+0.5*stripeWidth
      y = data$percentile[i] - 0.5
      symbols(x, y, circles = outerRad*stripeWidth, fg = as.character(data$visitColor[i]), bg = as.character(data$visitColor[i]), add = TRUE, inches = FALSE)
      symbols(x, y, circles = innerRad*stripeWidth, fg = as.character(data$stripeColorRank[i]), bg = as.character(data$stripeColorRank[i]), add = TRUE, inches = FALSE)  
      #  abline(v= , lwd) #seems like setting line widths accurately with lwd will be tricky, use rectangles instead
      rectXleftedge<-rectXleftedge + stripeWidth + stripeGap
    }
  }
  if (ntime == 2){
    #xlim was c(0, 121)
    plot(0, 0, xlim=c(0, 78), ylim=c(0, 101), main = paste(person, chemClass), col="white")
    #***Make paired stripes touch almost!!  make them one stripe basically with big gap to next one
    stripeGapSm = 0.5*(78 - stripeWidth*nlines)/nlines
    stripeGapLg = 1.5*(78 - stripeWidth*nlines)/nlines
    for (i in seq(1, nlines, 2)){
      #Take every other point, since we'll draw two at a time
      #Must use skinner stripes
      #Draw the rectangle for the first timepoint  
      rect(rectXleftedge, 0, rectXleftedge+stripeWidth, data$percentile[i], col=as.character(data$visitColor[i]), border = NA)
      x1 = rectXleftedge+0.5*stripeWidth
      y1 = data$percentile[i]
      symbols(x1, y1, circles = outerRad*stripeWidth, fg = as.character(data$visitColor[i]), bg = as.character(data$visitColor[i]), add = TRUE, inches = FALSE)
      symbols(x1, y1, circles = innerRad*stripeWidth, fg = as.character(data$stripeColorRank[i]), bg = as.character(data$stripeColorRank[i]), add = TRUE, inches = FALSE)  
      
      #also draw the next rectangle right next to it (which is the same chemical)
      rect(rectXleftedge+stripeWidth+stripeGapSm, 0, rectXleftedge+2*stripeWidth+stripeGapSm, data$percentile[i+1], col=as.character(data$visitColor[i+1]), border = NA)
      x2 = rectXleftedge+1.5*stripeWidth+stripeGapSm
      y2 = data$percentile[i+1]
      symbols(x2, y2, circles = outerRad*stripeWidth, fg = as.character(data$visitColor[i+1]), bg = as.character(data$visitColor[i+1]), add = TRUE, inches = FALSE)
      symbols(x2, y2, circles = innerRad*stripeWidth, fg = as.character(data$stripeColorRank[i+1]), bg = as.character(data$stripeColorRank[i+1]), add = TRUE, inches = FALSE)  
      
      
      rectXleftedge<-rectXleftedge + stripeWidth*2 + stripeGapSm + stripeGapLg
    }
  }
  
  dev.off()
}


#We want to use FR from participant C for the scarf demo
for (i in 1:length(partC))  
  linePlot(partC[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partC", i, "OLP.jpg", sep=""), sortType="byAmount")


for (i in 1:length(partA))
  linePlot(partA[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partA", i, "O.jpg", sep=""), sortType="byAmount")
for (i in 1:length(partC))  
  linePlot(partC[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partC", i, "O.jpg", sep=""), sortType="byAmount")
for (i in 1:length(partE))  
  linePlot(partE[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partE", i, "O.jpg", sep=""), sortType="byAmount")
for (i in 1:length(partG))  
  linePlot(partG[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partG", i, "O.jpg", sep=""), sortType="byAmount")
for (i in 1:length(partH))  
  linePlot(partH[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partH", i, "O.jpg", sep=""), sortType="byAmount")
for (i in 1:length(partI))  
  linePlot(partI[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partI", i, "O.jpg", sep=""), sortType="byAmount")


for (i in 2:length(partB))  
  linePlot(partB[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partB", i, "O.jpg", sep=""), sortType="byAmountV2")
for (i in 2:length(partJ))  
  linePlot(partJ[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partJ", i, "O.jpg", sep=""), sortType="byAmountV2")
for (i in 2:length(partD))  
  linePlot(partD[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partD", i, "O.jpg", sep=""), sortType="byAmountV2")
for (i in 2:length(partF))  
  linePlot(partF[[i]], paste("Desktop/GHSdataSept2014/plots/scarfPlots/partF", i, "O.jpg", sep=""), sortType="byAmountV2")

#################################################################
###################### NHANES CODE ##############################
#################################################################

### NHANES code 
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




#################################################################
####################### GENERAL #################################
#################################################################
#Decide what to do with non detects
#Subset chemical groups
#Order chemicals
#Pick line spacing (e.g. set framework to fill in)



#################################################################
#################### VALUES AS WIDTHS ###########################
#################################################################
#Pick constant line length
#Change concentrations into widths
#"bracket" it in somehow (know where it starts/ends)
#Divide into color patterns to better see missing values? (e.g. R G B)


#################################################################
#################### VALUES AS LENGTHS ##########################
#################################################################
#Pick constant line width
#Change concentrations into lengths









#####################################################################
#HAND MADE DRAFTS
#two people via a few methods?
setwd("/media/My Passport/GHS data")

library(plyr)

#This is good concept, but in reality will probably have more chemicals per class
#As a placeholder, use two time points (just pretending they are actually different chemicals)

#Pull in the data (from external harddrive!!)
data<-read.csv("phthalatedataLP.csv")

#arbitrarily pick timepoint V2
#dataV2<-subset(data, data$Visit=="V2")
dataV2<-subset(data, data$Visit!="V4")

#get the summary data for scaling purposes if needed
summaryData<-ddply(dataV2, .(Analyte), summarise, maxAmt=max(conc_ngperm3, na.rm=TRUE), medianAmt=median(conc_ngperm3, na.rm=TRUE), minAmt=min(conc_ngperm3, na.rm=TRUE), ndet=sum(conc_ngperm3==0, na.rm=TRUE))

dataV2<-merge(dataV2, summaryData, by="Analyte")

#Select one cincinati participant to make a test case of
part1<-subset(dataV2, dataV2$Household.ID=="C1038")
part1$scaledConc<-round(part1$conc_ngperm3/part1$maxAmt*100)


conc_ngperm3


