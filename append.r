
# append.r
# creates one file with selected variables for analysis

# select variables 
basesmall<-cbind(as.character(base$Q2_lga), as.character(base$Q3_ward), base$villageNo,
                 base$ageChildCalcDays, base$ageChildCalcMonths, base$calcUpToDate,
                 base$neverhadvacc, base$immCardSeen, base$numPenta, base$agePenta1,
                 base$agePenta2, base$agePenta3,base$ageMeasles, base$tetanusVacc, base$HFdistance, base$hardToReach, 
                 base$femaleChild)
midsmall<-cbind(as.character(mid$Q2_lga), as.character(mid$Q3_ward), mid$villageNo, 
                 mid$ageChildCalcDays, mid$ageChildCalcMonths,  mid$calcUpToDate, 
                 mid$neverhadvacc, mid$immCardSeen, mid$numPenta, mid$agePenta1, 
                 mid$agePenta2, mid$agePenta3, mid$ageMeasles, mid$tetanusVacc, mid$HFdistance, mid$hardToReach,
                 mid$femaleChild)
finalsmall<-cbind(as.character(final$Q1.LGA), as.character(final$Q2.Ward), final$villageNo, 
                 final$ageChildCalcDays, final$ageChildCalcMonths,final$calcUpToDate,
                 final$neverhadvacc, final$immCardSeen, final$numPenta, final$agePenta1,
                 final$agePenta2, final$agePenta3, final$ageMeasles, final$tetanusVacc, final$HFdistance, final$hardToReach,
                 final$femaleChild)


basesmall<-as.data.frame(basesmall)
midsmall<-as.data.frame(midsmall)
finalsmall<-as.data.frame(finalsmall)
basesmall$survey<-1
midsmall$survey<-2
finalsmall$survey<-3

allsmall<-rbind(basesmall, midsmall, finalsmall)
colnames(allsmall)<-c("lga","ward","village","ageChildCalcDays","ageChildCalcMonths","calcUpToDate", "neverhadvacc","immCardSeen",
		          "numPenta","agePenta1","agePenta2","agePenta3","ageMeasles","tetanusVacc","HFdistance","hardToReach",
                      "femaleChild","survey")

allsmall$rlga<-as.numeric(allsmall$lga)
allsmall$rward<-as.numeric(allsmall$ward)
allsmall$rvillage<-allsmall$village


# create variables

# fully up to date
allsmall$uptodate<-0
allsmall$uptodate[allsmall$VacStatus=="up-to-date"]<-1
allsmall$atleastpar<-1
allsmall$atleastpar[allsmall$VacStatus=="Not vaccinated"]<-0

# at least one vaccination
allsmall$calcAtLeastPar <- 1
allsmall$calcAtLeastPar[allsmall$neverhadvacc==1]<-0

# vaccine status categories (0=never had any vaccines, 1=partial,2=fully up to date)
allsmall$status<-1
allsmall$status[allsmall$neverhadvacc==1]<-0
allsmall$status[allsmall$calcUpToDate==1]<-2

# intervention arm
allsmall$researchArm <- 0
allsmall$researchArm[allsmall$lga=="biase"]<-1
allsmall$researchArm[allsmall$lga=="etung"]<-1
allsmall$researchArm[allsmall$lga=="ikom"]<-1
allsmall$researchArm[allsmall$lga=="obudu"]<-1

allsmall$interv<-0
allsmall$interv[allsmall$researchArm==1]<-1

# categorical variable for survey and interv
allsmall$intervSurvey<-(allsmall$interv*10)+allsmall$survey


# indicator for intervention arm at time of survey 2
allsmall$interv2<-0
allsmall$interv2[allsmall$survey==2 & allsmall$lga=="biase"]<-1
allsmall$interv2[allsmall$survey==2 & allsmall$lga=="etung"]<-1
allsmall$interv2[allsmall$survey==2 & allsmall$lga=="ikom"]<-1
allsmall$interv2[allsmall$survey==2 & allsmall$lga=="obudu"]<-1

# indicator for intervention arm at time of survey 3
allsmall$interv3<-0
allsmall$interv3[allsmall$survey==3 & allsmall$lga=="biase"]<-1
allsmall$interv3[allsmall$survey==3 & allsmall$lga=="etung"]<-1
allsmall$interv3[allsmall$survey==3 & allsmall$lga=="ikom"]<-1
allsmall$interv3[allsmall$survey==3 & allsmall$lga=="obudu"]<-1

# ages are factors, change to numeric
allsmall$ageChildCalcMonths<-as.numeric(as.character(allsmall$ageChildCalcMonths))
allsmall$ageChildCalcDays<-as.numeric(as.character(allsmall$ageChildCalcDays))



