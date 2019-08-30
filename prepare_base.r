
# prepare_base.r
# prepares baseline survey data - standardize variable names, create indicators and categories


# lga, ward, village as numeric
base$lgaNum<-as.numeric(base$Q2_lga)
base$wardNum<-as.numeric(base$Q3_ward)
base$villageNum<-base$villageNo

# intervention arm indicator
base$interv<-0
base$interv[base$researchArm=="intervention"]<-1

# AGE OF CHILDREN (create variables ageChildCalcDays, ageChildCalcMonths)
# turn string dates into R date format
dtemp<-as.character(base$date_of_interview)
dday<-as.numeric(substring(dtemp,1,2))
dmonth<-substring(dtemp,5,7)
dyear<-as.numeric(substring(dtemp,9,10))
base$dateInterview<-ISOdate(2016,12,dday)

dtemp<-as.character(base$Q16_Birth_date_of_child)
dday<-as.numeric(substring(dtemp,1,2))
dmonth<-substring(dtemp,5,7)
dmonthNum<-0
dmonthNum[dmonth=="Jan"]<-1;dmonthNum[dmonth=="Feb"]<-2;dmonthNum[dmonth=="Mrz"]<-3;dmonthNum[dmonth=="Apr"]<-4;
dmonthNum[dmonth=="Mai"]<-5;dmonthNum[dmonth=="Jun"]<-6;dmonthNum[dmonth=="Jul"]<-7;dmonthNum[dmonth=="Aug"]<-8;
dmonthNum[dmonth=="Sep"]<-9;dmonthNum[dmonth=="Okt"]<-10;dmonthNum[dmonth=="Nov"]<-11;dmonthNum[dmonth=="Dez"]<-12;
dyear<-as.numeric(substring(dtemp,9,10))+2000
base$dateBirth<-ISOdate(dyear,dmonthNum,dday)
base$ageChildCalcDays<-base$dateInterview  - base$dateBirth
base$ageChildCalcMonths <- trunc(base$ageChildCalcDays/30.5)
base$ageChildCalcMonths <- trunc(base$ageChildCalcDays/30.5)


# make categories
base$agecat <- 0
base$agecat[base$ageChildCalcMonths>=6]<-6
base$agecat[base$ageChildCalcMonths>=12]<-12
base$agecat[base$ageChildCalcMonths>=18]<-18


# CALCULATE OUTCOME 'fully up to date'
base$calcUpToDate <- 1
# margin of 14 days allowed for timeliness of vaccine 
margin <- 14

# birth vaccinations
base$calcUpToDate[base$ageChildCalcDays>=margin & base$Q22_Has_child_been_given_BCG=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=margin & base$Q22_Has_child_been_given_BCG=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=margin & base$Q26_Has_child_received_OPV_0=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=margin & base$Q26_Has_child_received_OPV_0=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=margin & base$Q29_Has_child_received_HBV0=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=margin & base$Q29_Has_child_received_HBV0=="No"]<-0

# 6w vaccinations
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q32_Has_child_received_Penta_1=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q32_Has_child_received_Penta_1=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q32_Has_child_received_Penta_1==""]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q35_Has_child_received_OPV_1=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q35_Has_child_received_OPV_1=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q35_Has_child_received_OPV_1==""]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q50_Has_child_received_PCV_1=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q50_Has_child_received_PCV_1=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((6*7)+margin) & base$Q50_Has_child_received_PCV_1==""]<-0

# 10w vaccinations
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q38_Has_child_received_Penta_2=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q38_Has_child_received_Penta_2=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q38_Has_child_received_Penta_2==""]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q41_Has_child_received_OPV_2=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q41_Has_child_received_OPV_2=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q41_Has_child_received_OPV_2==""]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q53_Has_child_received_PCV_2=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q53_Has_child_received_PCV_2=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((10*7)+margin) & base$Q53_Has_child_received_PCV_2==""]<-0

# 14w vaccinations
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q44_Has_child_received_Penta_3=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q44_Has_child_received_Penta_3=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q44_Has_child_received_Penta_3==""]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q47_Has_child_received_OPV_3=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q47_Has_child_received_OPV_3=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q47_Has_child_received_OPV_3==""]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q56_Has_child_received_PCV_3=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q56_Has_child_received_PCV_3=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((14*7)+margin) & base$Q56_Has_child_received_PCV_3==""]<-0

# 9mo measles
base$calcUpToDate[base$ageChildCalcDays>=((39*7)+margin) & base$Q59_Has_child_recved_measlesVac=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((39*7)+margin) & base$Q59_Has_child_recved_measlesVac=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((39*7)+margin) & base$Q59_Has_child_recved_measlesVac==""]<-0

# 12mo yellow fever
base$calcUpToDate[base$ageChildCalcDays>=((52*7)+margin) & base$Q62_Has_child_recvd_yellowFev=="Don_t_know"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((52*7)+margin) & base$Q62_Has_child_recvd_yellowFev=="No"]<-0
base$calcUpToDate[base$ageChildCalcDays>=((52*7)+margin) & base$Q62_Has_child_recvd_yellowFev==""]<-0



# CALCULATE OUTCOME - Never had any vaccinations

 base$neverhadvacc <- 1
# birth vaccinations
base$neverhadvacc[base$Q22_Has_child_been_given_BCG=="Yes"]<-0
base$neverhadvacc[base$Q22_Has_child_been_given_BCG=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q26_Has_child_received_OPV_0=="Yes"]<-0
base$neverhadvacc[base$Q26_Has_child_received_OPV_0=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q29_Has_child_received_HBV0=="Yes"]<-0
base$neverhadvacc[base$Q29_Has_child_received_HBV0=="Yes_but_cant_remember_date"]<-0

# 6w vaccinations
base$neverhadvacc[base$Q32_Has_child_received_Penta_1=="Yes"]<-0
base$neverhadvacc[base$Q32_Has_child_received_Penta_1=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q35_Has_child_received_OPV_1=="Yes"]<-0
base$neverhadvacc[base$Q35_Has_child_received_OPV_1=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q50_Has_child_received_PCV_1=="Yes"]<-0
base$neverhadvacc[base$Q50_Has_child_received_PCV_1=="Yes_but_cant_remember_date"]<-0

# 10w vaccinations
base$neverhadvacc[base$Q38_Has_child_received_Penta_2=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q38_Has_child_received_Penta_2=="Yes"]<-0
base$neverhadvacc[base$Q41_Has_child_received_OPV_2=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q41_Has_child_received_OPV_2=="Yes"]<-0
base$neverhadvacc[base$Q53_Has_child_received_PCV_2=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q53_Has_child_received_PCV_2=="Yes"]<-0

# 14w vaccinations
base$neverhadvacc[base$Q44_Has_child_received_Penta_3=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q44_Has_child_received_Penta_3=="Yes"]<-0
base$neverhadvacc[base$Q47_Has_child_received_OPV_3=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q47_Has_child_received_OPV_3=="Yes"]<-0
base$neverhadvacc[base$Q56_Has_child_received_PCV_3=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q56_Has_child_received_PCV_3=="Yes"]<-0

# 9mo measles
base$neverhadvacc[base$Q59_Has_child_recved_measlesVac=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q59_Has_child_recved_measlesVac=="Yes"]<-0

# 12mo yellow fever
base$neverhadvacc[base$Q62_Has_child_recvd_yellowFev=="Yes_but_cant_remember_date"]<-0
base$neverhadvacc[base$Q62_Has_child_recvd_yellowFev=="Yes"]<-0


# immunization card seen
base$immCardSeen<-0
base$immCardSeen[as.character(base$Q21_Immunization_card)=="Seen"]<-1



# INDIVIDUAL VACCINATIONS

# function to turn string dates into R dates
dateFn<-function(charDate) {
   dtemp<-charDate
   dday<-as.numeric(substring(dtemp,1,2))
   dmonth<-substring(dtemp,5,7)
   dmonthNum<-0
   dmonthNum[dmonth=="Jan"]<-1; dmonthNum[dmonth=="Feb"]<-2;dmonthNum[dmonth=="Mrz"]<-3;dmonthNum[dmonth=="Apr"]<-4;
   dmonthNum[dmonth=="Mai"]<-5; dmonthNum[dmonth=="Jun"]<-6;dmonthNum[dmonth=="Jul"]<-7;dmonthNum[dmonth=="Aug"]<-8;
   dmonthNum[dmonth=="Sep"]<-9; dmonthNum[dmonth=="Okt"]<-10;dmonthNum[dmonth=="Nov"]<-11;dmonthNum[dmonth=="Dez"]<-12;
   dyear<-as.numeric(substring(dtemp,9,10))+2000
   dateOut<-ISOdate(dyear,dmonthNum,dday)
   return(dateOut)
}


# ages at time of vaccination are in weeks
base$dateBCG<-dateFn(as.character(base$Q23_date_BCG_was_given))
base$ageBCG<-(base$dateBCG-base$dateBirth)/((60*60*24)*7)


# PENTA 1
base$datePenta1<-dateFn(as.character(base$Q33_Date_of_Penta_1))
base$agePenta1<-(base$datePenta1-base$dateBirth)/7

base$penta1<-as.character(base$Q32_Has_child_received_Penta_1)
base$penta1[base$Q32_Has_child_received_Penta_1=="Yes_but_cant_remember_date"]<-"Yes"
base$penta1[base$Q32_Has_child_received_Penta_1=="Don_t_know"]<-NA
base$penta1[base$Q32_Has_child_received_Penta_1==""]<-NA


# PENTA 2
base$datePCV1<-dateFn(as.character(base$Q39_Date_of_Penta_2))
base$agePenta2<-(base$datePCV1-base$dateBirth)/7

base$penta2<-as.character(base$Q38_Has_child_received_Penta_2)
base$penta2[base$Q38_Has_child_received_Penta_2=="Yes_but_cant_remember_date"]<-"Yes"
base$penta2[base$Q38_Has_child_received_Penta_2=="Don_t_know"]<-NA
base$penta2[base$Q38_Has_child_received_Penta_2==""]<-NA

      
# Penta 3
base$datePenta3<-dateFn(as.character(base$Q45_Date_of_Penta_3))
base$agePenta3<-(base$datePenta3-base$dateBirth)/7

base$penta3<-as.character(base$Q44_Has_child_received_Penta_3)
base$penta3[base$Q44_Has_child_received_Penta_3=="Yes_but_cant_remember_date"]<-"Yes"
base$penta3[base$Q44_Has_child_received_Penta_3=="Don_t_know"]<-NA
base$penta3[base$Q44_Has_child_received_Penta_3==""]<-NA


# measles
base$ageChildCalcMonths9<-base$ageChildCalcMonths
base$ageChildCalcMonths9[base$ageChildCalcMonths9>=18]<-18
base$ageChildCalcMonths9[base$ageChildCalcMonths9>=9 & base$ageChildCalcMonths9<12]<-9
base$ageChildCalcMonths9[base$ageChildCalcMonths9>=12 & base$ageChildCalcMonths9<15]<-12
base$ageChildCalcMonths9[base$ageChildCalcMonths9>=15 & base$ageChildCalcMonths9<18]<-15

base$dateMeasles<-dateFn(as.character(base$Q60_Date_of_measles_vaccine))
base$ageMeasles<-(base$dateMeasles-base$dateBirth)/7

base$measles<-base$Q59_Has_child_recved_measlesVac
base$measles[base$Q59_Has_child_recved_measlesVac=="Yes_but_cant_remember_date"]<-"Yes"
base$measles[base$Q59_Has_child_recved_measlesVac=="Don_t_know"]<-NA
base$measles[base$Q59_Has_child_recved_measlesVac==""]<-NA


# number of penta vaccinations in children over 6 months (to assess drop-out)
base$numPenta<-0
for (i in 1:dim(base)[1]) {
  if (!is.na(base$penta1[i])) {
      if (base$penta1[i]=="Yes") base$numPenta[i]<-base$numPenta[i]+1
  }
  if (!is.na(base$penta2[i])) {
      if (base$penta2[i]=="Yes") base$numPenta[i]<-base$numPenta[i]+1
  }
  if (!is.na(base$penta3[i])) {
      if (base$penta3[i]=="Yes") base$numPenta[i]<-base$numPenta[i]+1
  }
}


# ever had measles
base$everHadMeasles<-0
for (i in 1:length(base$everHadMeasles)) {
   if (as.character(base$Q71has_child_ever_had_MEASLES[i]=="Yes")) {
       base$everHadMeasles[i]<-1   
   }
   if (as.character(base$Q71has_child_ever_had_MEASLES[i]=="")) {
       base$everHadMeasles[i]<-NA
   }
}


# ANC attendance
base$attendANC<-0
base$attendANC[as.character(base$Q73_mother_attend_ANC)=="Yes"]<-1
base$attendANC[as.character(base$Q73_mother_attend_ANC)=="Don_t_know"]<-NA


# tetanus vaccination of mother
base$tetanusVacc<-0
base$tetanusVacc[as.character(base$Q78DoseOf_TetanusToxoid_recvd)=="Two"]<-1
base$tetanusVacc[as.character(base$Q78DoseOf_TetanusToxoid_recvd)=="Three"]<-1
base$tetanusVacc[as.character(base$Q78DoseOf_TetanusToxoid_recvd)=="More_than_three"]<-1
base$tetanusVacc[as.character(base$Q78DoseOf_TetanusToxoid_recvd)==""]<-NA


# distance to HF
base$HFdistance<-as.character(base$Q12_How_close_is_the_HF_you_use)

# hard to reach
base$hardToReach<-0
base$hardToReach[as.character(base$Hard.to.reach)=="Yes"]<-1


# sex of child
base$femaleChild<-0
base$femaleChild[as.character(base$Q18_Sex_of_child)=="Female"]<-1
# no missing

# vaccination status
base$Vaccination.Status<-as.character(base$Vaccination.Status)


# clean variable 'where sought treatment'
# make category for 'not ill' in Q72


# where treatment sought for child's last ill health
# make a category for 'not ill' in base-term survey
base$whereSoughtTrt<-as.character(base$Q72Wher_you_soughtFor_treatmn)
base$specify_trt_place<-as.character(base$Q72_specify_other_treatment_place)

base$whereSoughtTrt[grep("Not ",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("not been",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("never been",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("not sick",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("never being",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("new",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("hasn't been",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("never needed",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("not exerienced",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("not fallen",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("not had illness",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("no problem",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Have not",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Healthy",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("not experienced",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("No sick",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("NOT ILL",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Never been",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Newborn",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("there ok",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("there OK",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Never sick",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Never been healed",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("New birth",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("Never fallen",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("no sick",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("healthy",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("never sick",base$specify_trt_place)] <-"not ill"
base$whereSoughtTrt[grep("no sick",base$specify_trt_place)] <-"not ill"

base$whereSoughtTrt[base$whereSoughtTrt=="not ill"]<-""

base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Always sick"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Church"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Don't go"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Don't know"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="I dnt know"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Know way"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Local herbs"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Locally prepared herbs"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="None"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Not treated"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Not specified"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Not taken anything"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Always sick"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Nowhere"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Resident health worker"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Took  native  treatment"]<-"OtherT"
base$whereSoughtTrt[substring(base$Q72_specify_other_treatment_place,1,7)=="Treated"]<-"OtherT"
base$whereSoughtTrt[base$Q72_specify_other_treatment_place=="Voming"]<-"OtherT"
base$whereSoughtTrt[base$whereSoughtTrt=="other_specify"]<-""


# age of carer
base$agecarercat<-13
base$agecarercat[base$Q6_Age_of_caregiver_years>=20]<-20
base$agecarercat[base$Q6_Age_of_caregiver_years>=30]<-30
base$agecarercat[base$Q6_Age_of_caregiver_years>=40]<-40
base$agecarercat[base$Q6_Age_of_caregiver_years>=50]<-50
base$agecarercat[base$Q6_Age_of_caregiver_years>=60]<-60
# - one value is 3519 - checked and is incorrect
base$agecarercat[base$Q6_Age_of_caregiver_years==3519]<-NA

# religion
base$Q10_Religious_Affiliation[base$Q10_Religious_Affiliation=="None"]<-"Other_Rel_Affiliation"
base$Q10_Religious_Affiliation[base$Q10_Religious_Affiliation=="Islam"]<-"Other_Rel_Affiliation"








