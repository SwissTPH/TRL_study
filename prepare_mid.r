

# prepare_mid.r
# prepares mid-survey data 



# CALCULATE AGE (ageChildCalcMonths, ageChildCalcDays) 
dtemp<-as.character(mid$SubmissionDate)
dday<-as.numeric(substring(dtemp,1,2))
dmonth<-as.numeric(substring(dtemp,4,5))
dyear<-as.numeric(substring(dtemp,7,10))
mid$dateInterview<-mdy.date(dmonth, dday, dyear)

dtemp<-as.character(mid$Q16_Birth_date_of_child)
dday<-as.numeric(substring(dtemp,1,2))
dmonth<-as.numeric(substring(dtemp,4,5))
dyear<-as.numeric(substring(dtemp,7,10))
mid$dateBirth<-mdy.date(dmonth, dday, dyear)

mid$ageChildCalcDays<-mid$dateInterview  - mid$dateBirth
mid$ageChildCalcMonths <- trunc(mid$ageChildCalcDays/30.5)
# make categories
mid$agecat <- 0
mid$agecat[mid$ageChildCalcMonths>=6]<-6
mid$agecat[mid$ageChildCalcMonths>=12]<-12
mid$agecat[mid$ageChildCalcMonths>=18]<-18

# CALCULATE UP TO DATE for age
# at birth - OPV0, HEP0, BCG
# at 6w - Penta1, OPV1, PCV1
# at 10w - Penta2, OPV2, PCV2
# at 14w - Penta3, OPV3, PCV3
# at 39w - measles
# at 52w - yellow fever

mid$calcUpToDate <- 1
# allow margin of 14 days for timeliness
margin <- 14

# birth vaccinations
mid$calcUpToDate[mid$ageChildCalcDays>=margin & mid$Q22_Has_child_been_given_BCG=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=margin & mid$Q22_Has_child_been_given_BCG=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=margin & mid$Q26_Has_child_received_OPV_0=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=margin & mid$Q26_Has_child_received_OPV_0=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=margin & mid$Q29_Has_child_received_HBV0=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=margin & mid$Q29_Has_child_received_HBV0=="No"]<-0

# 6w vaccinations
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q32_Has_child_received_Penta_1=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q32_Has_child_received_Penta_1=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q32_Has_child_received_Penta_1==""]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q35_Has_child_received_OPV_1=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q35_Has_child_received_OPV_1=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q35_Has_child_received_OPV_1==""]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q50_Has_child_received_PCV_1=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q50_Has_child_received_PCV_1=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((6*7)+margin) & mid$Q50_Has_child_received_PCV_1==""]<-0

# 10w vaccinations
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q38_Has_child_received_Penta_2=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q38_Has_child_received_Penta_2=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q38_Has_child_received_Penta_2==""]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q41_Has_child_received_OPV_2=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q41_Has_child_received_OPV_2=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q41_Has_child_received_OPV_2==""]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q53_Has_child_received_PCV_2=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q53_Has_child_received_PCV_2=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((10*7)+margin) & mid$Q53_Has_child_received_PCV_2==""]<-0

# 14w vaccinations
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q44_Has_child_received_Penta_3=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q44_Has_child_received_Penta_3=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q44_Has_child_received_Penta_3==""]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q47_Has_child_received_OPV_3=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q47_Has_child_received_OPV_3=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q47_Has_child_received_OPV_3==""]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q56_Has_child_received_PCV_3=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q56_Has_child_received_PCV_3=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((14*7)+margin) & mid$Q56_Has_child_received_PCV_3==""]<-0

# 9mo measles
mid$calcUpToDate[mid$ageChildCalcDays>=((39*7)+margin) & mid$Q59_Has_child_recved_measlesVac=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((39*7)+margin) & mid$Q59_Has_child_recved_measlesVac=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((39*7)+margin) & mid$Q59_Has_child_recved_measlesVac==""]<-0

# 12mo yellow fever
mid$calcUpToDate[mid$ageChildCalcDays>=((52*7)+margin) & mid$Q62_Has_child_recvd_yellowFev=="Don_t_know"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((52*7)+margin) & mid$Q62_Has_child_recvd_yellowFev=="No"]<-0
mid$calcUpToDate[mid$ageChildCalcDays>=((52*7)+margin) & mid$Q62_Has_child_recvd_yellowFev==""]<-0



# CALCULATE OUTCOME - never had any vaccinations

mid$neverhadvacc <- 1
# birth vaccinations
mid$neverhadvacc[mid$Q22_Has_child_been_given_BCG=="Yes"]<-0
mid$neverhadvacc[mid$Q22_Has_child_been_given_BCG=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q26_Has_child_received_OPV_0=="Yes"]<-0
mid$neverhadvacc[mid$Q26_Has_child_received_OPV_0=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q29_Has_child_received_HBV0=="Yes"]<-0
mid$neverhadvacc[mid$Q29_Has_child_received_HBV0=="Yes_but_cant_remember_date"]<-0

# 6w vaccinations
mid$neverhadvacc[mid$Q32_Has_child_received_Penta_1=="Yes"]<-0
mid$neverhadvacc[mid$Q32_Has_child_received_Penta_1=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q35_Has_child_received_OPV_1=="Yes"]<-0
mid$neverhadvacc[mid$Q35_Has_child_received_OPV_1=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q50_Has_child_received_PCV_1=="Yes"]<-0
mid$neverhadvacc[mid$Q50_Has_child_received_PCV_1=="Yes_but_cant_remember_date"]<-0

# 10w vaccinations
mid$neverhadvacc[mid$Q38_Has_child_received_Penta_2=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q38_Has_child_received_Penta_2=="Yes"]<-0
mid$neverhadvacc[mid$Q41_Has_child_received_OPV_2=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q41_Has_child_received_OPV_2=="Yes"]<-0
mid$neverhadvacc[mid$Q53_Has_child_received_PCV_2=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q53_Has_child_received_PCV_2=="Yes"]<-0

# 14w vaccinations
mid$neverhadvacc[mid$Q44_Has_child_received_Penta_3=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q44_Has_child_received_Penta_3=="Yes"]<-0
mid$neverhadvacc[mid$Q47_Has_child_received_OPV_3=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q47_Has_child_received_OPV_3=="Yes"]<-0
mid$neverhadvacc[mid$Q56_Has_child_received_PCV_3=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q56_Has_child_received_PCV_3=="Yes"]<-0

# 9mo measles
mid$neverhadvacc[mid$Q59_Has_child_recved_measlesVac=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q59_Has_child_recved_measlesVac=="Yes"]<-0

# 12mo yellow fever
mid$neverhadvacc[mid$Q62_Has_child_recvd_yellowFev=="Yes_but_cant_remember_date"]<-0
mid$neverhadvacc[mid$Q62_Has_child_recvd_yellowFev=="Yes"]<-0


# immunization card seen
mid$immCardSeen<-0
mid$immCardSeen[as.character(mid$Q21_Immunization_card)=="Seen"]<-1


# individual vaccinations

mid$ageChildCalcMonths6<-mid$ageChildCalcMonths
mid$ageChildCalcMonths6[mid$ageChildCalcMonths6>8]<-8

mid$bcg6<-mid$Q22_Has_child_been_given_BCG
mid$bcg6[mid$Q22_Has_child_been_given_BCG=="Yes_but_cant_remember_date"]<-"Yes"
mid$bcg6[mid$Q22_Has_child_been_given_BCG=="Don_t_know"]<-NA


# function for getting dates from a character date (charDate)

dateFn<-function(charDate) {
   dtemp<-charDate
   dday<-as.numeric(substring(dtemp,1,2))
   dmonth<-as.numeric(substring(dtemp,4,5))
   #dmonthNum<-0
   #dmonthNum[dmonth=="Jan"]<-1; dmonthNum[dmonth=="Feb"]<-2;dmonthNum[dmonth=="Mrz"]<-3;dmonthNum[dmonth=="Apr"]<-4;
   #dmonthNum[dmonth=="Mai"]<-5; dmonthNum[dmonth=="Jun"]<-6;dmonthNum[dmonth=="Jul"]<-7;dmonthNum[dmonth=="Aug"]<-8;
   #dmonthNum[dmonth=="Sep"]<-9; dmonthNum[dmonth=="Okt"]<-10;dmonthNum[dmonth=="Nov"]<-11;dmonthNum[dmonth=="Dez"]<-12;
   dyear<-as.numeric(substring(dtemp,7,10))
   dateOut<-mdy.date(dmonth,dday,dyear)
   return(dateOut)
}



mid$Q23_date_BCG_was_given<-as.character(mid$Q23_date_BCG_was_given) 
mid$Q23_date_BCG_was_given[mid$Q23_date_BCG_was_given=="Wrong entry"]<-NA
mid$Q23_date_BCG_was_given[mid$Q23_date_BCG_was_given=="Yes_but_cant_remember_date"]<-NA

mid$dateBCG<-NA
mid$dateBCG<-dateFn(as.character(mid$Q23_date_BCG_was_given))

mid$ageBCG<-(mid$dateBCG-mid$dateBirth)/7
# in weeks



# PENTA 1
mid$Q33_Date_of_Penta_1[mid$Q33_Date_of_Penta_1=="Wrong entry"]<-NA
mid$Q33_Date_of_Penta_1[mid$Q33_Date_of_Penta_1=="Wrong entry "]<-NA
mid$datePenta1<-dateFn(as.character(mid$Q33_Date_of_Penta_1))
mid$agePenta1<-(mid$datePenta1-mid$dateBirth)/7

mid$penta1<-as.character(mid$Q32_Has_child_received_Penta_1)
mid$penta1[mid$Q32_Has_child_received_Penta_1=="Yes_but_cant_remember_date"]<-"Yes"
mid$penta1[mid$Q32_Has_child_received_Penta_1=="Don_t_know"]<-NA
mid$penta1[mid$Q32_Has_child_received_Penta_1==""]<-NA


# PENTA 2
mid$Q39_Date_of_Penta_2[mid$Q39_Date_of_Penta_2=="Wrong entry"]<-NA
mid$datePenta2<-dateFn(as.character(mid$Q39_Date_of_Penta_2))
mid$agePenta2<-(mid$datePenta2-mid$dateBirth)/7

mid$penta2<-as.character(mid$Q38_Has_child_received_Penta_2)
mid$penta2[mid$Q38_Has_child_received_Penta_2=="Yes_but_cant_remember_date"]<-"Yes"
mid$penta2[mid$Q38_Has_child_received_Penta_2=="Don_t_know"]<-NA
mid$penta2[mid$Q38_Has_child_received_Penta_2==""]<-NA


      
# Penta 3
mid$Q45_Date_of_Penta_3[as.character(mid$Q45_Date_of_Penta_3)=="Wrong entry"]<-NA
mid$Q45_Date_of_Penta_3[as.character(mid$Q45_Date_of_Penta_3)=="Wrong entry "]<-NA
mid$datePenta3<-dateFn(as.character(mid$Q45_Date_of_Penta_3))
mid$agePenta3<-(mid$datePenta3-mid$dateBirth)/7

mid$penta3<-as.character(mid$Q44_Has_child_received_Penta_3)
mid$penta3[mid$Q44_Has_child_received_Penta_3=="Yes_but_cant_remember_date"]<-"Yes"
mid$penta3[mid$Q44_Has_child_received_Penta_3=="Don_t_know"]<-NA
mid$penta3[mid$Q44_Has_child_received_Penta_3==""]<-NA



# measles
mid$ageChildCalcMonths9<-mid$ageChildCalcMonths
mid$ageChildCalcMonths9[mid$ageChildCalcMonths9>=18]<-18
mid$ageChildCalcMonths9[mid$ageChildCalcMonths9>=9 & mid$ageChildCalcMonths9<12]<-9
mid$ageChildCalcMonths9[mid$ageChildCalcMonths9>=12 & mid$ageChildCalcMonths9<15]<-12
mid$ageChildCalcMonths9[mid$ageChildCalcMonths9>=15 & mid$ageChildCalcMonths9<18]<-15

mid$Q60_Date_of_measles_vaccine[mid$Q60_Date_of_measles_vaccine=="Wrong entry"]<-NA
mid$dateMeasles<-dateFn(as.character(mid$Q60_Date_of_measles_vaccine))

mid$ageMeasles<-(mid$dateMeasles-mid$dateBirth)/7

mid$measles<-mid$Q59_Has_child_recved_measlesVac
mid$measles[mid$Q59_Has_child_recved_measlesVac=="Yes_but_cant_remember_date"]<-"Yes"
mid$measles[mid$Q59_Has_child_recved_measlesVac=="Don_t_know"]<-NA
mid$measles[mid$Q59_Has_child_recved_measlesVac==""]<-NA


# number of penta vaccinations in children over 6 months (to assess drop-out)
mid$numPenta<-0
for (i in 1:dim(mid)[1]) {
  if (!is.na(mid$penta1[i])) {
      if (mid$penta1[i]=="Yes") mid$numPenta[i]<-mid$numPenta[i]+1
  }
  if (!is.na(mid$penta2[i])) {
      if (mid$penta2[i]=="Yes") mid$numPenta[i]<-mid$numPenta[i]+1
  }
  if (!is.na(mid$penta3[i])) {
      if (mid$penta3[i]=="Yes") mid$numPenta[i]<-mid$numPenta[i]+1
  }
}


# ever had measles
mid$everHadMeasles<-0
for (i in 1:length(mid$everHadMeasles)) {
   if (as.character(mid$Q70has_child_ever_had_MEASLES[i]=="Yes")) {
       mid$everHadMeasles[i]<-1   
   }
   if (as.character(mid$Q70has_child_ever_had_MEASLES[i]=="")) {
       mid$everHadMeasles[i]<-NA
   }
}


# ANC attendance
mid$attendANC<-0
mid$attendANC[as.character(mid$Q72_mother_attend_ANC)=="Yes"]<-1
mid$attendANC[as.character(mid$Q72_mother_attend_ANC)=="Don_t_know"]<-NA


# tetanus vaccination of mother
mid$tetanusVacc<-0
mid$tetanusVacc[as.character(mid$Q77DoseOf_TetanusToxoid_recvd)=="Two"]<-1
mid$tetanusVacc[as.character(mid$Q77DoseOf_TetanusToxoid_recvd)=="Three"]<-1
mid$tetanusVacc[as.character(mid$Q77DoseOf_TetanusToxoid_recvd)=="More_than_three"]<-1
mid$tetanusVacc[as.character(mid$Q77DoseOf_TetanusToxoid_recvd)==""]<-NA

# distance to HF
mid$HFdistance<-as.character(mid$Q12_How_close_is_the_HF_you_use)

# hard to reach
mid$hardToReach<-0
mid$hardToReach[as.character(mid$access_state)=="hard_to_reach"]<-1

# sex of child 
mid$femaleChild<-0
mid$femaleChild[as.character(mid$Q18_Sex_of_child)=="Female"]<-1
# no missing



# where treatment sought for child's last ill health
# make a category for 'not ill' in mid-term survey
mid$whereSoughtTrt<-as.character(mid$Q71Wher_you_soughtFor_treatmn)
mid$specify_trt_place<-as.character(mid$Q71_specify_other_treatment_place)

mid$whereSoughtTrt[grep("Not ",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("not been",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("never been",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("not sick",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("never being",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("new",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("hasn't been",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("never needed",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("not exerienced",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("not fallen",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("not had illness",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("no problem",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Have not",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Healthy",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("not experienced",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("No sick",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("NOT ILL",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Never been",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Newborn",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("there ok",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("there OK",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Never sick",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Never been healed",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("New birth",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("Never fallen",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("no sick",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("healthy",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("never sick",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[grep("no sick",mid$specify_trt_place)] <-"not ill"
mid$whereSoughtTrt[mid$whereSoughtTrt=="not ill"]<-""

mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Always sick"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Church"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Don't go"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Don't know"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="I dnt know"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Know way"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Local herbs"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Locally prepared herbs"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="None"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Not treated"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Not specified"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Not taken anything"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Always sick"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Nowhere"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Resident health worker"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Took  native  treatment"]<-"OtherT"
mid$whereSoughtTrt[substring(mid$Q71_specify_other_treatment_place,1,7)=="Treated"]<-"OtherT"
mid$whereSoughtTrt[mid$Q71_specify_other_treatment_place=="Voming"]<-"OtherT"

mid$whereSoughtTrt[mid$whereSoughtTrt=="other_specify"]<-"OtherT"

# age of carer
mid$agecarercat<-13
mid$agecarercat[mid$Q6_Age_of_caregiver_years>=20]<-20
mid$agecarercat[mid$Q6_Age_of_caregiver_years>=30]<-30
mid$agecarercat[mid$Q6_Age_of_caregiver_years>=40]<-40
mid$agecarercat[mid$Q6_Age_of_caregiver_years>=50]<-50
mid$agecarercat[mid$Q6_Age_of_caregiver_years>=60]<-60
# one value is 343y - set to missing
mid$agecarercat[mid$Q6_Age_of_caregiver_years==343]<-NA
mid$Q6_Age_of_caregiver_years[mid$Q6_Age_of_caregiver_years==343]<-NA

# level of education of carer
mid$educCat<-as.character(mid$Q7_level_of_educ_of_caregiver)       
mid$educCat[mid$educCat=="Some_Primary"]<-"Primary"
mid$educCat[mid$educCat=="Completed_Primary"]<-"Primary"
mid$educCat[mid$educCat=="Some_Secondary"]<-"Secondary"
mid$educCat[mid$educCat=="Completed_Secondary"]<-"Secondary"
mid$educCat[mid$educCat=="Some_Tertiary"]<-"Tertiary"
mid$educCat[mid$educCat=="Completed_Tertiary"]<-"Tertiary"

# religion
mid$Q10_Religious_Affiliation<-as.character(mid$Q10_Religious_Affiliation)
mid$Q10_Religious_Affiliation[mid$Q10_Religious_Affiliation=="Islam"]<-"Islam/Other/None"
mid$Q10_Religious_Affiliation[mid$Q10_Religious_Affiliation=="Other_Rel_Affiliation"]<-"Islam/Other/None"
mid$Q10_Religious_Affiliation[mid$Q10_Religious_Affiliation=="None"]<-"Islam/Other/None"







