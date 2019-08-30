

# prepare_final.r
# prepares final survey data 



# calculate age

dtemp<-as.character(final$SubmissionDate)
dtemp[as.character(final$SubmissionDate)=="Endline"]<-"19. Jan 19"
# take middle of the 10 days of survey if date not specified
dday<-as.numeric(substring(dtemp,1,2))
dmonth<-substring(dtemp,4,6)
dmonth[dmonth=="Jan"]<-1
dmonth<-as.numeric(dmonth)
dyear<-as.numeric(substring(dtemp,7,11))
final$dateInterview<-mdy.date(dmonth, dday, dyear)

dtemp<-as.character(final$Q16_Birth_date_of_child)
dtemp[dtemp=="Agam"]<-NA
dday<-as.numeric(substring(dtemp,1,2))
dmonth<-as.numeric(substring(dtemp,4,5))
dyear<-as.numeric(substring(dtemp,7,10))
final$dateBirth<-mdy.date(dmonth, dday, dyear)

final$ageChildCalcDays<-final$dateInterview  - final$dateBirth
final$ageChildCalcMonths <- trunc(final$ageChildCalcDays/30.5)

# make categories
final$agecat <- 0
final$agecat[final$ageChildCalcMonths>=6]<-6
final$agecat[final$ageChildCalcMonths>=12]<-12
final$agecat[final$ageChildCalcMonths>=18]<-18

# CALCULATE VARIABLE UP_TO_DATE by age
# at birth - OPV0, HEP0, BCG
# at 6w - Penta1, OPV1, PCV1
# at 10w - Penta2, OPV2, PCV2
# at 14w - Penta3, OPV3, PCV3
# at 39w - measles
# at 52w - yellow fever

final$calcUpToDate <- 1
# margin of 14 days
margin <- 14

# birth vaccinations
final$calcUpToDate[final$ageChildCalcDays>=margin & final$Q22_Has_child_been_given_BCG=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=margin & final$Q22_Has_child_been_given_BCG=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=margin & final$Q26_Has_child_received_OPV_0=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=margin & final$Q26_Has_child_received_OPV_0=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=margin & final$Q29_Has_child_received_HBV0=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=margin & final$Q29_Has_child_received_HBV0=="No"]<-0

# 6w vaccinations
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q32_Has_child_received_Penta_1=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q32_Has_child_received_Penta_1=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q32_Has_child_received_Penta_1==""]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q35_Has_child_received_OPV_1=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q35_Has_child_received_OPV_1=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q35_Has_child_received_OPV_1==""]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q50_Has_child_received_PCV_1=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q50_Has_child_received_PCV_1=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((6*7)+margin) & final$Q50_Has_child_received_PCV_1==""]<-0

# 10w vaccinations
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q38_Has_child_received_Penta_2=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q38_Has_child_received_Penta_2=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q38_Has_child_received_Penta_2==""]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q41_Has_child_received_OPV_2=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q41_Has_child_received_OPV_2=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q41_Has_child_received_OPV_2==""]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q53_Has_child_received_PCV_2=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q53_Has_child_received_PCV_2=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((10*7)+margin) & final$Q53_Has_child_received_PCV_2==""]<-0

# 14w vaccinations
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q44_Has_child_received_Penta_3=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q44_Has_child_received_Penta_3=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q44_Has_child_received_Penta_3==""]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q47_Has_child_received_OPV_3=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q47_Has_child_received_OPV_3=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q47_Has_child_received_OPV_3==""]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q56_Has_child_received_PCV_3=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q56_Has_child_received_PCV_3=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((14*7)+margin) & final$Q56_Has_child_received_PCV_3==""]<-0


# 9mo measles
final$calcUpToDate[final$ageChildCalcDays>=((39*7)+margin) & final$Q59_Has_child_recved_measlesVac=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((39*7)+margin) & final$Q59_Has_child_recved_measlesVac=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((39*7)+margin) & final$Q59_Has_child_recved_measlesVac==""]<-0

# 12mo yellow fever
final$calcUpToDate[final$ageChildCalcDays>=((52*7)+margin) & final$Q62_Has_child_recvd_yellowFev=="Don_t_know"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((52*7)+margin) & final$Q62_Has_child_recvd_yellowFev=="No"]<-0
final$calcUpToDate[final$ageChildCalcDays>=((52*7)+margin) & final$Q62_Has_child_recvd_yellowFev==""]<-0



# Never had any vaccinations

final$neverhadvacc <- 1

# birth vaccinations
final$neverhadvacc[final$Q22_Has_child_been_given_BCG=="Yes"]<-0
final$neverhadvacc[final$Q22_Has_child_been_given_BCG=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q26_Has_child_received_OPV_0=="Yes"]<-0
final$neverhadvacc[final$Q26_Has_child_received_OPV_0=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q29_Has_child_received_HBV0=="Yes"]<-0
final$neverhadvacc[final$Q29_Has_child_received_HBV0=="Yes_but_cant_remember_date"]<-0


# 6w vaccinations
final$neverhadvacc[final$Q32_Has_child_received_Penta_1=="Yes"]<-0
final$neverhadvacc[final$Q32_Has_child_received_Penta_1=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q35_Has_child_received_OPV_1=="Yes"]<-0
final$neverhadvacc[final$Q35_Has_child_received_OPV_1=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q50_Has_child_received_PCV_1=="Yes"]<-0
final$neverhadvacc[final$Q50_Has_child_received_PCV_1=="Yes_but_cant_remember_date"]<-0

# 10w vaccinations
final$neverhadvacc[final$Q38_Has_child_received_Penta_2=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q38_Has_child_received_Penta_2=="Yes"]<-0
final$neverhadvacc[final$Q41_Has_child_received_OPV_2=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q41_Has_child_received_OPV_2=="Yes"]<-0
final$neverhadvacc[final$Q53_Has_child_received_PCV_2=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q53_Has_child_received_PCV_2=="Yes"]<-0

# 14w vaccinations
final$neverhadvacc[final$Q44_Has_child_received_Penta_3=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q44_Has_child_received_Penta_3=="Yes"]<-0
final$neverhadvacc[final$Q47_Has_child_received_OPV_3=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q47_Has_child_received_OPV_3=="Yes"]<-0
final$neverhadvacc[final$Q56_Has_child_received_PCV_3=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q56_Has_child_received_PCV_3=="Yes"]<-0

# 9mo measles
final$neverhadvacc[final$Q59_Has_child_recved_measlesVac=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q59_Has_child_recved_measlesVac=="Yes"]<-0

# 12mo yellow fever
final$neverhadvacc[final$Q62_Has_child_recvd_yellowFev=="Yes_but_cant_remember_date"]<-0
final$neverhadvacc[final$Q62_Has_child_recvd_yellowFev=="Yes"]<-0


# Immunization card seen
final$immCardSeen<-0
final$immCardSeen[as.character(final$Q21.Immunization.card)=="Seen"]<-1



# individual vaccinations

final$ageChildCalcMonths6<-final$ageChildCalcMonths
final$ageChildCalcMonths6[final$ageChildCalcMonths6>8]<-8
final$bcg6<-final$Q22_Has_child_been_given_BCG
final$bcg6[final$Q22_Has_child_been_given_BCG=="Yes_but_cant_remember_date"]<-"Yes"
final$bcg6[final$Q22_Has_child_been_given_BCG=="Don_t_know"]<-NA

# function for getting dates from a character date (charDate)
dateFn2<-function(charDate) {
   dtemp<-charDate
   dday<-as.numeric(substring(dtemp,1,2))
   dmonth<-as.numeric(substring(dtemp,4,5))
   dyear<-as.numeric(substring(dtemp,7,10))
   dateOut<-mdy.date(dmonth,dday,dyear)
   return(dateOut)
}


final$Q23_date_BCG_was_given<-as.character(final$Q23_date_BCG_was_given)
final$Q23_date_BCG_was_given[final$Q23_date_BCG_was_given=="Yes"]<-NA
final$dateBCG<-dateFn2(final$Q23_date_BCG_was_given)
final$ageBCG<-(final$dateBCG-final$dateBirth)/7

# PENTA 1
final$Q33_Date_of_Penta_1<-as.character(final$Q33_Date_of_Penta_1)
final$Q33_Date_of_Penta_1[final$Q33_Date_of_Penta_1=="Yes"]<-NA
final$datePenta1<-dateFn2(as.character(final$Q33_Date_of_Penta_1))
final$agePenta1<-(final$datePenta1-final$dateBirth)/7

final$penta1<-as.character(final$Q32_Has_child_received_Penta_1)
final$penta1[final$Q32_Has_child_received_Penta_1=="Yes_but_cant_remember_date"]<-"Yes"
final$penta1[final$Q32_Has_child_received_Penta_1=="Don_t_know"]<-NA
final$penta1[final$Q32_Has_child_received_Penta_1==""]<-NA


# PENTA 2
final$Q39_Date_of_Penta_2[final$Q39_Date_of_Penta_2=="Yes"]<-NA
final$datePenta2<-dateFn2(as.character(final$Q39_Date_of_Penta_2))
final$agePenta2<-(final$datePenta2-final$dateBirth)/7

final$penta2<-as.character(final$Q38_Has_child_received_Penta_2)
final$penta2[final$Q38_Has_child_received_Penta_2=="Yes_but_cant_remember_date"]<-"Yes"
final$penta2[final$Q38_Has_child_received_Penta_2=="Don_t_know"]<-NA
final$penta2[final$Q38_Has_child_received_Penta_2==""]<-NA


# Penta 3
final$Q45_Date_of_Penta_3[final$Q45_Date_of_Penta_3=="Yes"]<-NA
final$datePenta3<-dateFn2(as.character(final$Q45_Date_of_Penta_3))
final$agePenta3<-(final$datePenta3-final$dateBirth)/7

final$penta3<-as.character(final$Q44_Has_child_received_Penta_3)
final$penta3[final$Q44_Has_child_received_Penta_3=="Yes_but_cant_remember_date"]<-"Yes"
final$penta3[final$Q44_Has_child_received_Penta_3=="Don_t_know"]<-NA
final$penta3[final$Q44_Has_child_received_Penta_3==""]<-NA

# measles
final$ageChildCalcMonths9<-final$ageChildCalcMonths
final$ageChildCalcMonths9[final$ageChildCalcMonths9>=18]<-18
final$ageChildCalcMonths9[final$ageChildCalcMonths9>=9 & final$ageChildCalcMonths9<12]<-9
final$ageChildCalcMonths9[final$ageChildCalcMonths9>=12 & final$ageChildCalcMonths9<15]<-12
final$ageChildCalcMonths9[final$ageChildCalcMonths9>=15 & final$ageChildCalcMonths9<18]<-15

final$dateMeasles<-dateFn2(as.character(final$Q60_Date_of_measles_vaccine))
final$ageMeaslesvac<-(final$dateMeasles-final$dateBirth)/7

final$measles<-as.character(final$Q59_Has_child_recved_measlesVac)
final$measles[final$Q59_Has_child_recved_measlesVac=="Yes_but_cant_remember_date"]<-"Yes"
final$measles[final$Q59_Has_child_recved_measlesVac=="Don_t_know"]<-NA
final$measles[final$Q59_Has_child_recved_measlesVac==""]<-NA


# number of penta vaccinations in children over 6 months (to assess drop-out)

final$numPenta<-0
for (i in 1:dim(final)[1]) {
  if (!is.na(final$penta1[i])) {
      if (final$penta1[i]=="Yes") final$numPenta[i]<-final$numPenta[i]+1
  }
  if (!is.na(final$penta2[i])) {
      if (final$penta2[i]=="Yes") final$numPenta[i]<-final$numPenta[i]+1
  }
  if (!is.na(final$penta3[i])) {
      if (final$penta3[i]=="Yes") final$numPenta[i]<-final$numPenta[i]+1
  }
}

# final$numPenta[final$ageChildCalcMonths6<6]<-NA
 final$numPenta[final$ageChildCalcDays<(7*16)]<-NA


# ever had measles
final$everHadMeasles<-0
for (i in 1:length(final$everHadMeasles)) {
   if (as.character(final$Q70has_child_ever_had_MEASLES[i]=="Yes")) {
       final$everHadMeasles[i]<-1   
   }
   if (as.character(final$Q70has_child_ever_had_MEASLES[i]=="")) {
       final$everHadMeasles[i]<-NA
   }
}


# ANC attendance
final$attendANC<-0
final$attendANC[as.character(final$Q72_mother_attend_ANC)=="Yes"]<-1
final$attendANC[as.character(final$Q72_mother_attend_ANC)=="Don_t_know"]<-NA


# tetanus vacc of mother
final$tetanusVacc<-0
final$tetanusVacc[as.character(final$Q77DoseOf_TetanusToxoid_recvd)=="Two"]<-1
final$tetanusVacc[as.character(final$Q77DoseOf_TetanusToxoid_recvd)=="Three"]<-1
final$tetanusVacc[as.character(final$Q77DoseOf_TetanusToxoid_recvd)=="More_than_three"]<-1
final$tetanusVacc[as.character(final$Q77DoseOf_TetanusToxoid_recvd)==""]<-NA


# distance to HF
final$HFdistance<-as.character(final$Q12_How_close_is_the_HF_you_use)

# hard to reach
final$hardToReach<-0
final$hardToReach[as.character(final$access_state)=="hard_to_reach"]<-1

# sex of child
final$femaleChild<-0
final$femaleChild[as.character(final$Q18_Sex_of_child)=="Female"]<-1
# no missing



final$Vac.Status<-as.character(final$IMMUNIZATION.STATUS)




# where treatment sought for child's last ill health 
# make a category for 'not ill' in final-term survey
final$whereSoughtTrt<-as.character(final$Q71Wher_you_soughtFor_treatmn)
final$specify_trt_place<-as.character(final$Q71_specify_other_treatment_place)

final$whereSoughtTrt[grep("Not ",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not been",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("never been",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("never being",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("new",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("hasn't been",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("never needed",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not exerienced",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not fallen",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not had illness",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("no problem",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Have not",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Healthy",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not experienced",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("No sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("NOT ILL",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Never been",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Newborn",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("there ok",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("there OK",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Never sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Never been healed",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("New birth",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Never fallen",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("no sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("healthy",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("never sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("no sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not seek",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not being sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Did not treat",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("has been in good health",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not  sick",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("Never",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("hasn't shown any signs of illness",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("No noticed sickness",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("The cough just started",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("mild",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("not ill",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("N/A",final$specify_trt_place)] <-"not ill"
final$whereSoughtTrt[grep("No illness",final$specify_trt_place)] <-"not ill"

final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Always sick"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Church"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Don't go"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Don't know"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="I dnt know"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Know way"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Local herbs"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Locally prepared herbs"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="None"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Not treated"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Not specified"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Not taken anything"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Always sick"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Nowhere"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Resident health worker"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Took  native  treatment"]<-"OtherT"
final$whereSoughtTrt[substring(final$Q71_specify_other_treatment_place,1,7)=="Treated"]<-"OtherT"
final$whereSoughtTrt[final$Q71_specify_other_treatment_place=="Voming"]<-"OtherT"
final$whereSoughtTrt[final$whereSoughtTrt=="not ill"]<-""



# age of carer
final$agecarercat<-13
final$agecarercat[final$Q6_Age_of_caregiver_in_years>=20]<-20
final$agecarercat[final$Q6_Age_of_caregiver_in_years>=30]<-30
final$agecarercat[final$Q6_Age_of_caregiver_in_years>=40]<-40
final$agecarercat[final$Q6_Age_of_caregiver_in_years>=50]<-50
final$agecarercat[final$Q6_Age_of_caregiver_in_years>=60]<-60

# level of education
final$educCat<-as.character(final$Q7_level_of_educ_of_caregiver)       
final$educCat[final$educCat=="Some_Primary"]<-"Primary"
final$educCat[final$educCat=="Completed_Primary"]<-"Primary"
final$educCat[final$educCat=="Some_Secondary"]<-"Secondary"
final$educCat[final$educCat=="Completed_Secondary"]<-"Secondary"
final$educCat[final$educCat=="Some_Tertiary"]<-"Tertiary"
final$educCat[final$educCat=="Completed_Tertiary"]<-"Tertiary"

# religion
final$Q10_Religious_Affiliation[final$Q10_Religious_Affiliation=="None"]<-"Other_Rel_Affiliation"
final$Q10_Religious_Affiliation[final$Q10_Religious_Affiliation=="Islam"]<-"Other_Rel_Affiliation"








