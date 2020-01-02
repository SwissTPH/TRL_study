

# table 13.r


# -- TETANUS VACCINATION OF MOTHER ---- 

# baseline
table(base$Q78DoseOf_TetanusToxoid_recvd, base$researchArm, exclude=NULL)
prop.table(table(base$Q78DoseOf_TetanusToxoid_recvd, base$researchArm), margin=2)

# mid
table(mid$Q77DoseOf_TetanusToxoid_recvd, mid$researchArm, exclude=NULL)
prop.table(table(mid$Q77DoseOf_TetanusToxoid_recvd, mid$researchArm, exclude=NULL), margin=2)

# final
table(final$Q77DoseOf_TetanusToxoid_recvd, final$researchArm, exclude=NULL)
prop.table(table(final$Q77DoseOf_TetanusToxoid_recvd, final$researchArm, exclude=NULL), margin=2)

# test
m1 <- glmer(tetanusVacc ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)



# -- ATTENDANCE AT ANC BY MOTHER ----

# baseline
table(base$Q73_mother_attend_ANC, base$researchArm, exclude=NULL)
prop.table(table(base$Q73_mother_attend_ANC, base$researchArm), margin=2)

# mid
table(mid$Q72_mother_attend_ANC, mid$researchArm, exclude=NULL)
prop.table(table(mid$Q72_mother_attend_ANC, mid$researchArm, exclude=NULL), margin=2)

# final
table(final$Q72_mother_attend_ANC, final$researchArm, exclude=NULL)
prop.table(table(final$Q72_mother_attend_ANC, final$researchArm, exclude=NULL), margin=2)

# test
m1 <- glmer(attendANC ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)



# -- HAS CHILD EVER HAD MEASLES? ----


# ever had measles
# children all ages (not just >9mo)
table(allsmall$everHadMeasles, allsmall$intervSurvey,exclude=NULL)
prop.table(table(allsmall$everHadMeasles, allsmall$intervSurvey),margin=2)

m1 <- glmer(everHadMeasles ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)



# -- Table 14 ------

# ICC values taken from random effects models

# up-to-date vaccination
m1 <- glmer(calcUpToDate ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.390, ward 0.212, village 0.062
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.39+0.212+0.062+residE
# LGA 
0.390/totalVar
# ward
0.212/totalVar
# village
0.062/totalVar

# at least one vaccine
m1 <- glmer(calcAtLeastPar ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 1.838, ward 0.115, village 0.310
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-1.838+0.115+0.310+residE
# LGA 
1.838/totalVar
# ward
0.115/totalVar
# village
0.310/totalVar


# penta1 on time
# run code as for table 10 to prepare
source("table9to12.r")
m1 <- glmer(penta1OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.477, ward 0.116, village 0.121
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.477+0.116+0.121+residE
# LGA 
0.477/totalVar
# ward
0.116/totalVar
# village
0.121/totalVar


# penta2 on time
# run code as for table 10 to prepare (if not run for penta1 on time)
#source("table9to12.r")
m1 <- glmer(penta2OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.477, ward 0.140, village 0.111
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.477+0.140+0.111+residE
# LGA 
0.477/totalVar
# ward
0.140/totalVar
# village
0.111/totalVar


# penta3 on time
# run code as for table 10 to prepare (if not already run)
#source("table9to12.r")
m1 <- glmer(penta3OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.649, ward 0.257, village 0.063
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.649+0.257+0.063+residE
# LGA 
0.649/totalVar
# ward
0.257/totalVar
# village
0.063/totalVar


# 3 penta doses in children aged 6-23 months
temp1<-allsmall[allsmall$ageChildCalcMonths>=6,]
temp1$penta3doses<-0
temp1$penta3doses[temp1$numPenta==3]<-1
temp1$penta3doses[is.na(temp1$numPenta)]<-NA
m1 <- glmer(penta3doses ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp1, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.615, ward 0.224, village 0.119
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.615+0.224+0.119+residE
# LGA 
0.615/totalVar
# ward
0.224/totalVar
# village
0.119/totalVar



m1 <- glmer(penta3OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.649, ward 0.257, village 0.063
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.649+0.257+0.063+residE
# LGA 
0.649/totalVar
# ward
0.257/totalVar
# village
0.063/totalVar



# measles on time
# run code as for table 10 to prepare (if not already run)
#source("table9to12.r")
m1 <- glmer(measlesOnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.396, ward 0.158, village 0.039
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.396+0.158+0.039+residE
# LGA 
0.396/totalVar
# ward
0.158/totalVar
# village
0.039/totalVar



# attend ANC
m1 <- glmer(attendANC ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.352, ward 0.455, village 0.181
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.352+0.455+0.181+residE
# LGA 
0.352/totalVar
# ward
0.455/totalVar
# village
0.181/totalVar


# everHadMeasles
m1 <- glmer(everHadMeasles ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# random effects output for variance on logit scale
# LGA 0.482, ward 0.093, village 0.034
# ICC logit up to date
residE<-(pi^2)/3
residE
totalVar<-0.482+0.093+0.034+residE
# LGA 
0.482/totalVar
# ward
0.093/totalVar
# village
0.034/totalVar



# --------------------------------------------------------
