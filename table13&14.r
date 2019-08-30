

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

# ICC values taken from random effects 
m1 <- glmer(calcUpToDate ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# ICC logit for LGA up to date
residE<-(pi^2)/3
residE
totalVar<-0.39+0.212+0.062+residE
0.39/totalVar


# --------------------------------------------------------
