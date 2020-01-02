

# table8.r
# vaccination status by sub-group


# -- BY AGE-GROUP ---

allsmall$agecat <- 0
allsmall$agecat[allsmall$ageChildCalcMonths>=12]<-12
# 0-11 months
allsmall2<-allsmall[allsmall$agecat==0,]
table(allsmall2$status, allsmall2$intervSurvey)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)

# 12-23 months
allsmall2<-allsmall[allsmall$agecat==12,]
table(allsmall2$status, allsmall2$intervSurvey)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)

# agecat interaction test
allsmall$interv2agecat<-(allsmall$agecat/12)*allsmall$interv2
allsmall$interv3agecat<-(allsmall$agecat/12)*allsmall$interv3
# calcAtLeastPar, calcUpToDate
m1 <- glmer(calcAtLeastPar ~ interv2 + interv3 + as.factor(agecat) + interv2agecat + interv3agecat + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# calcAtLeastPar, calcUpToDate
m1 <- glmer(calcUpToDate ~ interv2 + interv3 + as.factor(agecat) + interv2agecat + interv3agecat + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# nb zero in one category




# -- BY STRATIFICATION ZONE --- 

allsmall$zone<-0
allsmall$lga<-as.character(allsmall$lga)
allsmall$zone[allsmall$lga=="odukpani" | allsmall$lga=="biase"]<-1
allsmall$zone[allsmall$lga=="ogoja" | allsmall$lga=="obudu"]<-2
allsmall$zone[allsmall$lga=="calabar_municipality" | allsmall$lga=="ikom"]<-3
# 0=central rural, 1=south rural, 2=north urban, 3=mixed urban
# central rural
allsmall2<-allsmall[allsmall$zone==0,] 
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)
# south rural
allsmall2<-allsmall[allsmall$zone==1,] 
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)
# north urban
allsmall2<-allsmall[allsmall$zone==2,] 
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)
# mixed urban
allsmall2<-allsmall[allsmall$zone==3,] 
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)



# ---- BY DISTANCE TO HEALTH FACILITY ----- 

allsmall$HF30p<-0
allsmall$HF30p[allsmall$HFdistance=="30mins_to_45mins"]<-1
allsmall$HF30p[allsmall$HFdistance=="LessThan_45_mins_to_1hr_walk"]<-1
allsmall$HF30p[allsmall$HFdistance=="moreThan_an_hour_walk"]<-1
allsmall$HF30p[allsmall$HFdistance==""]<-NA

# run for each combination of HF20 (0,1) and control/interv arm
temp1<-allsmall[allsmall$researchArm==1 & allsmall$HF30p==1,]
table(temp1$status, temp1$survey,exclude=NULL)
prop.table(table(temp1$status, temp1$survey),margin=2)

# check interactions
allsmall$HF30pS2<-allsmall$HF30p*allsmall$interv2
allsmall$HF30pS3<-allsmall$HF30p*allsmall$interv3
m1 <- glmer(calcUpToDate ~ interv2 + interv3  + HF30p + HF30pS2 + HF30pS3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)


# ---- BY HARD TO REACH CLASSIFICATION -----

# hard to reach
allsmall2<-allsmall[allsmall$hardToReach==1,]
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)

# not hard to reach
allsmall2<-allsmall[allsmall$hardToReach==0,]
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)

# check interactions
allsmall$harS2<-as.numeric(allsmall$hardToReach)*allsmall$interv2
allsmall$harS3<-as.numeric(allsmall$hardToReach)*allsmall$interv3
m1 <- glmer(calcUpToDate ~ interv2 + interv3  + hardToReach + harS2 + harS3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)


# --- BY SEX OF CHILD ----- 
#male
allsmall2<-allsmall[allsmall$femaleChild==0,]
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)
#female
allsmall2<-allsmall[allsmall$femaleChild==1,]
table(allsmall2$status, allsmall2$intervSurvey, exclude=NULL)
prop.table(table(allsmall2$status, allsmall2$intervSurvey), margin=2)

# sex of child interaction test
# calcAtLeastPar, calcUpToDate
allsmall$interv2female<-allsmall$interv2*as.numeric(allsmall$femaleChild)
allsmall$interv3female<-allsmall$interv3*as.numeric(allsmall$femaleChild)
m1 <- glmer(calcAtLeastPar ~ interv2 + interv3 + as.factor(femaleChild) + interv2female + interv3female + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)


# ---------------------------------------------------------------






