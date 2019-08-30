# table9to12.r
# Tables 9, 10, 11 & 12


# -- TABLE 9 -------

# age at vaccination (in weeks)
allsmall$agePenta1<-as.numeric(as.character(allsmall$agePenta1))
allsmall$agePenta2<-as.numeric(as.character(allsmall$agePenta2))
allsmall$agePenta3<-as.numeric(as.character(allsmall$agePenta3))
allsmall$ageMeasles<-as.numeric(as.character(allsmall$ageMeasles))

temp1<-allsmall[allsmall$researchArm==1 & allsmall$survey==3,]
# this is for intervention arm & survey 3. Run for each combination of survey and arm.
quantile(temp1$agePenta1, probs=c(0.0,0.25,0.5,0.75,1.0), na.rm=TRUE)
quantile(temp1$agePenta2, probs=c(0.0,0.25,0.5,0.75,1.0), na.rm=TRUE)
quantile(temp1$agePenta3, probs=c(0.0,0.25,0.5,0.75,1.0), na.rm=TRUE)
quantile(temp1$ageMeasles, probs=c(0.0,0.25,0.5,0.75,1.0), na.rm=TRUE)

 


# -- TABLE 10 ---------

# proportion with vaccination on time of those old enough

# Penta 1 on time
allsmall$penta1OnTime<-0
allsmall$penta1OnTime[allsmall$agePenta1<=8 & allsmall$agePenta1>4]<-1
allsmall$penta1OnTime[(allsmall$ageChildCalcDays/7)<8]<-NA

table(allsmall$penta1OnTime, allsmall$intervSurvey)
prop.table(table(allsmall$penta1OnTime, allsmall$intervSurvey), margin=2)

m1 <- glmer(penta1OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])


# Penta 2 on time
allsmall$penta2OnTime<-0
allsmall$penta2OnTime[allsmall$agePenta2<=12 & allsmall$agePenta2>8]<-1
allsmall$penta2OnTime[(allsmall$ageChildCalcDays/7)<12]<-NA

table(allsmall$penta2OnTime, allsmall$intervSurvey)
prop.table(table(allsmall$penta2OnTime, allsmall$intervSurvey), margin=2)

m1 <- glmer(penta2OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])


# penta3 on time
allsmall$penta3OnTime<-0
allsmall$penta3OnTime[allsmall$agePenta3<=16 & allsmall$agePenta3>12]<-1
allsmall$penta3OnTime[(allsmall$ageChildCalcDays/7)<16]<-NA

table(allsmall$penta3OnTime, allsmall$intervSurvey)
prop.table(table(allsmall$penta3OnTime, allsmall$intervSurvey), margin=2)

m1 <- glmer(penta3OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])


# measles on time
allsmall$measlesOnTime<-0
allsmall$measlesOnTime[allsmall$ageMeasles<=41 & allsmall$ageMeasles>37]<-1
allsmall$measlesOnTime[(allsmall$ageChildCalcDays/7)<41]<-NA

table(allsmall$measlesOnTime, allsmall$intervSurvey)
prop.table(table(allsmall$measlesOnTime, allsmall$intervSurvey), margin=2)

m1 <- glmer(measlesOnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])
# to get OR and 95% CI
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])



# penta3 on time of those who had penta 1 on time

temp1<-allsmall[allsmall$penta1OnTime==1,]
table(temp1$penta3OnTime, temp1$intervSurvey)
prop.table(table(temp1$penta3OnTime, temp1$intervSurvey), margin=2)

m1 <- glmer(penta3OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp1, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])




# -- TABLE 11 ------- 

# number of penta vacc if child is aged 6-23mo


temp1<-allsmall[allsmall$ageChildCalcMonths>=6,]

table(temp1$numPenta, temp1$intervSurvey)
prop.table(table(temp1$numPenta, temp1$intervSurvey), margin=2)

# estimate ORs and CIs
temp1$penta3doses<-0
temp1$penta3doses[temp1$numPenta==3]<-1
temp1$penta3doses[is.na(temp1$numPenta)]<-NA

m1 <- glmer(penta3doses ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp1, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv2",2])

exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])




# --- TABLE 12 -----

# if child aged 14+2 weeks
temp1<-allsmall[allsmall$ageChildCalcDays>=(16*7),]
# control arm
temp2<-temp1[allsmall$researchArm==0,]
table(temp2$numPenta, temp2$survey)
prop.table(table(temp2$numPenta, temp2$survey),margin=2)
# intervention arm
temp2<-temp1[allsmall$researchArm==1,]
table(temp2$numPenta, temp2$survey)
prop.table(table(temp2$numPenta, temp2$survey),margin=2)


# penta 1 but not penta 3
temp1$penta3<-0
temp1$penta3[!is.na(temp1$agePenta3)]<-1
temp2<-temp1[as.numeric(temp1$agePenta1)>0,]
# control arm
temp3<-temp2[temp2$researchArm==0,]
table(temp3$penta3, temp3$survey)
prop.table(table(temp3$penta3, temp3$survey),margin=2)
# intervention arm
temp3<-temp2[temp2$researchArm==1,]
table(temp3$penta3, temp3$survey)
prop.table(table(temp3$penta3, temp3$survey),margin=2)


m1 <- glmer(penta3 ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp2, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv2",2])

exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])



# penta3 'dropout'
# intervention arm
temp1<-allsmall[allsmall$penta1OnTime==1 & allsmall$researchArm==1,]
table(temp1$penta3OnTime, temp1$survey)
prop.table(table(temp1$penta3OnTime, temp1$survey),margin=2)

# control arm
temp1<-allsmall[allsmall$penta1OnTime==1 & allsmall$researchArm==0,]
table(temp1$penta3OnTime, temp1$survey)
prop.table(table(temp1$penta3OnTime, temp1$survey),margin=2)

temp1<-allsmall[allsmall$penta1OnTime==1,]
m1 <- glmer(penta3OnTime ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp1, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv2",2])

# ------------------------------------------------




