

# tables6&7.r
# Vaccination status of children 
# Estimated impact of the intervention



# -- Table 6 ----- 

table(allsmall$status, allsmall$intervSurvey, exclude=NULL)
prop.table(table(allsmall$status, allsmall$intervSurvey, exclude=NULL), margin=2)

# test at baseline
temp1<-allsmall[allsmall$survey==1,]
m1 <- glmer(calcUpToDate ~ interv + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp1, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
temp1<-allsmall[allsmall$survey==1,]
m1 <- glmer(calcAtLeastPar ~ interv + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = temp1, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)



# -- Table 7 ---

# ORs for fully up to date
m1 <- glmer(calcUpToDate ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv2",2])
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])

# ORs for at least partial
m1 <- glmer(calcAtLeastPar ~ interv2 + interv3 + as.factor(survey) + (1 | rlga) + (1 | rward) + (1 | rvillage) ,
  data = allsmall, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)

# to get OR and 95% CI
exp(summary(m1)$coefficients["interv2",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv2",2])
exp(summary(m1)$coefficients["interv3",1] + 
+     qnorm(c(0.025,0.5,0.975)) * summary(m1)$coefficients["interv3",2])

# ------------------------------------------------------





