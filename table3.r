
# table3.r
# analyses for Table 3 of report 

# -- TABLE 3: Characteristics of the respondents ---- 


# -- AGE OF CARER IN YEARS  ----

# From baseline survey
table(base$agecarercat, base$researchArm, exclude=NULL)
prop.table(table(base$agecarercat, base$researchArm), margin=2)

# From mid-survey
table(mid$agecarercat, mid$researchArm, exclude=NULL)
prop.table(table(mid$agecarercat, mid$researchArm, exclude=NULL),margin=2)

# From final survey
table(final$agecarercat, final$researchArm, exclude=NULL)
prop.table(table(final$agecarercat, final$researchArm, exclude=NULL),margin=2)

# test for baseline survey only
# small numbers in 40+ age-groups so combine with 40+
base$agecarercat2<-base$agecarercat
base$agecarercat2[base$agecarercat>40]<-40

#m1 <- glmer(interv ~ as.factor(agecarercat2) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
#  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#summary(m1)
# random effects for ward and village are not needed (shown by above model which is commented out)
m1 <- glmer(interv ~ as.factor(agecarercat2) + (1 | lgaNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(agecarercat))
anova(m1,m2)




# -- LEVEL OF EDUCATION OF CAREGIVERS --- 

# From baseline survey
table(base$Q7_level_of_educ_of_caregiver, base$Q7.level.of.educ.merged, exclude=NULL)
table(base$Q7.level.of.educ.merged, base$researchArm)
prop.table(table(base$Q7.level.of.educ.merged, base$researchArm), margin=2)

# compare at baseline
# m1 <- glmer(interv ~ as.factor(Q7.level.of.educ.merged) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
#  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#summary(m1)
# above model(commented out) shows the random effects for ward and village cannot be estimated (overfit) 
# use the model below 
m1 <- glmer(interv ~ as.factor(Q7.level.of.educ.merged) + (1 | lgaNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(Q7.level.of.educ.merged))
anova(m1,m2)

# From mid-term survey
table(mid$educCat, mid$researchArm, exclude=NULL)
prop.table(table(mid$educCat, mid$researchArm, exclude=NULL), margin=2)

# From final survey
table(final$educCat, final$researchArm, exclude=NULL)
prop.table(table(final$educCat, final$researchArm, exclude=NULL), margin=2)



# -- RELIGIOUS AFFILIATION ----

# base
table(base$Q10_Religious_Affiliation, base$Q10b_other_religious_affiliation, exclude=NULL)
table(base$Q10_Religious_Affiliation, base$researchArm)
prop.table(table(base$Q10_Religious_Affiliation, base$researchArm), margin=2)

# mid survey
table(mid$Q10_Religious_Affiliation, mid$researchArm)
prop.table(table(mid$Q10_Religious_Affiliation, mid$researchArm), margin=2)

# final
table(final$Q10_Religious_Affiliation, final$researchArm, exclude=NULL)
prop.table(table(final$Q10_Religious_Affiliation, final$researchArm, exclude=NULL), margin=2)

# test of intervention vs control arm at baseline
base$Q10_Religious_Affiliation[base$Q10_Religious_Affiliation=="None"]<-"Other_Rel_Affiliation"
#m1 <- glmer(interv ~ as.factor(Q10_Religious_Affiliation) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
#  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#summary(m1)
# including random effects for ward and village leads to singularity (overfitting)
# use the model below
m1 <- glmer(interv ~ as.factor(Q10_Religious_Affiliation) + (1 | lgaNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(Q10_Religious_Affiliation))
anova(m1,m2)



# -- WHERE HELP WAS SOUGHT LAST FOR CHILD'S ILL HEALTH ---

# From baseline survey
base1<-base[base$whereSoughtTrt!="",]
table(base1$whereSoughtTrt, base1$researchArm, exclude=NULL)
prop.table(table(base1$whereSoughtTrt, base1$researchArm), margin=2)

# From mid-term survey
table(mid$whereSoughtTrt, mid$researchArm, exclude=NULL)
prop.table(table(mid$whereSoughtTrt[mid$whereSoughtTrt!=""], mid$researchArm[mid$whereSoughtTrt!=""]), margin=2)

# From final survey
table(final$whereSoughtTrt, final$researchArm, exclude=NULL)
prop.table(table(final$whereSoughtTrt[final$whereSoughtTrt!=""], final$researchArm[final$whereSoughtTrt!=""]), margin=2)

# test of intervention vs control arm at baseline
#m1 <- glmer(interv ~ as.factor(whereSoughtTrt) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
#  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#summary(m1)
# three random effects leads to singularity - removed ward and village for fitting 
m1 <- glmer(interv ~ as.factor(whereSoughtTrt) + (1 | lgaNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(Q10_Religious_Affiliation))
anova(m1,m2)



# -- DISTANCE TO HEALTH FACILITY --- 

# From baseline survey

table(base$Q12_How_close_is_the_HF_you_use, base$researchArm, exclude=NULL)
prop.table(table(as.character(base$Q12_How_close_is_the_HF_you_use), base$researchArm), margin=2)

#m1 <- glmer(interv ~ as.factor(Q12_How_close_is_the_HF_you_use) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
#  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#summary(m1)
#model without ward and village is preferred (to avoid singularity/overfitting)
m1 <- glmer(interv ~ as.factor(Q12_How_close_is_the_HF_you_use) + (1 | lgaNum) ,
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(Q12_How_close_is_the_HF_you_use))
anova(m1,m2)



# From mid-term survey
table(mid$Q12_How_close_is_the_HF_you_use, mid$researchArm, exclude=NULL)
prop.table(table(mid$Q12_How_close_is_the_HF_you_use, mid$researchArm, exclude=NULL), margin=2)

# From the final survey
table(final$Q12_How_close_is_the_HF_you_use, final$researchArm, exclude=NULL)
prop.table(table(final$Q12_How_close_is_the_HF_you_use, final$researchArm, exclude=NULL), margin=2)



# -- HARD TO REACH --- 

# baseline survey
table(base$hardToReach, base$researchArm)
prop.table(table(base$hardToReach, base$researchArm), margin=2)

# mid-term survey
table(mid$hardToReach, mid$researchArm)
prop.table(table(mid$hardToReach, mid$researchArm), margin=2)

# final survey
table(final$hardToReach, final$researchArm)
prop.table(table(final$hardToReach, final$researchArm), margin=2)

#m1 <- glmer(interv ~ as.factor(hardToReach) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
#  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
#summary(m1)
# model without ward and village is preferred due to singularity (overfitting) 
m1 <- glmer(interv ~ as.factor(hardToReach) + (1 | lgaNum) ,
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)



# -----------------------------------------------------------






