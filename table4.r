
# table4.r
# analyses for Table 4 of report 

# -- TABLE 4: Characteristics of the children ---- 


# -- AGE OF CHILD IN MONTHS  ----

# From baseline survey
table(base$agecat, base$researchArm, exclude=NULL)
prop.table(table(base$agecat, base$researchArm, exclude=NULL), margin=2)

m1 <- glmer(interv ~ as.factor(agecat) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(agecat))
anova(m1,m2)

# mid-term survey
table(mid$agecat, mid$researchArm, exclude=NULL)
prop.table(table(mid$agecat, mid$researchArm, exclude=NULL), margin=2)

# final survey
table(final$agecat, final$researchArm, exclude=NULL)
prop.table(table(final$agecat, final$researchArm, exclude=NULL), margin=2)

 

# --- SEX OF CHILD ----- 

# baseline
table(base$Q18_Sex_of_child, base$researchArm, exclude=NULL)
prop.table(table(base$Q18_Sex_of_child, base$researchArm, exclude=NULL), margin=2)
 
m1 <- glmer(interv ~ as.factor(Q18_Sex_of_child) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(Q18_Sex_of_child))
anova(m1,m2)

# mid-term
table(mid$Q18_Sex_of_child, mid$researchArm, exclude=NULL)
prop.table(table(mid$Q18_Sex_of_child, mid$researchArm, exclude=NULL), margin=2)
  
# final
table(final$Q18_Sex_of_child, final$researchArm, exclude=NULL)
prop.table(table(final$Q18_Sex_of_child, final$researchArm, exclude=NULL), margin=2)
  


# -- BIRTH ORDER OF CHILD --- 
 
# baseline 
table(base$Q19_Birth_order_of_the_child, base$researchArm, exclude=NULL)
prop.table(table(base$Q19_Birth_order_of_the_child, base$researchArm, exclude=NULL), margin=2)

m1 <- glmer(interv ~ as.factor(Q19_Birth_order_of_the_child) + (1 | lgaNum) + (1 | wardNum) + (1 | villageNum),
  data = base, family = binomial, control = glmerControl(optimizer = "bobyqa"))
summary(m1)
m2 <- update(m1,~.-as.factor(Q19_Birth_order_of_the_child))
anova(m1,m2)

# mid-term
table(mid$Q19_Birth_order_of_the_child, mid$researchArm, exclude=NULL)
prop.table(table(mid$Q19_Birth_order_of_the_child, mid$researchArm, exclude=NULL), margin=2)
 
# final
table(final$Q19_Birth_order_of_the_child, final$researchArm, exclude=NULL)
prop.table(table(final$Q19_Birth_order_of_the_child, final$researchArm, exclude=NULL), margin=2)


# -------------------------------------------------------
