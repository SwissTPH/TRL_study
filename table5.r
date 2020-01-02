

# table5.r
# Table 5: immunization cards seen



# baseline
# has some missings - classify as no for this (ie recorded as not having been seen)
base$immcardseen <- 0
base$immcardseen[base$Q21_Immunization_card=="Seen"]<-1

base2<-base[base$researchArm=="control",]
table(base2$agecat)
table(base2$agecat, base2$immcardseen)
prop.table(table(base2$agecat,base2$immcardseen), margin=1)

base2<-base[base$researchArm=="intervention",]
table(base2$agecat)
table(base2$agecat, base2$immcardseen)
prop.table(table(base2$agecat,base2$immcardseen), margin=1)



# mid
# has some missings - classify as no for this (ie recorded as not having been seen)
mid$immcardseen <- 0
mid$immcardseen[mid$Q21_Immunization_card=="Seen"]<-1

mid2<-mid[mid$researchArm=="control",]
table(mid2$agecat)
table(mid2$agecat, mid2$immcardseen)
prop.table(table(mid2$agecat,mid2$immcardseen), margin=1)

mid2<-mid[mid$researchArm=="intervention",]
table(mid2$agecat)
table(mid2$agecat, mid2$immcardseen)
prop.table(table(mid2$agecat,mid2$immcardseen), margin=1)


# final
# has some missings - classify as no for this (ie recorded as not having been seen)
final$immcardseen <- 0
final$Q21_Immunization_card<-final$Q21.Immunization.card
final$immcardseen[final$Q21_Immunization_card=="Seen"]<-1

final2<-final[final$researchArm=="control",]
table(final2$agecat)
table(final2$agecat, final2$immcardseen)
prop.table(table(final2$agecat,final2$immcardseen), margin=1)

final2<-final[final$researchArm=="intervention",]
table(final2$agecat)
table(final2$agecat, final2$immcardseen)
prop.table(table(final2$agecat,final2$immcardseen), margin=1)

# ---------------------------------------------------------
