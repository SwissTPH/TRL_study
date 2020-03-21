
# master.r 
# master file to call and describe the other R scripts


# install required packages if not already installed
# install.packages("lme4")
# install.packages("date")
require(lme4)
library(date)

# set working directory to where code and data is
setwd("C:/amanda/SCIH support/xavier/nigeria vacc cov/final survey & report/data & code for public - revised")

# prepare baseline, mid and final survey data
# during the project, analysis was done after every survey to report to 3ie
base<-read.csv("TRL baseline survey data anonym.csv", sep=",")
source("prepare_base.r")
mid<-read.csv("TRL midterm survey data anonym.csv", sep=",")
source("prepare_mid.r")
final<-read.csv("TRL final survey data anonym.csv", sep=",")
source("prepare_final.r")
 

# run R script to select and append variables needed for analyses of all surveys
source("append.r")
# creates a database called allsmall


# ANALYSES

# table 3: characteristics of the caregivers by survey and arm
source("table3.r",print=TRUE)

# table 4: characteristics of the children by survey and arm
source("table4.r", print=TRUE)

# table 5: immunization cards seen 
source("table5.r",print=TRUE)

# table 6: vaccination status of children
# table 7: estimated impact of the intervention 
source("table6&7.r",print=TRUE)

# table 8: vaccination status by subgroup
source("table8.r", print=TRUE)

# table 9: age at vaccination
# table 10: proportion of children who received vaccination on time
# table 11: number of Penta doses
# table 12: Drop-out: number of children who received Penta 1 on time but not Penta 3
source("table9to12.r",print=TRUE)

# table 13: other outcomes. health care utilization by mother and child illness
# table 14: ICC values
source("table13&14.r", print=TRUE)












