#Emily's paper
#PACS cutoffs

setwd("C:/Users/sbuja/Documents/Manuscripts for Publication/Emily - PACS")

#required libraries
library(SpPack)
library(multilevel) #MLM analysis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(xlsx) #package to import xls files directly
library(grid) #for multiplot
library(psych) #for alpha (cronbach's alpha function)
library(GGally) #ggpairs plots
library(gmodels) #CrossTable Function
library(Hmisc) #rcorr
library(stats)
library(pscl) # for mcfadden R2 in logistic regression
library(caret) #for crossvalidation methods
library(ROCR) #For crossvalidation AUC curve
library(scales) #for percent axis
library(Deducer)

#IMPORT DATA----
EmPACS <- read.csv("Emily PACS Dataset 010818.csv", header=T, na.strings=c("NA"))
SpDesc(EmPACS)

#recode craving symptom, NA=NA, 1,2=0, 3=1
EmPACS$SCID.CraveC <- ifelse(EmPACS$SCIDE14C==3,1,
                             ifelse(EmPACS$SCIDE14C<3,0,NA))
table(EmPACS$SCIDE14C)
table(EmPACS$SCID.CraveC)

EmPACS$PACS <- rowSums(EmPACS[c("pacs1", "pacs2", "pacs3", "pacs4", "pacs5")])
SpDesc(EmPACS$PACS)
SpHist(EmPACS, variable="PACS")



