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


#Analyze PACS cutoffs
#loop through PACS >=1, through >=29
#compute sensitivity and specificity for SCID.CraveC outcome
CutoffTests <- data.frame(cutoff=1:29, Sensitivity=NA, Specificity=NA)
for(i in 1:29){
  EmPACS$PACS.Bin <- ifelse(EmPACS$PACS>=i,1,0)
  CrossTable(EmPACS$SCID.CraveC,EmPACS$PACS.Bin)
  #sensitivity = True Detected Positives / (Total Positive Events)
  CutoffTests$Sensitivity[i] <- table(EmPACS$SCID.CraveC,EmPACS$PACS.Bin)[2,2] / sum(table(EmPACS$SCID.CraveC,EmPACS$PACS.Bin)[2,])
  #Specificity = True Detected Negatives / (Total Negative Events)
  CutoffTests$Specificity[i] <- table(EmPACS$SCID.CraveC,EmPACS$PACS.Bin)[1,1] / sum(table(EmPACS$SCID.CraveC,EmPACS$PACS.Bin)[1,])
}

#help with plotting glitch
CutoffTests$Specificity[25] <- CutoffTests$Specificity[25]  - 0.0001

EmPACS.plot <- ggplot(CutoffTests, aes(x=1-Specificity, y=Sensitivity)) + 
  geom_point() + 
  geom_line() + 
  #geom_text(aes(label=cutoff), nudge_y=0.02) +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), aes(x=x,y=y)) +
  scale_x_continuous(limits=c(0,1.05), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,1.05), expand=c(0,0)) + 
  SpTheme()
EmPACS.plot

ggsave()
