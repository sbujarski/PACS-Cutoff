#Emily's paper
#PACS cutoffs

setwd("C:/Users/sbuja/Documents/Manuscripts for Publication/Emily - PACS/PACS Cutoff")

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
  #geom_text(aes(label=cutoff), nudge_y=0.03) +
  geom_line(data=data.frame(x=c(0,1), y=c(0,1)), aes(x=x,y=y)) +
  annotate("rect", fill="white", colour="black", xmin = .03, xmax = .4, ymin = .875, ymax = 1.05) +
  annotate("text", label="Optimal Cutpoint >=15\nSensitivity = 0.671\nSpecificity = 0.815", 
           x=0.05, y=0.9, colour="black", size=4, fontface="bold", hjust=0, vjust=0) +
  geom_line(data=data.frame(x=c(.17, 0.1849057), y=c(0.875, .7)), aes(x=x, y=y), 
            arrow = arrow(length=unit(0.10,"in"), ends="last"), size = 1) +
  scale_x_continuous(limits=c(0,1.05), expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,1.06), expand=c(0,0)) + 
  SpTheme()
EmPACS.plot

ggsave(EmPACS.plot, filename="EmPACS.plot.png", dpi=500, width=6, height=5)


#Calculating Optimum
#best average sensitivity and specificity
CutoffTests$MeanSS <- (CutoffTests$Sensitivity + CutoffTests$Specificity)/2
max(CutoffTests$MeanSS)
#0.7431636, using cutoff of 15

#maximum perpendicular distance from 50/50 line
CutoffTests$Distance <- abs((1-CutoffTests$Specificity) - CutoffTests$Sensitivity)/sqrt(2)
max(CutoffTests$Distance)
#0.3438853, also usiing cutoff of 15





