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

