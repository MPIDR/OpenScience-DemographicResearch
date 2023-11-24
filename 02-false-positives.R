
## --------------------------------------------------------- ##
##
##  FILE 02: partially reproduce Figure 1 of the manuscript
##  (only for all sharable Open Access publications) 
##
##  sessionInfo() details:
##
##  R version 4.2.3 (2023-03-15 ucrt)
##  Platform: x86_64-w64-mingw32/x64 (64-bit)
##  Running under: Windows Server x64 (build 17763)
##  
##  attached base packages:
##  stats  graphics  grDevices  utils  datasets 
##  methods  base     
## 
##  other attached packages:
##  lubridate_1.9.2 forcats_1.0.0  
##  stringr_1.5.0   dplyr_1.1.1     purrr_1.0.1    
##  readr_2.1.4     tidyr_1.3.0     tibble_3.2.1   
##  ggplot2_3.4.1   tidyverse_2.0.0 
##
## --------------------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## set up the directory where .R is saved (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## packages
library(tidyverse)

## input and output data directory 
DIR <- paste0(getwd(),"/data/output")

## read Demography data (from text search) 
setwd(DIR)
load("Demography.Rdata")

##---- MANUAL CHECKING 1: extract two random issues from the three jorunals

## code to extract two random issues (based on sample function) removed
## but Volume 58, Issue 4 (2021) [included here] was randomly picked,
## hence the following adjustment

## adjust dataframe for 1 false negative
falseneg_pap <- c("1525feehan.pdf")

DF.long <- DF.long %>% 
  mutate(FalsePos=0,FalseNeg=0,
    FalseNeg=case_when(
      Name %in% falseneg_pap ~ 1,
      TRUE ~ FalseNeg),
    OpenMaterials=case_when(
      FalseNeg == 1 ~ 1,
      TRUE ~ OpenMaterials)) 
# sum(DF.long$FalsePos)
# sum(DF.long$FalseNeg)
# sum(DF.long$OpenMaterials)
# sum(DF.long$OpenMaterials)

##---- MANUAL CHECKING 2: screen for false positives
checkDF <- DF.long %>% 
  filter(OpenMaterials==1)

nrow(checkDF)

## finding false positives
falsepos_pap <- c("1059bernard.pdf")

## adjusting
sum(DF.long$OpenMaterials)
DF.long <- DF.long %>% 
  mutate(FalsePos=case_when(
    Name %in% falsepos_pap ~ 1,
    TRUE ~ FalsePos),
    OpenMaterials=case_when(
      FalsePos == 1 ~ 0,
      TRUE ~ OpenMaterials)) 
sum(DF.long$OpenMaterials)

## saving
setwd(DIR)
save(DF.long,file="Demography-cleaned.Rdata")


## END
