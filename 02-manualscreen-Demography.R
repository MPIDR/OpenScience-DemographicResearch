
## --------------------------------------------------------- ##
##
##  FILE 02: manually screening all articles with open study 
##  materials to remove false positives in Demography  
##
##  sessionInfo() details:
##
##  R version 4.3.2 (2023-10-31 ucrt)
##  Platform: x86_64-w64-mingw32/x64 (64-bit)
##  Running under: Windows Server x64 (build 17763)
##  
##  attached base packages:
##  stats  graphics  grDevices  utils  datasets 
##  methods  base     
## 
##  other attached packages:
##  lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4     
##  purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    
##  ggplot2_3.4.4   tidyverse_2.0.0
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
load("01-Demography.Rdata")

##---- manual screening to remove false positives

## checking for duplicates
DF.long %>% select(Name) %>%
  group_by(Name) %>%
  summarise(count=n()) %>% filter(count>1)

## df of interest
checkDF <- DF.long %>% 
  filter(OpenMaterials==1)

## detected false positives
falsepos_pap <- c("111stelter.pdf","31leone.pdf","321mogi.pdf","393kabatek.pdf",
                  "821harvey.pdf","1373hellstrand.pdf","1499hill.pdf",
                  "1955andriano.pdf",
                  "2337hacker.pdf","137boissonneault.pdf","27behrman.pdf",
                  "37mark.pdf","433kucheva.pdf",
                  "563smith-greenaway.pdf","629payne.pdf","1045conley.pdf",
                  "1093dribe.pdf","949payne.pdf","1249bernard.pdf",
                  "1299newmyer.pdf","1377harkness.pdf","1631conte.pdf",
                  "1763pinchak.pdf","2187scotti.pdf",
                  "123dong.pdf","327alvarez.pdf","517garciabrazales.pdf",
                  "707oflaherty.pdf","915yu.pdf","1139gorman.pdf","1207pessin.pdf","1815boen.pdf")


## adjusting
sum(DF.long$OpenMaterials)
sum(DF.long$OpenMaterials) - length(falsepos_pap)
DF.long <- DF.long %>% 
  mutate(FalsePos=0,
         FalsePos=case_when(
           Name %in% falsepos_pap ~ 1,
           TRUE ~ FalsePos),
         OpenMaterials=case_when(
           FalsePos == 1 ~ 0,
           TRUE ~ OpenMaterials)) 
sum(DF.long$OpenMaterials)

## saving
setwd(DIR)
save(DF.long,file="02-Demography-cleaned.Rdata")

## END
