
## --------------------------------------------------------- ##
##
##  FILE 02: manually screening all articles with open study 
##  materials to remove false positives in Demographic Research
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
load("01-DemRes.Rdata")

##---- manual screening to remove false positives

## checking for duplicates
DF.long %>% select(Name) %>%
  group_by(Name) %>%
  summarise(count=n()) %>% filter(count>1)

## removing eventual duplicates 
DF.long <- DF.long %>% 
  filter(! (Volume==23 & Name=="22-12.pdf"))

## df of interest
checkDF <- DF.long %>% 
  filter(OpenMaterials==1)

## detected false positives
falsepos_pap <- c("22-10.pdf","22-6.pdf","22-9.pdf","23-32.pdf",
                  "23-4.pdf","23-8.pdf","24-24.pdf","24-31.pdf",
                  "25-21.pdf","25-5.pdf","26-22.pdf","26-23.pdf",
                  "27-21.pdf","27-26.pdf","28-10.pdf","28-12.pdf",
                  "28-16.pdf","28-18.pdf","28-27.pdf","28-33.pdf",
                  "28-7.pdf","29-11.pdf","29-6.pdf","30-34.pdf",
                  "30-58.pdf","30-9.pdf","31-14.pdf","31-27.pdf",
                  "32-36.pdf","33-13.pdf","34-18.pdf","35-18.pdf",
                  "35-5.pdf","36-1.pdf","36-10.pdf","36-34.pdf",
                  "36-46.pdf","36-49.pdf","36-53.pdf","37-20.pdf",
                  "37-32.pdf","37-53.pdf","38-45.pdf","38-54.pdf",
                  "39-11.pdf","39-35.pdf","39-42.pdf","40-8.pdf",
                  "40-15.pdf","40-18.pdf","40-25.pdf","40-26.pdf",
                  "40-29.pdf","40-47.pdf","40-55.pdf","42-10.pdf",
                  "42-16.pdf","42-25.pdf","42-9.pdf","43-47.pdf",
                  "43-57.pdf","44-1.pdf","44-22.pdf","44-26.pdf",
                  "44-30.pdf","44-36.pdf","44-49.pdf","44-50.pdf",
                  "44-9.pdf","45-11.pdf","45-12.pdf","45-20.pdf",
                  "45-28.pdf","45-45.pdf","46-19.pdf","46-22.pdf",
                  "46-33.pdf","46-4.pdf","47-26.pdf","47-28.pdf",
                  "47-3.pdf","47-30.pdf","48-10.pdf","48-14.pdf",
                  "48-22.pdf","48-26.pdf","48-30.pdf","48-32.pdf",
                  "49-10.pdf")

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
save(DF.long,file="02-DemRes-cleaned.Rdata")

## END
