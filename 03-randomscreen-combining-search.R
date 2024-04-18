
## --------------------------------------------------------- ##
##
##  FILE 03: performing a random screen of some volumes and
##  issues to validate the two-step search algorithm, and 
##  combining text searches in a single file
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

## directories
DIR <- paste0(getwd(),"/data/output")

##------- READING TEXT SEARCH DATA  -----------------------

## read Demography data (from text search) 
setwd(DIR)
load("02-Demography-cleaned.Rdata")
DFall <- DF.long 
vols.demo <- unique(DF.long$Volume)
issues.demo <- unique(DF.long$Issue)

## read PDR data (from text search) 
## code removed as PDR data not sharable

## read Pop Studies data (from text search) 
## code removed as Pop Studies data not sharable

## read Dem Res data (from text search) 
load("02-DemRes-cleaned.Rdata")
DFall <- DFall %>% 
  bind_rows(DF.long)
vols.dr <- unique(DF.long$Volume)

##---- RANDOM SCREEN MANUAL CHECKING ------------

## extract two random issues from the four journals

## first run
set.seed(1)
DFdemo <- DFall %>% 
  filter(Journal=="Demography",Volume==sample(vols.demo,1),
         Issue==sample(issues.demo,1))

## now one volume of DR
DFdr <- DFall %>% 
  filter(Journal=="Dem Res",Volume==sample(vols.dr,1))

## repeating (without DR)
set.seed(100)
DFdemo <- DFall %>% 
  filter(Journal=="Demography",Volume==sample(vols.demo,1),
         Issue==sample(issues.demo,1))

## manually inspecting the articles in these issues and volume
## did not reveal any false positives or negatives

##---- COMBINING ALL TEXT SEARCH DATA ------------

## here commented out since I have copied the longer dataset
## that contains articles also for the non-sharable publications
## in Pop Studies, PDR and Demography

## saving combined dataframe
# setwd(DIR)
# save(DFall,file="03-combined-cleaned.Rdata")


## END
