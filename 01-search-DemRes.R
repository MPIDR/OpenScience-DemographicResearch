
## --------------------------------------------------------- ##
##
##  FILE 01: estimate share of articles with open software
##  in relevant publications of Demographic Research
##  during the years 2010-2023 (all sharable Open Access publications) 
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
##  strex_2.0.0     lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   
##  dplyr_1.1.4     purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     
##  tibble_3.2.1    ggplot2_3.4.4   tidyverse_2.0.0 pdftools_3.4.0
##
## --------------------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## set up the directory where .R is saved (R-studio command)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## packages
library(pdftools)
library(tidyverse)
library(strex)

## loading list of keywords
source("funs/keywords.R")

## input directory (where Demographic Research papers are stored)
inputDIR <- paste0(getwd(),"/data/input/DemographicResearch")
## output directory 
outDIR <- paste0(getwd(),"/data/output")
## temporary directory 
tempDIR <- paste0(getwd(),"/data/temp")

## delete temporary folder if present
unlink(tempDIR,recursive = T)


##----- starting the text search -----
setwd(inputDIR)

## number of volumes
n.vol <- length(dir())

## starting a for loop to analyse each volume separately
vol <- 1
for (vol in 1:n.vol){
  ## unzipping 
  setwd(inputDIR)
  zipF <- dir()[vol]
  unzip(zipF,exdir=tempDIR)
  
  ## loading all files
  file_list <- list.files(path=tempDIR,pattern="*.pdf")
  setwd(tempDIR)
  all_files <- lapply(file_list, FUN = function(files) {
    
    pdftools::pdf_text(files)
    
  })
  
  ## delete temporary folder
  unlink(tempDIR,recursive = T)
  
  ## extract volume number and year
  volume <- str_nth_number(zipF, n = 1)
  years <- rep(seq(2010, 2023, by = 1), each = 2)
  year <- years[vol] 
  cat("Analysing volume",volume,"year",year,"\n")
  
  ## number of articles in this volume
  n <- length(all_files)

  ## create long dataframe for storing results
  DF.long.temp <- tibble(Year=rep(year,n),Volume=rep(volume,n),
                         Issue=rep(NA,n),Article=rep(NA,n),
                         Name=rep(NA,n),OpenAccess=rep(1,n),OpenMaterials=rep(0,n),
                         OMpage=rep(NA,n),OMkeyword=rep(NA,n),
                         OMrequest=rep(0,n),Journal=rep("Dem Res",n))
  
  ## analyse each paper
  i <- 1
  for (i in 1:n){
    ## save article number and name
    DF.long.temp$Article[i] <- i
    DF.long.temp$Name[i] <- file_list[i]
    
    ## transom to text
    paper <- all_files[i]
    text<-tolower(unlist(paper))
    
    ## transformed text to remove ­ character sometimes imported by pdftools
    text2 <-gsub("­", "", tolower(unlist(paper)))
    
    ## find References page
    any_ref_page <- any(str_detect(text2,"\\breferences\\b"))
    
    ## exclude references (keep first page of references as sometimes it 
    ## coincides with data availability statement)
    if (any_ref_page){
      text2 <- text2[1:max(which(str_detect(text2,"\\breferences\\b")))]
    }
    
    ## find Open Access mention
    OAmention <- any(str_detect(text2,paste("creative commons",collapse = '|')))
    if (OAmention){
      DF.long.temp$OpenAccess[i] <- 1
    } 
    
    ## find Open Materials mention and page
    OMmention <- any(str_detect(text2,paste(paste0("\\b",keywords,"\\b"),collapse = '|')))
    whi <- which(str_detect(text2,paste(paste0("\\b",keywords,"\\b"),collapse = '|')))
    ## flag if this is available upon request (and check later)
    requestTRUE <- any(str_detect(text2,paste(paste0("\\b",keywordsREQUEST,"\\b"),collapse = '|')))
    if (OMmention){
      DF.long.temp$OpenMaterials[i] <- 1
      DF.long.temp$OMpage[i] <- paste(whi, collapse = "; ")
      if (requestTRUE) DF.long.temp$OMrequest[i] <- 1
      ## extract specific keywords detected
      keytemp <- list()
      k <- 1
      j <- 1
      for (j in 1:length(keywords)){
        whi <- which(str_detect(text2,paste0("\\b",keywords[j],"\\b")))
        if (length(whi)>0){
          keytemp[[k]] <-  keywords[j]
          k <- k+1
        }
      }
      DF.long.temp$OMkeyword[i] <- paste(unlist(keytemp), collapse = "; ")
    } 
  }
  
  if (vol==1){
    DF.long <- DF.long.temp
  }else{
    DF.long <- DF.long %>% 
      bind_rows(DF.long.temp)
  }
  
}

## save data
setwd(outDIR)
save(DF.long,file="01-DemRes.Rdata")

##---- plotting
DF.long %>% 
  group_by(Year) %>% 
  summarise(Articles=n(),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials)) %>% 
  mutate(Year=as.integer(Year),frac_OA=OpenAccess/Articles,
         frac_OM=OpenMaterials/Articles) %>% 
  pivot_longer(c(frac_OA,frac_OM)) %>% 
  mutate(name=recode(name,frac_OA="Open Access",
                     frac_OM="Open Study Materials")) %>% 
  ggplot(aes(x=Year,y=value, group=1)) +
  geom_point(size=2) + geom_line() +
  facet_wrap(.~name)+
  scale_shape_manual(values = c(15)) +
  labs(y="Percentage",color="Journal") +
  theme_bw(base_size = 14) +
  theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1))+
  scale_x_continuous(breaks= seq(min(DF.long$Year),max(DF.long$Year),2))

## END
