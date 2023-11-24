
## --------------------------------------------------------- ##
##
##  FILE 01: estimate share of articles with Open Access and 
##  Open Study Materials in relevant publications of Demography  
##  during the years 2021-2023 (all sharable Open Access publications) 
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
##  strex_1.6.0     lubridate_1.9.2 forcats_1.0.0  
##  stringr_1.5.0   dplyr_1.1.1     purrr_1.0.1    
##  readr_2.1.4     tidyr_1.3.0     tibble_3.2.1   
##  ggplot2_3.4.1   tidyverse_2.0.0 pdftools_3.4.0
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

## input directory (where Demography papers are stored)
inputDIR <- paste0(getwd(),"/data/input/Demography")
## output directory 
outDIR <- paste0(getwd(),"/data/output")
## temporary directory 
tempDIR <- paste0(getwd(),"/data/temp")

## delete temporary folder if present
unlink(tempDIR,recursive = T)

## loading list of keywords
source("funs/keywords.R")

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
    # message(files)
    pdftools::pdf_text(files)
    
  })
  
  ## delete temporary folder
  unlink(tempDIR,recursive = T)
  
  ## extract volume number, issue number and year (from zipped folder)
  volume <- str_nth_number(zipF, n = 1)
  issue <- str_nth_number(zipF, n = 2)
  year <- str_nth_number(zipF, n = 3)
  cat("Analysing volume",volume,
      "issue", issue,"year",year,"\n")
  
  ## number of articles in this volume
  n <- length(all_files)
  
  ## create long dataframe for storing individual-level results (i.e. for each paper)
  DF.long.temp <- tibble(Year=rep(year,n),Volume=rep(volume,n),
                         Issue=rep(issue,n),Article=rep(NA,n),Name=rep(NA,n),
                         OpenAccess=rep(0,n),OpenMaterials=rep(0,n),
                         OMpage=rep(NA,n),OMkeyword=rep(NA,n),
                         Journal=rep("Demography",n))
  
  ## analyse each individual paper
  i <- 1
  for (i in 1:n){
    ## save article number and name
    DF.long.temp$Article[i] <- i
    DF.long.temp$Name[i] <- file_list[i]
    
    ## transform paper to text
    paper <- all_files[i]
    text <- tolower(unlist(paper))
    
    ## transformed text to remove ­ character sometimes imported by pdftools
    text2 <- gsub("­", "", tolower(unlist(paper)))
    
    ## find Open Access mention
    OAmention <- any(str_detect(text2,paste("creative commons",collapse = '|')))
    if (OAmention){
      DF.long.temp$OpenAccess[i] <- 1
    } 
    
    ## find Open Materials mention and page
    OMmention <- any(str_detect(text2,paste(paste0("\\b",keywords,"\\b"),collapse = '|')))
    whi <- which(str_detect(text2,paste(paste0("\\b",keywords,"\\b"),collapse = '|')))
    ## make sure this is not available upon request
    requestTRUE <- any(str_detect(text2,paste(paste0("\\b",keywordsREQUEST,"\\b"),collapse = '|')))
    if (OMmention & !requestTRUE){
      DF.long.temp$OpenMaterials[i] <- 1
      DF.long.temp$OMpage[i] <- whi[1]
      ## extract specific keyword detected
      for (j in 1:length(keywords)){
        whi <- which(str_detect(text2,paste0("\\b",keywords[j],"\\b")))
        if (length(whi)>0){
          DF.long.temp$OMkeyword[i] <- keywords[j]
          break
        }
      }
    } 
  }
  
  ## combine long dataframe
  if (vol==1){
    DF.long <- DF.long.temp
  }else{
    DF.long <- DF.long %>% 
      bind_rows(DF.long.temp)
  }
  
}

## save data
setwd(outDIR)
save(DF.long,file="Demography.Rdata")

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
