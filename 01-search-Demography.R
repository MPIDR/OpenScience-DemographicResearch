
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

##-----------------------------------------------------------##
## IMPORTANT NOTE BEFORE STARTING THE ANALYSIS !!!
## for some papers, a strange character is imported by pdftools.
## this is unfortunately not rendered well by OSF, hence it 
## is necessary to copy it manually in line 147
## Here's how to visualise the strange character (a dot with a red space):
## run all code until line 94, then use the following code (here commented)
##
# vol <- 13
# setwd(inputDIR)
# zipF <- dir()[vol]
# unzip(zipF,exdir=tempDIR)
# file_list <- list.files(path=tempDIR,pattern="*.pdf")
# setwd(tempDIR)
# all_files <- lapply(file_list, FUN = function(files) {
#   pdftools::pdf_text(files)
# })
# paper <- all_files[12]
# text <- tolower(unlist(paper))
# text[10]
##
## looking at the printed word reproducibility (or any other word) 
## in text[10] and copying it in Rstudio, it appears that there are 
## some weird spaces, i.e. it is the word "repro­duc­ibil­ity"
## ---> the strange charachter should be copied in line 147
## Then, rerun everything from start
##-----------------------------------------------------------##


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

## final dataframe where to store results
DF <- tibble(Year=rep(NA,n.vol),Volume=rep(NA,n.vol),
             Issue=rep(NA,n.vol),Articles=rep(NA,n.vol),
             OpenAccess=rep(NA,n.vol),OpenMaterials=rep(NA,n.vol),
             Journal=rep("Demography",n.vol))



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
  DF$Volume[vol] <- str_nth_number(zipF, n = 1)
  DF$Issue[vol] <- str_nth_number(zipF, n = 2)
  DF$Year[vol] <- str_nth_number(zipF, n = 3)
  cat("Analysing volume",DF$Volume[vol],
      "issue", DF$Issue[vol],
      "year",DF$Year[vol],"\n")
  
  ## number of articles in this volume
  n <- length(all_files)
  DF$Articles[vol] <- n
  
  ## create long dataframe for storing individual-level results (i.e. for each paper)
  DF.long.temp <- tibble(Year=rep(DF$Year[vol],n),Volume=rep(DF$Volume[vol],n),
                         Issue=rep(DF$Issue[vol],n),Article=rep(NA,n),
                         OpenAccess=rep(0,n),OpenMaterials=rep(0,n),
                         OMpage=rep(NA,n),OMkeyword=rep(NA,n),
                         Journal=rep("Demography",n))
  
  ## vectors to store number of OA and OM in the issue (1 yes, 0 no)
  open.access <- open.materials <- numeric(n)
  
  ## analyse each individual paper
  i <- 1
  for (i in 1:n){
    ## save article name
    DF.long.temp$Article[i] <- file_list[i]
    
    ## transform paper to text
    paper <- all_files[i]
    text <- tolower(unlist(paper))
    
    ## COPY HERE THE STRANGE CHARACTER!!!
    ## transformed text to remove ­ character sometimes imported by pdftools
    text2 <- gsub("­", "", tolower(unlist(paper)))
    
    ## find page with Open Access mention
    whi <- which(str_detect(text,paste("creative commons",collapse = '|')))
    whi2 <- which(str_detect(text2,paste("creative commons",collapse = '|')))
    if (length(whi)>0 | length(whi2)>0){
      open.access[i] <- 1
      DF.long.temp$OpenAccess[i] <- 1
    } 
    
    ## find page with Open Materials mention
    whi <- which(str_detect(text,paste(paste0("\\b",keywords,"\\b"),collapse = '|')))
    whi2 <- which(str_detect(text2,paste(paste0("\\b",keywords,"\\b"),collapse = '|')))
    if (length(whi)>0 | length(whi2)>0){
      open.materials[i] <- 1
      DF.long.temp$OpenMaterials[i] <- 1
      DF.long.temp$OMpage[i] <- whi2[1]
      ## extract specific keyword detected
      for (j in 1:length(keywords)){
        whi <- which(str_detect(text,paste0("\\b",keywords[j],"\\b")))
        whi2 <- which(str_detect(text2,paste0("\\b",keywords[j],"\\b")))
        if (length(whi)>0 | length(whi2)>0){
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
  
  ## count Open Access and Open Materials articles
  DF$OpenAccess[vol] <- sum(open.access)
  DF$OpenMaterials[vol] <- sum(open.materials)
  
}

## save data
setwd(outDIR)
save(DF,DF.long,file="Demography.Rdata")

##---- plotting

## plot share of Open Access articles by year 
DF %>% 
  group_by(Year) %>% 
  summarise(Articles=sum(Articles),OpenAccess=sum(OpenAccess)) %>% 
  mutate(Year=factor(Year),frac=OpenAccess/Articles) %>% 
  ggplot(aes(x=Year,y=frac, group=1)) +
  geom_point(size=4) + geom_line() +
  labs(title=expression(paste("Share of OA articles in ", italic("Demography"))),
       y="Percentage") +
  theme_bw(base_size = 16) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))


## plot share of Open Materials articles by year 
DF %>% 
  group_by(Year) %>% 
  summarise(Articles=sum(Articles),OpenMaterials=sum(OpenMaterials)) %>% 
  mutate(Year=factor(Year),frac=OpenMaterials/Articles) %>% 
  ggplot(aes(x=Year,y=frac, group=1)) +
  geom_point(size=4) + geom_line() +
  labs(title=expression(paste("Share of articles with Open Materials in ", italic("Demography"))),
       y="Percentage") +
  theme_bw(base_size = 16) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1))


## END
