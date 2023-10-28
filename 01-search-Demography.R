

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
    whi <- which(str_detect(text,paste(keywords,collapse = '|')))
    whi2 <- which(str_detect(text2,paste(keywords,collapse = '|')))
    if (length(whi)>0 | length(whi2)>0){
      open.materials[i] <- 1
      DF.long.temp$OpenMaterials[i] <- 1
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
