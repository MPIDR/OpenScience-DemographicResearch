
## --------------------------------------------------------- ##
##
##  FILE 05: reproduce Figure 2 of the manuscript
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

## directories
inputWEBSITE <- paste0(getwd(),"/data/input")
inputTEXT <- paste0(getwd(),"/data/output")
outDIR <- paste0(getwd(),"/output")

## read Demographic Research data (scraped from website) 
setwd(inputWEBSITE)
load("DemographicResearch-Website.Rdata")

## combine volumes
DFdr <- DF %>% 
  group_by(Year,Journal) %>% 
  summarise(Articles=sum(OpenAccess),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials)) %>% 
  mutate(Source="Website")

## read other data  (from text search) 
setwd(inputTEXT)
load("DemRes.Rdata")

##  
DFothers <- DF.long %>% 
  group_by(Year,Journal) %>% 
  summarise(Articles=n(),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials))%>% 
  mutate(Source="Text Search")

## final dataset
myDF <- DFdr %>% 
  bind_rows(DFothers)

##----- Figure 2 plot -----

## colors
my.cols <- rev(c("#7F3C8D", "#9F6554"))

## figure
myDF %>% 
  mutate(Year=as.integer(Year),
         frac_OM=OpenMaterials/Articles) %>% 
  pivot_longer(c(frac_OM)) %>% 
  mutate(name=recode(name,
                     frac_OM="Open Software Codes")) %>% 
  ggplot(aes(x=Year,y=value, group=Source,
             color=Source, shape = Source, fill = Source)) +
  geom_point(size=2.7) + geom_line() +
  # facet_wrap(.~name)+
  scale_shape_manual(values = c(15, 16)) +
  scale_color_manual(values = my.cols) +
  scale_fill_manual(values = my.cols) +
  labs(y="Percentage") +
  theme_bw(base_size = 16) +
  # theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1))+
  scale_x_continuous(breaks= seq(min(myDF$Year),max(myDF$Year),2))

setwd(outDIR)
ggsave("Figure2.pdf",width = 10,height = 6)


## END
