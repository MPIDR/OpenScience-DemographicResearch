
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

## directories
demresDIR <- paste0(getwd(),"/data/input")
demographyDIR <- paste0(getwd(),"/data/output")
outDIR <- paste0(getwd(),"/output")

## read Demographic Research data (scraped from website) 
setwd(demresDIR)
load("DemographicResearch-Website.Rdata")
DFdr <- DF

## read Demography data (from text search) 
setwd(demographyDIR)
load("Demography-cleaned.Rdata")
DFdem <- DF.long %>% 
  group_by(Year,Journal) %>% 
  summarise(Articles=n(),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials))

DFall <- DFdr %>% 
  bind_rows(DFdem)


##----- Figure 1 plot -----

## colors
my.cols <- c("#7F3C8D", "#11A579")

## putting issues together for Demography 
DFall %>% 
  group_by(Year,Journal) %>% 
  summarise(Articles=sum(Articles),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials)) %>% 
  mutate(Year=as.integer(Year),frac_OA=OpenAccess/Articles,
         frac_OM=OpenMaterials/Articles) %>% 
  pivot_longer(c(frac_OA,frac_OM)) %>% 
  mutate(name=recode(name,frac_OA="Open Access",
                     frac_OM="Open Study Materials")) %>% 
  ggplot(aes(x=Year,y=value, group=Journal,
             color=Journal, shape = Journal, fill = Journal)) +
  geom_point(size=2) + geom_line() +
  facet_wrap(.~name)+
  scale_shape_manual(values = c(15, 16)) +
  scale_color_manual(values = my.cols) +
  scale_fill_manual(values = my.cols) +
  labs(y="Percentage",color="Journal") +
  theme_bw(base_size = 14) +
  theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1))+
  scale_x_continuous(breaks= seq(min(DFall$Year),max(DFall$Year),2))

setwd(outDIR)
ggsave("Figure1.pdf",width = 10,height = 6)


## END
