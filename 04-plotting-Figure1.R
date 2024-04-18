
## --------------------------------------------------------- ##
##
##  FILE 04: producing Figure 1 of the manuscript
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
textsearchDIR <- paste0(getwd(),"/data/output")
demresDIR <- paste0(getwd(),"/data/input")
outDIR <- paste0(getwd(),"/output")

## read Demographic Research data (scraped from website) 
setwd(demresDIR)
load("DemographicResearch-Website.Rdata")
DFdr <- DF %>% 
  mutate(Journal = "Dem Res (website)") 

## read other journals' data (from text search, after cleaning) 
setwd(textsearchDIR)
load("03-combined-cleaned.Rdata")

## combining data frames
DFsearch <- DFall %>% 
  group_by(Year,Journal) %>% 
  summarise(Articles=n(),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials)) %>% 
  mutate(Journal=case_when(
    Journal=="Dem Res"~"Dem Res (text search)",
    TRUE ~ Journal
  )) 

DFfin <- DFdr %>% 
  bind_rows(DFsearch)


##----- Figure 1 plot -----

## colors 
my.cols <- c( "hotpink1", "#7F3C8D", "#11A579", "#3969AC", "#E58606")

## putting issues together for Demography 
DFfin %>% 
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
  scale_shape_manual(values = c(15, 17, 16, 15, 23)) +
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


## -- EXTRA: comparing full results with those shared in this repo

## read Dem Res and Demography data (from text search of this repo)
setwd(textsearchDIR)
load("02-Demography-cleaned.Rdata")
DFrepo <- DF.long 
load("02-DemRes-cleaned.Rdata")
DFrepo <- DFrepo %>% 
  bind_rows(DF.long) %>% 
  mutate(source="OSF repo")

## results from full analysis (also non sharable articles)
DFsearch <- DFall %>% 
  mutate(source="all journals") %>% 
  filter(Journal=="Demography" | Journal=="Dem Res")

## combining dataframes
DFcomb <- DFrepo %>% 
  bind_rows(DFsearch)

## comparing
DFcomb %>% 
  filter((Journal=="Demography" & Year > 2020) | Journal=="Dem Res") %>% 
  group_by(Year,Journal,source) %>% 
  summarise(Articles=n(),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials)) %>% print(n=100)

DFcomb %>% group_by(Year,Journal,source) %>% 
  summarise(Articles=n(),OpenAccess=sum(OpenAccess),
            OpenMaterials=sum(OpenMaterials)) %>% 
  mutate(Year=as.integer(Year),frac_OA=OpenAccess/Articles,
         frac_OM=OpenMaterials/Articles) %>% 
  pivot_longer(c(frac_OA,frac_OM)) %>% 
  mutate(name=recode(name,frac_OA="Open Access",
                     frac_OM="Open Study Materials")) %>% 
  ggplot(aes(x=Year,y=value, group=Journal,
             color=Journal, shape = source, fill = Journal)) +
  geom_point(size=2) + geom_line() +
  facet_wrap(.~name)+
  scale_shape_manual(values = c(1, 4)) +
  scale_color_manual(values = my.cols[1:2]) +
  scale_fill_manual(values = my.cols[1:2]) +
  labs(y="Percentage",color="Journal") +
  theme_bw(base_size = 14) +
  theme(legend.position="bottom")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1))+
  scale_x_continuous(breaks= seq(min(DFall$Year),max(DFall$Year),2))



## END
