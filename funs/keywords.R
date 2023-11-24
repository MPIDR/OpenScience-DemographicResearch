
## list of keywords used to assess the availability 
## of open data and/or open software code
keywords <- c("reproducibility","replicability",
              "reproducible","r markdown",
              "code is available", "code used is available",
              "code for the analysis is available", 
              "code used for the analysis is available",
              "codes are available", "codes used are available",
              "codes for the analysis are available",
              "codes used for the analysis are available",
              "code can be obtained","codes can be obtained",
              "replication package", "replication bundle",
              "replication material",
              "replicable package", "replicable bundle",
              "replicable material","fully replicable",
              "code to replicate","codes to replicate", 
              "code to reproduce","codes to reproduce", 
              "code and data","codes and data", 
              "data and code","data and codes", 
              "code and documentation","codes and documentation",
              "material to replicate","materials to replicate", 
              "material to reproduce","materials to reproduce", 
              "stata code","stata syntax file", "stata program",
              "matlab code","matlab program",
              "r code", "r program","r script",
              "replicate the results",
              "replicate the method","replicate the methods",
              "reproduce the results",
              "reproduce the method","reproduce the methods")


## NOTES:
## 1. The word "replicable" is not included alone to avoid picking up sentences
## such as "this experience/strategy/policy is replicable in other contexts"
## 2. Data and codes are often provided in the Open Science Framework, Github or
## Zenodo repositories. However, these repositories are sometimes included in 
## the References, hence why they were not considered here

## list of keywords used to check if the availability 
## of open data and/or open software code is upon request
keywordsREQUEST <- c("on request","upon request")