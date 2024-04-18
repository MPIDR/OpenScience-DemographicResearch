

## adjectives usually associated with reproducibility
var1 <- c("replication","reproducible","replicable","replicability",
          "reproducibility","analysis","stata", "r", "matlab", "sas", "source",
          "excel")

## nouns (object of reproducibility)
var2 <- c("code","material","bundle","routine", "file", "program",
          "result","example","package","workbook")
var3 <- paste0(var2,"s")  ## plural

## verbs
var4 <- c("are reproducible","are replicable","is reproducible","is replicable",
          "are fully reproducible","are fully replicable","is fully reproducible","is fully replicable",
          "to reproduce","to replicate","to fully reproduce","to fully replicate")

## availability of codes, programs, routines
var5 <- c("available","open", "open-access", "open access","online",
          "provided","analytic")
var6 <- c("code","routine", "program")
var7 <- paste0(var6,"s")  ## plural
var8 <- c("is available","are available","available")

## replication/reproduction of results/analyses
var9 <- c("reproduce","replicate")
var10 <- c("result","the result", "all result", "all authors' result", 
           "all the authors' result","analysis",
           "the analysis","all analysis", "all authors' analysis", 
           "all the authors' analysis","analyses","the analyses",
           "all analyses","all authors' analyses", 
           "all the authors' analyses")
var11 <- paste0(var10,"s")  ## plural

## combining adjectives with nouns
key1 <- expand_grid(var1,var2) %>% 
  mutate(key=paste(var1,var2)) %>%
  filter(key!="analysis result") %>% 
  select(key) %>% pull()
key2 <- expand_grid(var1,var3) %>% 
  mutate(key=paste(var1,var3)) %>% 
  filter(key!="analysis results") %>% 
  select(key) %>% pull()

## combining nouns with verbs
key3 <- expand_grid(var2,var4) %>% 
  mutate(key=paste(var2,var4)) %>% 
  select(key) %>% pull()
key4 <- expand_grid(var3,var4) %>% 
  mutate(key=paste(var3,var4)) %>% 
  select(key) %>% pull()

## availability combinations
key5 <- expand_grid(var5,var6) %>% 
  mutate(key=paste(var5,var6)) %>% 
  select(key) %>% pull()
key6 <- expand_grid(var5,var7) %>% 
  mutate(key=paste(var5,var7)) %>% 
  select(key) %>% pull()
key7 <- expand_grid(var6,var8) %>% 
  mutate(key=paste(var6,var8)) %>% 
  select(key) %>% pull()
key8 <- expand_grid(var7,var8) %>% 
  mutate(key=paste(var7,var8)) %>% 
  select(key) %>% pull()

## reproduce combinations
key9 <- expand_grid(var9,var10) %>% 
  mutate(key=paste(var9,var10)) %>% 
  select(key) %>% pull()
key10 <- expand_grid(var9,var11) %>% 
  mutate(key=paste(var9,var11)) %>% 
  select(key) %>% pull()

## standalone repositories names for reproducibility purposes
key11 <- c("github","zenodo","osf","open science framework","figshare", "dataverse",
           "github.com","osf.io")

## document types and interactive applications that are typically reproducible
key12 <- c("r markdown","shinyapp", "shiny app", "shinyapps.io")

## including the keyowrd "routines", which is sometimes used in isolation
## (e.g. routines for estimating the model)
key13 <- c("routines")

## final list of keywords used to assess the availability 
## of open software code
keywords <- c(key1,key2,key3,key4,key5,key6,key7,key8,key9,key10,
              key11,key12,key13)

## NOTES:
## 1. The word "reproducibility" and "replicability" are not included alone 
## to avoid picking up general sentences that contain these terms

## list of keywords used to check if the availability 
## of open data and/or open software code is upon request
keywordsREQUEST <- c("on request","upon request",
                     "on reasonable request","upon reasonable request",
                     "on (reasonable) request","upon (reasonable) request",
                     "available from the author",
                     "available from the authors")


## keep only keywords and keywordsREQUEST objects
rm(list=setdiff(ls(), c("keywords", "keywordsREQUEST")))

