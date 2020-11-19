#---------------------------------------------------------#
# 
# Project: Randomization Study
# Perform series of randomizations  
# Programmer: Augustus 
# Reviewed: Kevin W. McConeghy
# Start: 09/29/2020
# 
#--------------------------------------------------------#
source(list.files(pattern='*cfg*'))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

# remotes::install_github('kmcconeghy/jumble')
library(jumble)  
library(nbpMatching) # for pairmatching
library(Rfast) # for M distance
library(progress) # for progress bars

df_samp <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[1]))
df_samp_varlist <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[2]))

# set to testrun  
if (testrun) {
  df_samp <- df_samp %>% group_by(size) %>%
    slice(1:100)
  df_samp_varlist <- inner_join(df_samp_varlist, df_samp[, 'sample'], 'sample')
} 

#----
# 7 Methods 
#1. Simple Random
#2. 2-strata randomization
#3. Pair-matched design
#4. K-means cluster stratified, a prior covariates
#5. PC, K-means cluster stratified
#6. Re-randomization
#---

sto_runinfo <- NULL
sto_runinfo$session <- sessioninfo::session_info()
sto_runinfo$runtimes <- list()
saveRDS(sto_runinfo, here::here('prj_dbdf', dta.names$f_cpt_list[3]))

cat('Randomization ready', '\n')