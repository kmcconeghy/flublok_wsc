#---------------------------------------------------------#
# 
# Project: Randomization Study
# Perform series of randomizations  
# Programmer: 
# Start: 09/29/2020
# 
#--------------------------------------------------------#
source(list.files(pattern='*cfg*'))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

library(jumble)  

df_samp_varlist <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[4]))

df_samp_010 <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[1]))

#----
# 7 Methods 
#1. Simple Random
#2. 2-strata randomization
#3. Pair-matched design
#4. K-means cluster stratified, a prior covariates
#5. PC, K-means cluster stratified
#6. Re-randomization
#---

# Simulation Set-up 

## Parameters  
testrun <- T  # to trial simulation set to T, set F when ready 

n_rndms <- 1000L # Nu
k_starts <- 20000L # Number of start points for K-means
k_iters <- 1000L # Number of K-means iterations

if (testrun) {
  n_rndms <- n_rndms / 100
  k_starts <- k_starts / 100
  k_iters <- k_iters / 100
}

st_seed <- as.integer(ymd('2019-12-26'))
set.seed(st_seed)

rndm_methods <- c('Simple Randomizations', 
                  '2-Strata randomization',
                  'Pair-matched design', 
                  'K-means, stratified',
                  'PC K-means, stratified',
                  'Re-randomization')

cat('Starting Seed: ', st_seed, '\n')
cat('No. of random simulations performed: ', n_rndms, '\n')

sto_runinfo <- NULL
sto_runinfo$session <- sessioninfo::session_info()
sto_runinfo$runtimes <- list()

# -- Run method 1 - Simple randomization
st_time <- Sys.time()

  df_rand_010 <- df_samp_010 %>%
   mutate(assignment = map(.x = data, 
                           .f = ~rnd_simple(., .id='accpt_id')))

end_time <- Sys.time()

sto_runinfo$runtimes$simple <- end_time - st_time


## execute

## save random datasets as list  

## record time taken



# Method 2. Simple stratified randomization - Race, Size  

## Call function

## execute

## save random datasets as list  

## record time taken

# Method 3. Pair-matched Randomization - Mahalanobis Distance  

## Call function

## execute

## save random datasets as list  

## record time taken

# Method 4. K-means clustering, key variables  

## Call function

## execute

## save random datasets as list  

## record time taken

# Method 5. K-means, PCA cluster eigen-weighted  

## Call function

## execute

## save random datasets as list  

## record time taken

# Method 6. Re-randomization  

## Call function

## execute

## save random datasets as list  

## record time taken