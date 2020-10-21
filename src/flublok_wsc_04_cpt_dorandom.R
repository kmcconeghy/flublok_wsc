
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

ltcfocus <- readRDS(here::here('prj_dbdf', dta.names$f_munge_list[1]))



#----
# 7 Methods 
#
#1. Simple Random
#2. 2-strata randomization
#3. Pair-matched design
#4. K-means cluster stratified, a prior covariates
#5. PC, K-means cluster stratified
#6. Re-randomization
#---

# Simulation Set-up 

## Parameters  

if (F) {
  n_rndms <- 1000L
  n_permutes <- 10L
  k_starts <- 100L
  k_iters <- 100L
} 

if (T) {
  n_rndms <- 10000L
  n_permutes <- 1000L
  k_starts <- 200000L
  k_iters <- 100L
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
cat('No. of permutations performed: ', n_permutes, '\n')


# -- Run method 1 - Simple randomization

## Call function

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