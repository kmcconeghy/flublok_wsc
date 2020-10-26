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

# remotes::install_github('kmcconeghy/jumble')
library(jumble)  

df_samp <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[1]))
df_samp_varlist <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[2]))

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

if (testrun) {
  df_samp <- df_samp[1:10, ]
  df_samp_varlist <- df_samp_varlist[1:10, ]
}

k_starts <- 20000L # Number of start points for K-means
k_iters <- 1000L # Number of K-means iterations

if (testrun) {
  k_starts <- k_starts / 100
  k_iters <- k_iters / 100
}

st_seed <- as.integer(ymd('2020-10-24'))
set.seed(st_seed)

rndm_methods <- c('rnd_simple',
                  'rnd_2strat',
                  'rnd_paired',
                  'rnd_kmns',
                  'rnd_kmpca',
                  'rnd_rerand')

cat('Starting Seed: ', st_seed, '\n')

df_rand <- df_samp %>%
  select(-data)

df_rand$res <- replicate(nrow(df_rand), tibble(method = rndm_methods), simplify = F)

sto_runinfo <- NULL
sto_runinfo$session <- sessioninfo::session_info()
sto_runinfo$runtimes <- list()

df_res_tab <- tibble()
# -- Run method 1 - Simple randomization
st_time <- Sys.time()

  ## randomization
  df_rand$assign<- map(.x = df_samp$data, 
                       .f = ~rnd_simple(., .id='accpt_id'))
  
  ## compute - mean differences  
  res_iter = pmap(list(df_samp$data, 
                       df_rand$assign,
                       df_samp_varlist$fac_adj),
                  .f = ~cpt_diff(..1, ..2, ..3)
  )
  
  df_rand$res <- map(df_rand$res, function(x) 
    bind_cols(x, res_iter))
  end_time <- Sys.time()
  
  ## record time taken
  sto_runinfo$runtimes$simple <- end_time - st_time
  
  ## save  
  saveRDS(df_rand, here::here('prj_dbdf', dta.names$f_rnd_res[1]))

# Method 2. Simple stratified randomization - Race, Size  
st_time <- Sys.time()

## execute - randomization
df_rand <- df_samp %>%
  mutate(assignment = map(.x = data, 
                          .f = ~rnd_2strat(., .id='accpt_id')))

## compute - mean differences  
df_rand$delta = pmap(list(df_rand$data, 
                          df_rand$assignment,
                          df_samp_varlist$fac_adj),
                     .f = ~cpt_diff(..1, ..2, ..3))

end_time <- Sys.time()

## record time taken
sto_runinfo$runtimes$simple <- end_time - st_time

## save  
saveRDS(df_rand, here::here('prj_dbdf', dta.names$f_rnd_res[1]))

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

# Save files  
## Runtimes  
saveRDS(sto_runinfo, here::here('prj_dbdf', dta.names$f_cpt_list[3]))
## save randomizations/mean differences  
saveRDS(sto_runinfo, here::here('prj_dbdf', dta.names$f_cpt_list[4]))
