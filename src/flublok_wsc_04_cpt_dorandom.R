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
testrun <- F  # to trial simulation set to T, set F when ready 

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
                  'rnd_strat',
                  'rnd_paired',
                  'rnd_kmns',
                  'rnd_kmpca',
                  'rnd_rerand')

cat('Starting Seed: ', st_seed, '\n')

df_rand <- df_samp %>%
  select(-data) %>%
  ungroup

rndm_tab <- tibble(method = rndm_methods) 
rndm_tab[df_samp_varlist$fac_adj[[1]]] <- NA_real_

df_rand$res <- replicate(nrow(df_rand), rndm_tab, simplify = F)

sto_runinfo <- NULL
sto_runinfo$session <- sessioninfo::session_info()
sto_runinfo$runtimes <- list()

# -- Run method 1 - Simple randomization
st_time <- Sys.time()

  ## execute - randomization  
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(df_samp$data))

  df_rand$assign <- map(.x = df_samp$data, 
                       .f = ~{
                         pb$tick()
                         rnd_simple(., .id='accpt_id')}) %>%
    map(.f = ~rename(., accpt_id = id))
  
  ## compute - mean differences  
  res_iter = pmap(list(df_samp$data, 
                       df_rand$assign,
                       df_samp_varlist$fac_adj),
                  .f = ~cpt_diff(..1, ..2, ..3)
  )
  
  ## add results to data.frame  
  df_rand$res <- map2(df_rand$res, # inner apply 
                      res_iter,
                      .f = function(x, y, rw=1) {
                        x[rw, 2:ncol(x)] <- y
                        return(x)
                      })
  
  end_time <- Sys.time()
  
  ## record time taken
  sto_runinfo$runtimes$simple <- end_time - st_time
  
# Method 2. Simple stratified randomization - Race, Size  
st_time <- Sys.time()

  ## execute - randomization
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(df_samp$data))
  df_rand$assign <- map2(.x = df_samp$data, 
                         .y = df_samp_varlist$strata,
                         .f = ~{
                           pb$tick()
                           rnd_strat(.x, .y, .id='accpt_id')})
  
  ## compute - mean differences  
  res_iter <- pmap(list(df_samp$data, 
                            df_rand$assign,
                            df_samp_varlist$fac_adj),
                       .f = ~cpt_diff(..1, ..2, ..3))

  ## add results to data.frame  
  df_rand$res <- map2(df_rand$res, # inner apply 
                      res_iter,
                      .f = function(x, y, rw=2) {
                        x[rw, 2:ncol(x)] <- y
                        return(x)
                      })
  
end_time <- Sys.time()

## record time taken
sto_runinfo$runtimes$strata <- end_time - st_time

# Method 3. Pair-matched Randomization - Mahalanobis Distance  
  st_time <- Sys.time()
  
  ## execute - randomization  
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(df_samp$data))
  
  df_rand$assign <- map2(.x = df_samp$data, 
                         .y = df_samp_varlist$data,
                         .f = ~{
                           pb$tick()
                           rnd_pairmatch(.x, .y, .id='accpt_id')})
  
  ## compute - mean differences  
  res_iter <- pmap(list(df_samp$data, 
                        df_rand$assign,
                        df_samp_varlist$fac_adj),
                   .f = ~cpt_diff(..1, ..2, ..3))
  
  ## add results to data.frame  
  df_rand$res <- map2(df_rand$res, # inner apply 
                      res_iter,
                      .f = function(x, y, rw=3) {
                        x[rw, 2:ncol(x)] <- y
                        return(x)
                      })
  
  end_time <- Sys.time()
  
  ## record time taken
  sto_runinfo$runtimes$pairmatch <- end_time - st_time

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
  st_time <- Sys.time()
  
  ## execute - randomization  
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(df_samp$data))
  
  df_rand$assign <- map2(.x = df_samp$data, 
                         .y = df_samp_varlist$data,
                         .f = ~{
                           pb$tick()
                           rnd_rerand(.x, .y, .id='accpt_id')
                           })
  ## compute - mean differences  
  res_iter <- pmap(list(df_samp$data, 
                        df_rand$assign,
                        df_samp_varlist$fac_adj),
                   .f = ~cpt_diff(..1, ..2, ..3))
  
  ## add results to data.frame  
  df_rand$res <- map2(df_rand$res,  
                      res_iter,
                      .f = function(x, y, rw=6) {
                        x[rw, 2:ncol(x)] <- y
                        return(x)
                      })
  
  end_time <- Sys.time()
  
  ## record time taken
  sto_runinfo$runtimes$rerand <- end_time - st_time

# Save files  

## Runtimes  
saveRDS(sto_runinfo, here::here('prj_dbdf', dta.names$f_cpt_list[3]))

## save randomizations/mean differences  
saveRDS(df_rand, here::here('prj_dbdf', dta.names$f_cpt_list[4]))
