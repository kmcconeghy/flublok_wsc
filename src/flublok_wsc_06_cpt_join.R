
# load original trial lists
df_samp <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[1]))
df_samp_varlist <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[2]))

# labels
rndm_methods <- c('rnd_simple',
                  'rnd_strat',
                  'rnd_pair',
                  'rnd_kmns',
                  'rnd_kmpca',
                  'rnd_rerand')

# length of each trial
sizes <- df_samp %>%
  select(size) %>%
  unlist %>%
  unique(.)


# each randomization result
d_res_simp <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[1])) 
d_res_strat <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[2])) 
d_res_pair <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[3])) 
d_res_kmns <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[4])) 
d_res_rerand <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[6])) 

#as one list
d_res <- list(simple = d_res_simp,
              strat = d_res_strat,
              pair = d_res_pair,
              kmns = d_res_kmns,
              rerand = d_res_rerand)

# Goal to reorganize file and prepare it for reporting  
df_var_cats <- df_samp %>%
  select(sample, size) 

df_var_cats$size <- unlist(df_var_cats$size)

## Add varlists  

#total vars the same for all, so take first list item
totvars <- df_samp_varlist$fac_adj[[1]]

#balancing vars
df_var_cats$bal_vars <- map(df_samp_varlist$data, unlist)
df_var_cats$str_vars <- df_samp_varlist$strata
df_var_cats$non_vars <- map(df_samp_varlist$data,
                            function(x, ...) totvars[!(totvars %in% unlist(x))])
# Four measures computed
## E[SMD] for all variables  
## E[SMD] for adj variables 
## E[SMD] for strata  
## E[SMD] for unadjusted

### All variables  
d_res_cats <- map(d_res, ~cpt_vargrp_means(., df_var_cats))

d_res_cats_2 <- map(d_res_cats, 
                    function(x) {
                      x %>%
                        group_by(size) %>%
                        summarize_at(vars(-sample), ~sd(., na.rm=T))
                    }) %>%
  bind_rows(.id = 'method') %>%
  mutate(method = factor(method, 
                         levels = c('simple',
                                    'strat',
                                    'pair',
                                    'kmns',
                                    'kmpca',
                                    'rerand'),
                         labels = c('Simple',
                                    'Categorical strata',
                                    'Pair-matched',
                                    'K-means stratified',
                                    'PCA K-means stratified',
                                    'Re-randomization')))

saveRDS(d_res_cats_2, here::here('prj_dbdf', dta.names$f_cpt_list[6]))
