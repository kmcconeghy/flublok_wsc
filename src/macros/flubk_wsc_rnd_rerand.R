
### -- Convert to function


## Find Acceptable Cut-Off Value  
st_run <- Sys.time()

df_rerand <- df_cov %>%
  select(-dc_hosp_any) %>%
  distinct(.) %>%
  mutate_at(vars(-fac_id), scale)

# Find M-distance cutoff empirically   
do_it <- function(x) {
  
  sim_iter <- rnd_allot(df_fac) %>%
    as.data.frame(.) %>%
    inner_join(df_rerand, ., by=c('fac_id'='id')) %>%
    select(-fac_id) %>%
    as.data.frame(.)
  
  mdis_iter <- mahalanobis(colMeans(sim_iter[sim_iter$group=='a', fac_key_vars], na.rm=T), 
                           colMeans(sim_iter[sim_iter$group=='b', fac_key_vars], na.rm=T), 
                           cov= cov(sim_iter[, fac_key_vars])) * 
    ((nrow(sim_iter[sim_iter$group=='a', ]) * nrow(sim_iter[sim_iter$group=='b', ])) / (nrow(sim_iter)))
}

m6_chk_mdis <- sapply(1:n_rndms, do_it)

m6_cutoff <- quantile(m6_chk_mdis, 0.001)  

st_end <- Sys.time()

cat('Method 6. Re-randomization cut-off', round(m6_cutoff, 3), ' \n')
st_end - st_run

## Perform Re-randomizations  

st_run <- Sys.time()

m6_res <- list()

m6_res$delta <- matrix(NA, nrow = n_re_rndms, ncol = length(chk_id_vars))  
m6_res$stdev <- df_trial[, chk_id_vars] %>% 
  map_dfr(., sd, na.rm=T) %>%
  t(.)

df_m6 <- df_trial %>%
  select(fac_id, fac_key_vars) %>%
  distinct()   

do_it <- function(x)  {
  repeat { # search for good rerand
    
    chk_rand <- rnd_allot(df_fac) %>%
      inner_join(df_m6[, c('fac_id', fac_key_vars)], ., by=c('fac_id'='id')) 
    
    mdis_iter <- Rfast::mahala(colMeans(chk_rand[chk_rand$group=='a', fac_key_vars], na.rm=T), 
                               colMeans(chk_rand[chk_rand$group=='b', fac_key_vars], na.rm=T), 
                               sigma= cov(chk_rand[, fac_key_vars])) * 
      ((nrow(chk_rand[chk_rand$group=='a', ]) * nrow(chk_rand[chk_rand$group=='b', ])) / (nrow(chk_rand)))
    
    if(mdis_iter<m6_cutoff) {
      break
    }
  }
  
  sim_iter <- inner_join(df_trial[, c('fac_id', chk_id_vars)], 
                         chk_rand[, c('fac_id', 'group')], 
                         by=c('fac_id')) %>%
    select(group, chk_id_vars)
  
  delta <- do_rand(sim_iter)
  return(delta)
}

m6_res$delta[1:n_rndms, ] <- t(sapply(1L:n_rndms, do_it))
m6_res$smd <- t(apply(m6_res$delta, 1, function(x) x / t(m6_res$stdev)))

st_end <- Sys.time()

cat(paste0(n_rndms), 'Method 6. Re-randomizations, \n')
st_end - st_run
