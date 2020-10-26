# Method 1. Simple Randomization  
## Random Assignment - Simple  
rnd_2strat  <- function(x, strata, .id) {
  
  strat_1 <- strata[1]
  strat_2 <- strata[2]
  id_list <- x %>%
    distinct(fac_id, fac_black, fac_ls) %>%
    mutate(cat_aa = ntile(fac_black, 5),
           cat_fs = ntile(fac_ls, 5),
           strata = interaction(cat_aa, cat_fs)) %>%
    distinct(fac_id, cat_aa, cat_fs, strata)   
    select(all_of(.id)) %>%
    distinct(.) %>%
    pull(.)
  
  rnd_rtrn <- jumble::rnd_allot(id_list)
  
  return(rnd_rtrn)
}

test  <- function(x) {
## 2 Stratum  
df_m2 <- df_trial %>%
  distinct(fac_id, fac_black, fac_ls) %>%
  mutate(cat_aa = ntile(fac_black, 5),
         cat_fs = ntile(fac_ls, 5),
         strata = interaction(cat_aa, cat_fs)) %>%
  distinct(fac_id, cat_aa, cat_fs, strata)   

#Matrix of values  
m2_res <- list()

m2_res$delta <- matrix(NA, nrow = n_rndms, ncol = length(chk_id_vars))  
m2_res$stdev <- df_trial[, chk_id_vars] %>% 
  map_dfr(., sd, na.rm=T) %>%
  t(.)

do_it <- function(x)  {
  sim_iter <- rnd_str(df_m2, strata, fac_id) %>%
    inner_join(df_trial[, c('fac_id', chk_id_vars)], ., by=c('fac_id')) %>%
    select(group, chk_id_vars)
  
  delta <- do_rand(sim_iter)
  return(delta)
}

m2_res$delta[1:n_rndms, ] <- t(sapply(1L:n_rndms, do_it))
m2_res$smd <- t(apply(m2_res$delta, 1, function(x) x / t(m2_res$stdev)))

st_end <- Sys.time()
cat(paste0(n_rndms), 'Randomizations for M2. Stratified randomization, facility %AA and size quintiles \n ')
st_end - st_run
}