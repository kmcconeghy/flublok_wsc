test  <- function(x, .id) {

# Method 1. Simple Randomization  
## Random Assignment - Simple  

id_list <- x %>%
  select(id) %>%
  distinct(.) %>%
  pull(.)

#Matrix of values  
m1_res <- list()

m1_res$delta <- matrix(NA, nrow = n_rndms, ncol = length(chk_id_vars)) %>%
  as.data.frame(.)

m1_res$stdev <- df_trial[, chk_id_vars] %>% 
  map_dfr(., sd, na.rm=T) %>%
  t(.)

do_it <- function(x)  {
  sim_iter <- rnd_allot(df_m1) %>%
    as.data.frame(.) %>%
    inner_join(df_trial[, c('fac_id', chk_id_vars)], ., by=c('fac_id'='id')) %>%
    select(-fac_id)
  
  delta <- do_rand(sim_iter)
  return(delta)
}

m1_res$delta[1L:n_rndms, ] <- t(sapply(1L:n_rndms, do_it))
m1_res$smd <- t(apply(m1_res$delta[, 1:length(chk_id_vars)], 1, function(x) x / t(m1_res$stdev)))

st_end <- Sys.time()

cat(paste0(n_rndms), 'Randomizations for M1. Simple Random Assignment \n ')
st_end - st_run
}