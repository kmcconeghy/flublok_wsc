test <- function(x) {
### -- Convert to function

#Matrix of values  
df_km_1 <- df_trial %>%
  select(fac_id, fac_key_vars) %>%
  distinct(.) %>%
  mutate_at(vars(fac_key_vars), scale)

repeat {
  df_km_2 <- df_km_1 %>%
    select(-fac_id) %>%
    as.matrix(.) %>%
    kmeans(x=., centers = k_clust, nstart = k_starts, iter.max=k_iters)
  
  df_m4 <- bind_cols(fac_id  = df_km_1$fac_id,
                     strata = df_km_2$cluster) %>%
    distinct(.)
  
  if (min(table(df_m4$strata)) <= 2) {
    break
  }
  k_clust <- k_clust + 1L
}

#Matrix of values  
m4_res <- list()

m4_res$delta <- matrix(NA, nrow = n_rndms, ncol = length(chk_id_vars))  
m4_res$stdev <- df_trial[, chk_id_vars] %>% 
  map_dfr(., sd, na.rm=T) %>%
  t(.)

do_it <- function(x)  {
  
  sim_iter <- rnd_str(df_m4, strata, fac_id) %>%
    inner_join(df_trial[, c('fac_id', chk_id_vars)], ., by=c('fac_id')) %>%
    select(group, chk_id_vars)
  
  delta <- do_rand(sim_iter)
  return(delta)
}

m4_res$delta[1:n_rndms, ] <- t(sapply(1:n_rndms, do_it))
m4_res$smd <-t(apply(m4_res$delta, 1, function(x) x / t(m4_res$stdev)))

st_end <- Sys.time()

cat(paste0(n_rndms), 'Method 4. K-means clustering on key variables \n')
st_end - st_run
}