
### -- Convert to function



## Principal components  

### Scale covariates  
df_prcomp <- df_cov %>%
  select(-dc_hosp_any) %>%
  distinct(.) 

df_prcomp_scl <- df_prcomp %>%
  select(-fac_id) %>%
  map_dfr(., scale) 


### Compute PC  
pca_result <- prcomp(df_prcomp_scl, scale = TRUE)  

summary(pca_result)

e_values <- pca_result$sdev[pca_result$sdev>1] 

# First for principal components
df_decomp <- data.frame(pca_result$x) %>%
  bind_cols(., df_prcomp) %>%
  select('fac_id', paste0('PC', 1:length(e_values), sep='')) %>%
  mutate_at(vars(-'fac_id'), scale)


df_pca_weighted <- bind_cols(fac_id=df_decomp$fac_id, 
                             x2=sweep(df_decomp[, 2:21], 2, e_values, "*")) 

#Matrix of values  
df_pca_1 <- df_pca_weighted 

repeat {
  df_km_2 <- df_pca_1 %>%
    select(-fac_id) %>%
    as.matrix(.) %>%
    kmeans(x=., centers = k_clust, nstart = k_starts, iter.max=k_iters)
  
  df_m5 <- bind_cols(fac_id  = df_pca_1$fac_id,
                     strata = df_km_2$cluster) %>%
    distinct(.)
  
  if (min(table(df_m5$strata)) <= 2) {
    break
  }
  k_clust <- k_clust + 1L
}

#Matrix of values  
m5_res <- list()

m5_res$delta <- matrix(NA, nrow = n_rndms, ncol = length(chk_id_vars))  
m5_res$stdev <- df_trial[, chk_id_vars] %>% 
  map_dfr(., sd, na.rm=T) %>%
  t(.)

do_it <- function(x)  {
  
  sim_iter <- rnd_str(df_m5, strata, fac_id) %>%
    inner_join(df_trial[, c('fac_id', chk_id_vars)], ., by=c('fac_id')) %>%
    select(group, chk_id_vars)
  
  delta <- do_rand(sim_iter)
  return(delta)
}

m5_res$delta[1:n_rndms, ] <- t(sapply(1:n_rndms, do_it))
m5_res$smd <-t(apply(m5_res$delta, 1, function(x) x / t(m5_res$stdev)))

st_end <- Sys.time()

cat(paste0(n_rndms), 'Method 5. K-means on PCA \n')
st_end - st_run
