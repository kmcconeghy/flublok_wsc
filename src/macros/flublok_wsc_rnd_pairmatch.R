test <- function(x) {
# Create Pair-Matches  

library(nbpMatching)

# create a covariate matrix
df_mtch <- df_trial %>%
  select(fac_id, fac_key_vars) %>%
  as.data.frame(.) %>%
  distinct(.)

# create distances
df_dist <- gendistance(df_mtch, idcol=1)

# create distancematrix object
df_mdm <- distancematrix(df_dist)

# create matches
df_mtch_2 <- nonbimatch(df_mdm)

# review quality of matches
df_qom <- qom(df_dist$cov, df_mtch_2$matches)

df_mtch_3 <- df_mtch_2$matches %>%
  select(Group1.ID, Group2.ID) %>%
  mutate(class = row_number()) %>%
  tidyr::pivot_longer(, cols = c('Group1.ID', 'Group2.ID'),
                      values_to = 'fac_id') %>%
  select(fac_id, class) %>%
  arrange(fac_id) %>%
  group_by(fac_id) %>%
  slice(1) %>%
  ungroup(.)

## Mahalanobis distane
df_m3 <- df_trial %>%
  select(fac_id) %>%
  inner_join(., df_mtch_3) %>%
  distinct(.)

n_distinct(df_mtch_3$fac_id)

st_run <- Sys.time()

#Matrix of values  
m3_res <- list()

m3_res$delta <- matrix(NA, nrow = n_rndms, ncol = length(chk_id_vars))  
m3_res$stdev <- df_trial[, chk_id_vars] %>% 
  map_dfr(., sd, na.rm=T) %>%
  t(.)

do_it <- function(x)  {
  sim_iter <- rnd_str(df_m3, class, fac_id) %>%
    inner_join(df_trial[, c('fac_id', chk_id_vars)], ., by=c('fac_id')) %>%
    select(group, chk_id_vars)
  
  delta <- do_rand(sim_iter)
  return(delta)
}

m3_res$delta[1:n_rndms, ] <- t(sapply(1L:n_rndms, do_it))
m3_res$smd <- t(apply(m3_res$delta, 1, function(x) x / t(m3_res$stdev)))

st_end <- Sys.time()
cat(paste0(n_rndms), 'Randomizations for M3. Pair-Matched randomization, Key Variables \n ')
st_end - st_run
}