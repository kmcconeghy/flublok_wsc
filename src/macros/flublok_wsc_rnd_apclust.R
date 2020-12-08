rnd_apclust  <- function(x, covs, .id='accpt_id') {
  cov_nms <- unlist(covs)
  
  # covariate matrix
  df_km_1 <- x %>%
    select(all_of(.id), all_of(cov_nms)) %>%
    as.data.frame(.) 
  
  d_clust <- apcluster(negDistMat, df_km_1[, -1])
  
  cluster <- d_clust@clusters %>% 
    lapply(., unname) %>% 
    bind_rows(., .id='cluster') %>% 
    pivot_longer(cols = everything(), 
                 names_to = 'cluster', values_to = 'index') %>%
    arrange(index) %>%
    mutate(cluster = str_extract(cluster, '[[:digit:]]+')) %>%
    pull(cluster)
  
  id_list <- bind_cols(accpt_id  = df_km_1[.id], 
                       strata = cluster)
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  
  return(rnd_rtrn)
}