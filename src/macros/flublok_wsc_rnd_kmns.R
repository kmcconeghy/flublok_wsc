rnd_kmns  <- function(x, covs, .id='accpt_id') {
  
  cov_nms <- unlist(covs)
  
  # covariate matrix
  df_km_1 <- x %>%
    select(all_of(.id), all_of(cov_nms)) %>%
    as.data.frame(.) 
  
  #repeat {
    df_km_2 <- kmeans(x=df_km_1[, -1], centers = 2, nstart = 10, iter.max=5)
    
    id_list <- bind_cols(accpt_id  = df_km_1[.id], 
                        strata = df_km_2$cluster)
    
  #  if (min(table(df_m4$strata)) <= 2) {
  #    break
  #  }
  #  k_clust <- k_clust + 1L
  #}
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  
  return(rnd_rtrn)
}