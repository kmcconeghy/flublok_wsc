rnd_kmns  <- function(x, covs, .id='accpt_id', nstart=1000, iter.max=20) {
  
  j <- min(ceiling(nrow(x)*0.2), 40)
  
  cov_nms <- unlist(covs)
  
  # covariate matrix
  df_km_1 <- x %>%
    select(all_of(.id), all_of(cov_nms)) %>%
    as.data.frame(.) 
  
  k_clust <- 2
  
  repeat {
    df_km_2 <- kmeans(x=df_km_1[, -1], 
                      centers = k_clust, 
                      nstart = nstart, 
                      iter.max = iter.max)
    
    id_list <- bind_cols(accpt_id  = df_km_1[.id], 
                        strata = df_km_2$cluster)
    
    if (min(table(id_list$strata)) <= j) {
      break
    }
    k_clust <- k_clust + 1L
  }
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  
  return(rnd_rtrn)
}