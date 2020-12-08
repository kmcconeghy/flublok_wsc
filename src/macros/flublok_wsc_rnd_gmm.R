rnd_gmm  <- function(x, covs, .id='accpt_id') {
  
  j <- min(ceiling(nrow(x)*0.2), 40)
  
  # covariate matrix
  df_km_1 <- x %>%
    select(all_of(.id), all_of(cov_nms)) %>%
    as.data.frame(.) 
  
  d_clust <- Mclust(df_km_1[, -1], G=2:(nrow(x)/2))
  m.best <- dim(d_clust$z)[2]
  
    df_km_2 <- kmeans(x=df_km_1[, -1], 
                      centers = k_clust, 
                      nstart = nstart, 
                      iter.max = iter.max)
    
    id_list <- bind_cols(accpt_id  = df_km_1[.id], 
                        strata = df_km_2$cluster)
    
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  
  return(rnd_rtrn)
}