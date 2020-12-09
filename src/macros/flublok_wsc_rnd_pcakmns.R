rnd_pcakmns <- function(x, .id='accpt_id') {

  j <- min(ceiling(nrow(x)*0.2), 30)
  
  ## Principal components  
  ### Scale covariates  
  df_prcomp <- x %>%
    select(-c(city, state, zip5, county)) # drop variables don't need

  ### Compute PC  
  pca_result <- prcomp(df_prcomp %>% select(-accpt_id), scale = F)  
  
  e_values <- pca_result$sdev[pca_result$sdev>1] #Filter by linear independence

  # First for principal components
  df_decomp <- data.frame(pca_result$x) %>%
    bind_cols(., df_prcomp) %>%
    select('accpt_id', paste0('PC', 1:length(e_values), sep=''))

  # weight by eigenvectors  
  df_pca_weighted <- bind_cols(fac_id=df_decomp$fac_id, 
                               x2=sweep(df_decomp[, 2:ncol(df_decomp)], 2, e_values, "*")) 

  #Matrix of values  
  df_pca_1 <- bind_cols(accpt_id = df_decomp[,"accpt_id"], df_pca_weighted) 
  
  k_clust <- 2
  
   repeat {
    df_pca_2 <- kmeans(x=df_pca_1[, -1], 
                      centers = k_clust, 
                      nstart = 100, 
                      iter.max = 100)
    
    id_list <- bind_cols(accpt_id  = df_pca_1[.id], 
                         strata = df_pca_2$cluster)
    
    if (min(table(id_list$strata)) <= j) {
      break
    }
    k_clust <- k_clust + 1L
}
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  return(rnd_rtrn)
  
}