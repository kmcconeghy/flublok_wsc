#x <- as.data.frame(df_samp$data[1])

rnd_pcakmns <- function(x, .id='accpt_id') {

## Principal components  

  ### Scale covariates  
  df_prcomp <- x %>%
    select(-c(city, state, zip5, county)) # drop variables don't need

  ### Compute PC  
  pca_result <- prcomp(df_prcomp[,-which(names(df_prcomp) %in% "accpt_id")], scale = F)  

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
  
  # repeat {
    df_pca_2 <- kmeans(x=df_pca_1[, -1], 
                      centers = k_clust, 
                      nstart = 5, 
                      iter.max = 100)
    
    id_list <- bind_cols(accpt_id  = df_pca_1[.id], 
                         strata = df_pca_2$cluster)
    
  #   if (min(table(id_list$strata)) <= j) {
  #     break
  #   }
  #   k_clust <- k_clust + 1L
  # }
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)

  
  return(rnd_rtrn)
  
}

# repeat {
  # df_km_2 <- df_pca_1 %>%
  #   select(-fac_id) %>%
  #   as.matrix(.) %>%
  #   kmeans(x=., centers = k_clust, nstart = k_starts, iter.max=k_iters)
  # 
  # df_m5 <- bind_cols(fac_id  = df_pca_1$fac_id,
  #                    strata = df_km_2$cluster) %>%
  #   distinct(.)
  
#   if (min(table(df_m5$strata)) <= 2) {
#     break
#   }
#   k_clust <- k_clust + 1L
# }
