# Method 3. Kmeans
# x=as.data.frame(df_samp$data[1])

rnd_kmns  <- function(x, .id='accpt_id') {
  
  #Matrix of values  
  df_km_1 <- x %>%
    select(all_of(.id)) %>%
    distinct(.) %>%
    pull(.)
  
  #repeat {
    df_km_2 <- x %>%
      select(-.id,-city,-state,-county) %>%
      #as.matrix(.) %>%
      kmeans(x=., centers = 2, nstart = 5, iter.max=5)
    
    df_m4 <- bind_cols(fac_id  = df_km_1,
                       strata = df_km_2$cluster) %>%
      distinct(.)
    
  #  if (min(table(df_m4$strata)) <= 2) {
  #    break
  #  }
  #  k_clust <- k_clust + 1L
  #}
  
  rnd_rtrn <- jumble::rnd_str(df_m4, strata,id='fac_id')
  
  return(rnd_rtrn)
}

rnd_kmns(x)
