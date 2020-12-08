choose_pa <- function(df_rerand, fac_list, cov_nms, Pa=0.001) {
  
  #reset acceptance cutoff to unacceptable
  mdis_iter <- NULL
  
  mdis_chk <- replicate(100, {
    chk_rand <- rnd_allot(fac_list) %>%
      inner_join(df_rerand, ., by=c('accpt_id' = 'id')) 
    
    c_mns_a <- colMeans(chk_rand[chk_rand$group=='a', cov_nms], na.rm=T)
    c_mns_b <- colMeans(chk_rand[chk_rand$group=='b', cov_nms], na.rm=T)
    c_cov <- cov(chk_rand[, cov_nms])
    c_tokeep <- which(c_mns_a != c_mns_b) #cant compute if no difference in means
    c_df_adj <- ((nrow(chk_rand[chk_rand$group=='a', ]) * nrow(chk_rand[chk_rand$group=='b', ])) / (nrow(chk_rand)))
    
    invisible(try({
      mahalanobis(c_mns_a[c_tokeep], c_mns_b[c_tokeep], cov= c_cov[c_tokeep, c_tokeep]) * c_df_adj
    }, silent=T)) 
    })
  
  r_val <- mdis_chk %>% as.numeric() %>% na.omit() %>% quantile(., 0.07, na.rm=T)
  
  return(r_val)
}

rnd_rerand <- function(x, covs, .id='accpt_id', Pa=0.001) {
  posscomb <- choose(nrow(x), nrow(x)/2)
  if (posscomb<1000) Pa <- posscomb / 1000 
  
  cov_nms <- unlist(covs) %>% unname(.)
  
  # create a covariate matrix
  df_rerand <- x %>%
    select(all_of(.id), all_of(cov_nms)) 
  
  #facility id list
  fac_list <- x %>%
    select(all_of(.id)) %>%
    pull(.)
  
  ## Find Acceptable Cut-Off Value  
  cutoff <- choose_pa(df_rerand, fac_list, cov_nms, Pa=Pa)
  k <- length(cov_nms) #number of covariates, degrees of freedom
  
  iter <- min(posscomb, 2000) # upper limit of randomizations to perform  
  
  # start iteration counter
  counter <- 1L
  repeat { # search for good rerand
    
    # perform randomization
    chk_rand <- rnd_allot(fac_list) %>%
      inner_join(df_rerand, ., by=c('accpt_id' = 'id')) 
    
    #reset acceptance cutoff to unacceptable
    mdis_iter <- NULL
    
    c_mns_a <- colMeans(chk_rand[chk_rand$group=='a', cov_nms], na.rm=T)
    c_mns_b <- colMeans(chk_rand[chk_rand$group=='b', cov_nms], na.rm=T)
    c_cov <- cov(chk_rand[, cov_nms])
    c_tokeep <- which(c_mns_a != c_mns_b) #cant compute if no difference in means
    c_df_adj <- ((nrow(chk_rand[chk_rand$group=='a', ]) * nrow(chk_rand[chk_rand$group=='b', ])) / (nrow(chk_rand)))
    
    invisible(try({
      mdis_iter <- mahalanobis(c_mns_a[c_tokeep], c_mns_b[c_tokeep], cov= c_cov[c_tokeep, c_tokeep]) * c_df_adj
    }, silent=T))
    
    if (!is.null(mdis_iter)) if (mdis_iter<cutoff) break
    
    counter <- counter + 1
    if (counter==iter) break # failed randomization 
  }
  
  if (counter<iter) {
    rnd_rtrn <- chk_rand %>%
      select(group, all_of(.id)) 
  } else {
    rnd_rtrn <- NULL
  }
  return(rnd_rtrn)
}