rnd_rerand <- function(x, covs, .id='accpt_id', Pa =NULL, tune=100) {
  
  if (is.null(Pa)) Pa <- 1/ nrow(x)
  
  cov_nms <- combine(covs)
  
  ## Find Acceptable Cut-Off Value  
  k <- length(cov_nms) #number of covariates, degrees of freedom

  ## Per Morgan et al., acceptance is Chi-square distributed (Pa, df = # parameters)
  chi_val <- qchisq(Pa, k)
  
  # create a covariate matrix
  df_rerand <- x %>%
    select(all_of(.id), all_of(cov_nms)) 
  
  #facility id list
  fac_list <- x %>%
    select(all_of(.id)) %>%
    pull(.)
  
  iter <- (1 /Pa)*tune # upper limit of randomizations to perform  
  
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
      mdis_iter <- Rfast::mahala(c_mns_a[c_tokeep], c_mns_b[c_tokeep], sigma= c_cov[c_tokeep, c_tokeep]) * c_df_adj
      }, silent=T))
    
    if (!is.null(mdis_iter)) if (mdis_iter<chi_val) break
    
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