# Method 2. Stratified Randomization  
## Random Assignment - 1 strata 
rnd_strat  <- function(x, strata, .id='accpt_id') {
  
  strat_1 <- strata[1, 1] 
  
  # to prevent too many strata in small sample size, x,
  # y = floor(x / 5), divide sample by 5 and round down
  # z = max(y, 2), minimum strata must be 2
  # strata size = min(z, 5), no strata >5  
  cat_size <- min(max(floor(nrow(x)/5), 2), 5)
  
  id_list <- x %>%
    mutate(strata = ntile(x[,strat_1], cat_size)) %>%
    dplyr::select(all_of(.id), strata)   
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  
  return(rnd_rtrn)
}