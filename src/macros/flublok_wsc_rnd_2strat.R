# Method 2. Stratified Randomization  
## Random Assignment - 2 strata 
rnd_2strat  <- function(x, strata, .id='accpt_id') {
  
  strat_1 <- strata[1, 1] 
  strat_2 <- strata[2, 1] 
  
  id_list <- x %>%
    mutate(cat_1 = ntile(strat_1, 5),
           cat_2 = ntile(strat_2, 5),
           strata = interaction(cat_1, cat_2)) %>%
    dplyr::select(.id, cat_1, cat_2, strata)   
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, id=.id)
  
  return(rnd_rtrn)
}