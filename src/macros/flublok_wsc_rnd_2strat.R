# Method 1. Simple Randomization  
## Random Assignment - 2 strata 
rnd_2strat  <- function(x, strata, .id='accpt_id') {
  
  strat_1 <- df_samp_varlist$strata[[1]][1, 1] 
  strat_2 <- df_samp_varlist$strata[[1]][2, 1] 
  
  x <- df_samp$data[[1]]
  id_list <- x %>%
    mutate(cat_1 = ntile(strat_1, 5),
           cat_2 = ntile(strat_2, 5),
           strata = interaction(cat_1, cat_2)) %>%
    distinct(.id, cat_1, cat_2, strata)   
  
  rnd_rtrn <- jumble::rnd_str(id_list, strata, accpt_id)
  
  return(rnd_rtrn)
}