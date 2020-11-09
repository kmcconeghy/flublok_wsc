
pct_chg <- function(num, denom, string=F) {
  pct_return <- ((num - denom) / denom)*100
  if (string) pct_return <- sprintf("%0.2f%%", pct_return)
  return(pct_return)
}

get_pcent_chg <- function(df) {
    denomat <- df[1, 2:ncol(df)]
    
    pct_chg_res <-
      apply(df[2:nrow(df), 2:ncol(df)], 1, 
            pct_chg, denom=denomat, string=F) %>%
      bind_rows(.) %>%
      as.data.frame() 
    
    summ_pct_df <- df[2:nrow(df), ] 
    
    
    summ_pct_df[1:nrow(summ_pct_df), 2:ncol(summ_pct_df)] <- pct_chg_res
    
    return(summ_pct_df)
}