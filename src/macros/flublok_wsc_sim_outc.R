# computes mean differences
cpt_power <- function(assign, sample, data, varlist) {
  
  # pull data from master lists
  data_2 <- data[data$sample==sample, ]
  vlist_2 <- varlist[varlist$sample==sample, ]
  df_1 <- inner_join(assign, data_2$data[[1]], 'accpt_id')
  
  strata <- vlist_2$strata[[1]]
  balance <- unlist(vlist_2$data[[1]])
  
  #assignment
  a <- if_else(df_1[, 'group']=='a', 1L, 0L)
  
  #error term
  e <- rnorm(nrow(df_1), 0, 1)
  
  balvars <- balance[!c(balance %in% strata)]
  unbalvars <- df_1 %>%
    select(-accpt_id, -group, -city, -zip5, -state, -county) %>%
    names(.)
  unbalvars <- unbalvars[!c(unbalvars %in% balvars)]
  
  #vars  
  x1 <- as_vector(df_1[, strata])
  x2 <- as_vector(df_1[, balvars[1]])
  x3 <- as_vector(df_1[, unbalvars[1]])
  
  y1 <- as_vector(a*0.2 + x1*0.2 + e)
  y2 <- as_vector(a*0.2 + x2*0.2 + e)
  y3 <- as_vector(a*0.2 + x3*0.2 + e)
  
  #strata var
  simmat <- tibble(y=y1, a=a, x=x1)
  fit <- lm(y ~ a + x, data=simmat)
  pval1 <- summary(fit)$coefficients[2,4]  
  
  #random balance var
  simmat <- tibble(y=y2, a=a, x=x2)
  fit <- lm(y ~ a + x, data=simmat)
  pval2 <- summary(fit)$coefficients[2,4] 
  
  #random unbalance var  
  simmat <- tibble(y=y3, a=a, x=x3)
  fit <- lm(y ~ a + x, data=simmat)
  pval3 <- summary(fit)$coefficients[2,4]
  
  res <- list(sample = sample, 
              size = data_2$size[[1]], 
              p_strata = pval1, 
              p_bala = pval2, 
              p_unbal = pval3)
  return(res)
}