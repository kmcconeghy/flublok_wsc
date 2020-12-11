# computes mean differences
cpt_power <- function(assign, sample, data, varlist) {
  
  # pull data from master lists
  data_2 <- data[data$sample==sample, ]
  vlist_2 <- varlist[varlist$sample==sample, ]
  df_1 <- inner_join(assign, data_2$data[[1]], 'accpt_id')
  
  strata <- unlist(vlist_2$strata)[[1]]
  balance <- unlist(vlist_2$data[[1]])
  
  #assignment
  a <- if_else(df_1[, 'group']=='a', 1L, 0L)
  
  #error term
  e <- rnorm(nrow(df_1), 0, 1)
  
  balvars <- balance[!c(balance %in% strata)]
  unbalvars <- df_1 %>%
    select(-accpt_id, -group, -city, -zip5, -state, -county, -any_of(c('strata', 'class'))) %>%
    names(.)
  unbalvars <- unbalvars[!c(unbalvars %in% balvars)]
  
  #vars  
  x1 <- as_vector(df_1[, strata])
  x2 <- as_vector(df_1[, balvars[1]])
  x3 <- as_vector(df_1[, unbalvars[1]])
  
  y1 <- as_vector(a*0.2 + x1*0.2 + e)
  y2 <- as_vector(a*0.2 + x2*0.2 + e)
  y3 <- as_vector(a*0.2 + x3*0.2 + e)
  
  str_form <- 'y ~ a'
  
  if (any(c('strata', 'class') %in% names(assign))) str_form <- paste0(str_form, ' + factor(strata)')
  
  #strata var
  simmat <- tibble(a=a) %>%
    bind_cols(., x2=x2, x3=x3) 
  
  if ('strata' %in% names(assign)) simmat <- bind_cols(simmat, strata = df_1$strata)
  if ('class' %in% names(assign)) simmat <- bind_cols(simmat, strata = df_1$class)
  
  fit <- lm(as.formula(str_form), data=bind_cols(y = y1, simmat))
  pval1 <- summary(fit)$coefficients[2,4]  
  
  #random balance var
  fit <- lm(as.formula(str_form), data=bind_cols(y = y2, simmat))
  pval2 <- summary(fit)$coefficients[2,4] 
  
  #random unbalance var  
  fit <- lm(as.formula(str_form), data=bind_cols(y = y3, simmat))
  pval3 <- summary(fit)$coefficients[2,4]
  
  res <- list(sample = sample, 
              size = data_2$size[[1]], 
              p_strata = pval1, 
              p_bala = pval2, 
              p_unbal = pval3)
  return(res)
}