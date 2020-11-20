# computes mean differences
cpt_diff <- function(df, assignment, .varlist) {
  
  # if randomization fails, return missing dataframe
  diff <- df %>%
    select(all_of(.varlist)) %>%
    add_row(.before=1) %>%
    .[1, ] %>% 
    as.data.frame() %>%
    mutate_all(as.double)
  
  if (!is.null(assignment)) {
    g_means <- inner_join(df, assignment, by=c('accpt_id')) %>%
      select('group', all_of(.varlist)) %>%
      group_by(group) %>%
      summarize_all(mean) 
    
    diff <- g_means[g_means$group=='b', !names(g_means) == 'group'] - 
      g_means[g_means$group=='a', !names(g_means) == 'group'] 
  }
    
    
  return(diff)
}

# function to summarize over variable categories
cpt_vargrp_means <- function(df_smd, df_varcats) {
  
  df_ <- inner_join(df_smd, df_varcats, by=c('sample', 'size'))
  
  
  df_$smd_e_tot <- df_ %>% select(-size, -sample, -bal_vars,
                                  -non_vars, -str_vars) %>%
    rowMeans(., na.rm=T)
  
  df_$smd_e_tot <- df_ %>% select(-size, -sample, -bal_vars,
                                  -non_vars, -str_vars) %>%
    rowMeans(., na.rm=T)
  
  
  df_$smd_e_bal <- apply(df_, 1,
                         function(x) x) %>% # convert to list
    map(., function(x) x[names(x) %in% unlist(x['bal_vars'])]) %>%
    map(., .f = ~list(mean(unlist(.), na.rm=T))) %>%
    unlist(.)
  
  df_$smd_e_str <- apply(df_, 1,
                         function(x) x) %>% # convert to list
    map(., function(x) x[names(x) %in% unlist(x['str_vars'])]) %>%
    map(., .f = ~list(mean(unlist(.), na.rm=T))) %>%
    unlist(.)
  
  df_$smd_e_non <- apply(df_, 1,
                         function(x) x) %>% # convert to list
    map(., function(x) x[names(x) %in% unlist(x['non_vars'])]) %>%
    map(., .f = ~list(mean(unlist(.), na.rm=T))) %>%
    unlist(.)
  
  return(df_ %>%
           select(sample, size, starts_with('smd_e')))
}