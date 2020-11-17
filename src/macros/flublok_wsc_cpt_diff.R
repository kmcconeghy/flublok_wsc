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