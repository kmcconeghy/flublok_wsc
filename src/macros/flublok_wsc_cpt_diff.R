cpt_diff <- function(df, assignment, .varlist) {
  
  g_means <- inner_join(df, assignment, by=c('accpt_id')) %>%
    select('group', all_of(.varlist)) %>%
    group_by(group) %>%
    summarize_all(mean) 
  
  diff <- g_means[g_means$group=='b', !names(g_means) == 'group'] - 
    g_means[g_means$group=='a', !names(g_means) == 'group'] 
  
  return(diff)
}