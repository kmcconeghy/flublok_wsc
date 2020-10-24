cpt_diff <- function(df, assignment, .id, .varlist) {
  
  diff <- inner_join(df, assignment, by=c(.id='id')) %>%
    select('group', all_of(.varlist)) %>%
    split(., 'group') %>%
    .[1] - .[2]
  
  
}