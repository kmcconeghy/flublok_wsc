rnd_pairmatch <- function(x, covs, .id='accpt_id') {

  cov_nms <- unlist(covs)
  
  # create a covariate matrix
  df_mtch <- x %>%
    select(all_of(.id), all_of(cov_nms)) %>%
    as.data.frame(.) 
  
  # create distances
  df_dist <- gendistance(df_mtch, idcol=1)
  
  # create distancematrix object
  df_mdm <- distancematrix(df_dist)
  
  # create matches
  df_mtch_2 <- nonbimatch(df_mdm)
  
  id_list <- df_mtch_2$matches %>%
    select(Group1.ID, Group2.ID) %>%
    mutate(class = row_number()) %>%
    tidyr::pivot_longer(cols = c('Group1.ID', 'Group2.ID'),
                        values_to = .id) %>%
    select(all_of(.id), class) 
  
  rnd_rtrn <- jumble::rnd_str(id_list, class, id=.id)
  
  return(rnd_rtrn)
}