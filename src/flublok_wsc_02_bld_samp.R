
#---------------------------------------------------------#
# 
# Project: Randomization Study
# Create a nested data-frame of sampled facilities  
# Programmer: Kevin McConeghy
# Start: 10/21/2020
# 
#--------------------------------------------------------#
source(list.files(pattern='*cfg*'))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

ltc_dta <- readRDS(here::here('prj_dbdf', dta.names$f_munge_list[1]))

set.seed(as.integer(ymd('2020-10-21')))

ltc_samp_10 <- replicate(1000, 
                         sample_n(ltc_dta , 10, replace=F), 
                         simplify=F) %>%
  bind_rows(.id = 'sample') %>%
  group_by(sample) %>%
  nest() #nest grouped df

ltc_samp_100 <- replicate(1000, 
                         sample_n(ltc_dta , 100, replace=F), 
                         simplify=F) %>%
  bind_rows(.id = 'sample') %>%
  group_by(sample) %>%
  nest() #nest grouped df

ltc_samp_500 <- replicate(1000, 
                          sample_n(ltc_dta , 500, replace=F), 
                          simplify=F) %>%
  bind_rows(.id = 'sample') %>%
  group_by(sample) %>%
  nest() #nest grouped df

saveRDS(ltc_samp_10, here::here('prj_dbdf', dta.names$f_cpt_list[1]))
saveRDS(ltc_samp_100, here::here('prj_dbdf', dta.names$f_cpt_list[2]))
saveRDS(ltc_samp_500, here::here('prj_dbdf', dta.names$f_cpt_list[3]))