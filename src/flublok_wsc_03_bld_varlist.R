#---------------------------------------------------------#
# 
# Project: Randomization Study
# Generate a list of variables for adjustment/comparison
# Programmer: Kevin McConeghy
# Start: 10/21/2020
# 
#--------------------------------------------------------#

source(list.files(pattern='*cfg*'))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

ltc_dta <- readRDS(here::here('prj_dbdf', dta.names$f_munge_list[1]))

var_all <- names(ltc_dta)

fac_id <- 'accpt_id'

fac_noadj <- c('city', 'zip5', 'state', 'county', 'PctPvtDays',
               'adj_medianlos', 'pctNHdaysSNF', 'hospptyr')

fac_adj <- var_all[!var_all %in% c(fac_id, fac_noadj)]

set.seed(as.integer(ymd('2020-12-08')))

fac_varsamp <- replicate(l_samples, 
                         data.frame(var_adj = sample(fac_adj, size=5, replace=F)), 
                         simplify=F) %>%
  bind_rows(.id='sample') %>%
  group_by(sample) %>%
  nest() %>%
  mutate(fac_id = 'accpt_id')

fac_varsamp$fac_adj <- list(fac_adj)
fac_varsamp$strata <- bind_rows(lapply(fac_varsamp$data, lapply, 
                             function(x) (sample(x, size=1)))) 
 

saveRDS(fac_varsamp, here::here('prj_dbdf', dta.names$f_cpt_list[2]))