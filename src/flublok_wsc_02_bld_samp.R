#---------------------------------------------------------#
# 
# Project: Randomization Study
# Create a nested data-frame of sampled facilities  
# Programmer: Kevin McConeghy
# Start: 10/21/2020
# 
#--------------------------------------------------------#

# Create a nested list of dataframes, length 10000
# each item in the list is a dataset of facilities sampled
# from LTCFocus, of length x, where x is a random variable of
# even values from 10, 500, this is setting up simulated 
# randomizations for a study of size anywhere from 10 to 500   

source(list.files(pattern='*cfg*'))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

ltc_dta <- readRDS(here::here('prj_dbdf', dta.names$f_munge_list[1]))

set.seed(as.integer(ymd('2020-10-21')))
  
iters_persize <- 5000 

sample_size <- sort(rep(c(seq(8, 20, 2),
                          seq(40, 200, 20),
                          seq(250, 500, 50)), iters_persize))

l_samples <- length(sample_size)

ltc_samp <- sapply(sample_size, 
                     function(x) sample_n(ltc_dta, x, replace=F), 
                         simplify=F) %>%
  bind_rows(.id = 'sample') %>%
  group_by(sample) %>%
  nest(.) #nest grouped df

ltc_samp$size <- map(ltc_samp$data, nrow)

saveRDS(ltc_samp, here::here('prj_dbdf', dta.names$f_cpt_list[1]))