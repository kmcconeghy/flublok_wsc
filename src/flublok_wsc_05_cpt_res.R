#---------------------------------------------------------#
# Project: Randomization Study
# Convert grouped dataframe into final performance dataset
# Programmer: Kevin McConeghy
# Start: 09/29/2020
#--------------------------------------------------------#

source(here::here(list.files(here::here(), pattern='*cfg*')))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

# Goal to reorganize file and prepare it for reporting  

df_varlist <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[2]))

df_res <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[4]))

df_res_2 <- df_res %>%
  select(sample, size, res) 

## Add varlists 
df_res_2$totvars <- map(df_res$res, ~names(.[, 2:ncol(.)]))
df_res_2$adj_vars <- map(df_varlist$data, unlist)
df_res_2$adj_str <- map(df_varlist$strata, unlist)
df_res_2$nonadj <- map2(df_res_2$totvars, 
                        df_res_2$adj_vars,
                        function(x, y) {
                          x[!(x %in% y)]
                        })

# Four measures computed
## E[SMD] for all variables  
## E[SMD] for adj variables 
## E[SMD] for strata  
## E[SMD] for unadjusted

### All variables 

df_res_2$smd_e = pmap(
  list(
  #all variables
  map(df_res_2$res,
      function(x) {
        apply(x[, 4:ncol(x)], 1, mean, na.rm=T) %>%
          tibble(smd_e_tot=.)
        }),
  #adjusted variables
  pmap(list(df_res_2$res,
            df_res_2$adj_vars),
       function(a, b) {
         a_2 <- a[, b]
         apply(a_2, 1, mean, na.rm=T) %>%
           tibble(smd_e_adj=.)
         }),
  #2str
  pmap(list(df_res_2$res,
            df_res_2$adj_str),
       function(a, b) {
         a_2 <- a[, b]
         apply(a_2, 1, mean, na.rm=T) %>%
         tibble(smd_e_str=.)
       }),
  #unadjusted
  pmap(list(df_res_2$res,
            df_res_2$nonadj),
       function(a, b) {
         a_2 <- a[, b]
         apply(a_2, 1, mean, na.rm=T) %>%
         tibble(smd_e_unadj=.)
       })),
  bind_cols)

### unnest
df_res_3 <- df_res_2 %>%
  select(sample, size, res, smd_e) %>%
  unnest(cols = c(size, res, smd_e)) %>%
    ungroup(.) %>%
  mutate(method = factor(method, 
                         levels = c('rnd_simple',
                                    'rnd_strat',
                                    'rnd_paired',
                                    'rnd_kmns',
                                    'rnd_kmpca',
                                    'rnd_rerand'),
                         labels = c('Simple',
                                    'Categorical strata',
                                    'Pair-matched',
                                    'K-means stratified',
                                    'PCA K-means stratified',
                                    'Re-randomization')))

saveRDS(df_res_3, here::here('prj_dbdf', dta.names$f_cpt_list[5]))
