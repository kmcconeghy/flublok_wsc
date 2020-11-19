# Simulation Set-up 
rndm_methods <- c('rnd_simple',
                  'rnd_strat',
                  'rnd_paired',
                  'rnd_kmns',
                  'rnd_kmpca',
                  'rnd_rerand')

sizes <- df_samp %>%
  select(size) %>%
  unlist %>%
  unique(.)

rndm_tab <- tibble(method = rndm_methods) 
rndm_tab[df_samp_varlist$fac_adj[[1]]] <- NA_real_

df_rand <- tibble(size = sizes, 
                  res = replicate(length(sizes), rndm_tab, simplify = F))

## Load randomization results  

d_res_simp <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[1])) 
d_res_strat <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[2])) 

d_res <- list(simple = d_res_simp,
              strat = d_res_strat)

for (i in 1:length(d_res)) {
  d_res[[i]] <- d_res[[i]] %>%
    group_by(size) %>%
    summarize_all(~sd(., na.rm=T)) %>%
    group_by(size) %>%
    nest()
}

for (i in 1:length(d_res)) {
  ## add results to data.frame  
  df_rand$res <- map2(df_rand$res,  
                      d_res[[i]]$data,
                      .f = function(x, y, rw=i) {
                        x[rw, 2:ncol(x)] <- y
                        return(x)
                      })
}

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
