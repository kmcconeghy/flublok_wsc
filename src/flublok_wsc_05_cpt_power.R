
# load original trial lists
df_samp <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[1]))
df_samp_varlist <- readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[2]))

# labels
rndm_methods <- c('rnd_simple',
                  'rnd_strat',
                  'rnd_pair',
                  'rnd_kmns',
                  'rnd_kmpca',
                  'rnd_rerand')

# length of each trial
sizes <- df_samp %>%
  select(size) %>%
  unlist %>%
  unique(.)

# each randomization result
d_res_simp    <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[1])) 
d_res_strat   <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[2])) 
d_res_pair    <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[3])) 
d_res_kmns    <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[4])) 
d_res_pcakmns <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[5])) 
d_res_rerand  <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[6])) %>%
  na.omit(.)

#as one list
d_res <- list(simple  = d_res_simp,
              strat   = d_res_strat,
              pair    = d_res_pair,
              kmns    = d_res_kmns,
              pcakmns = d_res_pcakmns,
              rerand  = d_res_rerand)

st_seed <- as.integer(ymd('2020-12-09'))
set.seed(st_seed)

run_cpt_power <- function(x) {
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", 
                         total = length(x$assign))
  
  ## compute - mean differences  
  res_iter = pmap(list(x$assign,
                       x$sample),
                  .f = ~{
                    pb$tick()
                    cpt_power(..1, ..2, df_samp, df_samp_varlist)}
  ) %>%
  bind_rows()

  return(res_iter)
}


# Goal to simulate an outcome  
cat('Estimating power...', '\n')
d_res_2 <- map(d_res, run_cpt_power)
cat('Done')

d_res_3 <- map(d_res_2, 
      function(x) {
        x %>%
          group_by(size) %>%
          summarize_at(vars(-sample), ~sum(.<0.05, na.rm=T)/n())
      }) %>%
  bind_rows(.id = 'method') %>%
  mutate(method = factor(method, 
                         levels = c('simple',
                                    'strat',
                                    'pair',
                                    'kmns',
                                    'pcakmns',
                                    'rerand'),
                         labels = c('Simple',
                                    'Categorical strata',
                                    'Pair-matched',
                                    'K-means stratified',
                                    'PCA K-means stratified',
                                    'Re-randomization')))
    
saveRDS(d_res_3, here::here('prj_dbdf', dta.names$f_cpt_list[5]))
