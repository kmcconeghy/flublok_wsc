
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
d_res_simp <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[1])) 
d_res_strat <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[2])) 
d_res_pair <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[3])) 
d_res_kmns <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[4])) 
d_res_rerand <- readRDS(here::here('prj_dbdf', dta.names$f_rand_res[6])) %>%
  na.omit(.)

#as one list
d_res <- list(simple = d_res_simp,
              strat  = d_res_strat,
              pair   = d_res_pair,
              kmns   = d_res_kmns,
              rerand = d_res_rerand)

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
    
saveRDS(d_res_2, here::here('prj_dbdf', dta.names$f_cpt_list[5]))
