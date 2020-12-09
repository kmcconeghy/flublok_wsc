st_seed <- as.integer(ymd('2020-12-09'))
set.seed(st_seed)
readRDS(here::here('prj_dbdf', dta.names$f_cpt_list[3]))

# -- Run method 5 - PCA K-means randomization

cat('Begin PCA K-means randomization procedure....', '\n')
st_time <- Sys.time()

## execute - randomization  
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(df_samp$data))

assign <- map(.x = df_samp$data,
               .f = ~{
                 pb$tick()
                 rnd_pcakmns(.x, .id='accpt_id')})

cat('Computing differences....', '\n')
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(df_samp$data))
## compute - mean differences  
res_iter = pmap(list(df_samp$data, 
                     assign,
                     df_samp_varlist$fac_adj), #fac_adj are all variables that could be used in adj
                .f = ~{
                  pb$tick()
                  cpt_diff(..1, ..2, ..3)}) %>%
  bind_rows()

res_iter$size <- unlist(df_samp$size)
res_iter$sample <- unlist(df_samp$sample)

saveRDS(res_iter, here::here('prj_dbdf', dta.names$f_rand_res[5]))

end_time <- Sys.time()

## record time taken
sto_runinfo$runtimes$kmns <- end_time - st_time

## Runtimes  
saveRDS(sto_runinfo, here::here('prj_dbdf', dta.names$f_cpt_list[3]))

cat('Done', '\n')