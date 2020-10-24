
#---------------------------------------------------------#
# 
# Project: Randomization Study
# File to Insheet LTFocus data
# Programmer: Kevin McConeghy
# Start: 09/29/2020
# 
#--------------------------------------------------------#
source(list.files(pattern='*cfg*'))
source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

ltcfocus <- readxl::read_excel(here::here('prj_dbdf', dta.names$f_raw_list[1]))

# some editing  
len <- nrow(ltcfocus)

ltcfocus_2 <- ltcfocus %>%
  select(-year, -PROV1680) %>%
  rename(city = PROV3225,
         zip5 = PROV2905) %>%
  mutate_at(vars('county', 'totbeds', zip5,
                 starts_with('pay'), 'restrain',
                 'acuindex2', 'rn2nrs', contains('ppd'),
                 'nhlat', 'nhlong', 'occpct', starts_with('agg'),
                 starts_with('avg'), starts_with('pct'), 
                 starts_with('NHC'), 'hospptyr', starts_with('obs'),
                 starts_with('adj'), starts_with('mds'), starts_with('sd'),
                 starts_with('Pct'), adm_bed), as.numeric) %>%
  mutate_at(vars('accpt_id', 'city', 'zip5', 'state', 'county'), 
            as.character) %>%
  select(-PROV0475, -PROV2720) %>%
  mutate_at(vars('alzunit', 'anyunit', 'multifac', 'profit', 'hospbase',
                 'anymdex'), .funs = list(~case_when(
                   .=='Yes' ~ 1L, 
                   .=='No' ~ 0L,
                   T ~ NA_integer_))) %>%
  select_if(~sum(is.na(.))/len<=0.1) %>% # drop mostly missing columns
  na.omit(.) %>% # drop any miss
  # Scale numeric vars by SD (for comparison later)
  mutate_if(~(is.numeric(.) & n_distinct(.)>2), scale) 

nrows <- nrow(ltcfocus)
n_distinct_facid <- n_distinct(ltcfocus$accpt_id)

# Unit testing  
testthat::expect_equal(nrows, n_distinct_facid)

saveRDS(ltcfocus_2, here::here('prj_dbdf', dta.names$f_munge_list[1]))