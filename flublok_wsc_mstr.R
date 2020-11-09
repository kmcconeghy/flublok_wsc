#--Project Set-up
  source(list.files(pattern='*cfg*'))

#-- Load data hierarchy  
  source(here::here('src', paste0(prj.specs$prj.prefix, '_lst_dtafiles.R')))

#--input facility file (only needs to run once)
  if (F) source(here::here('src', paste0(prj.specs$prj.prefix, '_01_inp_mkindta.R')))
  
#-- Create facility lists, variable lists 
  testrun <- F #set to true for quick run to test code
  if (T) {
    source(here::here('src', paste0(prj.specs$prj.prefix, '_02_bld_samp.R')))
    source(here::here('src', paste0(prj.specs$prj.prefix, '_03_bld_varlist.R')))
    rm(list=ls()[str_detect(ls(), 'ltc_')]) # clear datasets from memory
    gc() # clean up memory
  }

#-- Randomizations and computation
  if (T) {
    source(here::here('src', paste0(prj.specs$prj.prefix, '_04_cpt_dorandom.R')))
    source(here::here('src', paste0(prj.specs$prj.prefix, '_05_cpt_res.R')))
    rm(list=ls()[str_detect(ls(), 'df')]) # clear datasets from memory
    gc() # clean up memory
  }

#-- Reports  
  ## render_one, a function from Scotty
  ## remotes::install_github('kmcconeghy/Scotty')
  ## render_one('prefix', 'codepath', 'reportpath')
  if (T) {
    render_one('06_rpt_tab1', here::here('src'), here::here('output'))
    render_one('07_rpt_fig1', here::here('src'), here::here('output'))
  }
  
#End project