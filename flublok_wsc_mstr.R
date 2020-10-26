#--Project Set-up
  source(list.files(pattern='*cfg*'))

  ## - Project Prefix  
  prj_pfix <- prj.specs$prj.prefix
  
#-- Load data hierarchy  
  source(here::here('src', paste0(prj_pfix, '_lst_dtafiles.R')))

#-- dataset / munge  
  if (T) source(here::here('src', paste0(prj_pfix, '_01_inp_mkindta.R')))
  if (T) source(here::here('src', paste0(prj_pfix, '_02_bld_samp.R')))
  if (T) source(here::here('src', paste0(prj_pfix, '_03_bld_varlist.R')))
  rm(list=ls()) # clear datasets from memory
  gc() # clean up memory
  
  #-- Randomizations  
  if (T) source(here::here('src', paste0(prj_pfix, '_04_cpt_dorandom.R')))
  if (T) source(here::here('src', paste0(prj_pfix, '_05_cpt_performance.R')))
  
#--IF ONLY WANT ONE FILE TO RUN
  #render_one('B04', wd.CodeFiles, ReportFiles)

cat(paste0('Project Run: ', prj.RunTime %--% Sys.time()))

#End project