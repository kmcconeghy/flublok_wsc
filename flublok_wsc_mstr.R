#--Project Set-up
  source(list.files(pattern='*cfg*'))

#-- Load data hierarchy  
  source(here::here('src', 'flublok_wsc_lst_dtafiles.R'))

#-- dataset / munge  
  if (T) source(here::here('src', 'flublok_wsc_inp_01_mkindta.R'))

#-- Randomizations  
  if (T) source(here::here('src', 'flublok_wsc_bld_01_dorandom.R'))
  
#--IF ONLY WANT ONE FILE TO RUN
  #render_one('B04', wd.CodeFiles, ReportFiles)

cat(paste0('Project Run: ', prj.RunTime %--% Sys.time()))

#End project