dta.names <- list(
  f_sas_list = c(''),
  f_raw_list = c(''),
  f_munge_list = c(''),
  f_cpt_list = c(''),
  f_datasets = c('')
)


#--Filename checks  

#---SAS and RDS names match
chk_run <- function() {
  chk_f_sas_list <- str_replace(dta.names$f_sas_list, '\\..*', '') == str_replace(dta.names$f_raw_list, '\\..*', '')
  if (any(chk_f_sas_list==F)) stop('SAS and RDS raw filenames do not match')
}
chk_run()