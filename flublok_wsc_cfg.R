#-- load tidyverse
  cat('loading Scotty canon packages....')
    pkg_toload <- c('tidyverse', 'lubridate', 
                    'here', 'Scotty', 'knitr')
    
    hold_del <- sapply(pkg_toload, 
                       require,
                       warn.conflicts=F, quietly=T,
                       character.only=T)
  cat('Done', '\n')

#--Project specifics
prj.dir <- getwd()

#---Log output  
#con <- file(here::here('log', paste0("console_", timestamp(), ".log")))
#sink(con, split=T, append=F)

cat('load project specifics....', '\n')
  cat(paste0('Project working directory:', prj.dir), '\n')
  source(here(list.files(here(), pattern='*_specs.R')))
  cat('Project Root Name:', prj.specs$prj.prefix, ' ', 
      'Start Date:', prj.specs$prj.dt_start, '\n')
  cat('Programmer:', prj.specs$prj.coder, '\n')
  cat(paste0('Start Time: ', prj.specs$prj.runtime), '\n')
cat('Done', '\n')
  
#--System options
cat('Setting R system options....')
  options(scipen=999)
  options(max.print = 250)
cat('Done', '\n')

#--rtools, latex needed for some
cat('Checking dependencies', '\n')
  cat('--Rtools is installed:', pkgbuild::check_rtools(), '\n')
  cat('--Tinytex is installed:', tinytex:::is_tinytex(), '\n')
cat('Done', '\n')
  
#--Directories
cat('Checking directories...')
  source(here(list.files(here(), pattern='*_dirs.R')))
  share.dbdf.path <- paste0(dirname(here::here()), '/', 'dbdf')
  sas.dbdf.path <- paste0('P:/adap5/dbdf/analdata')
cat('Done', '\n')

#--Source local functions
cat('Sourcing local functions....')
  prj.func <- list.files(path=here::here('src', 'macros'), 
                         pattern = '.R', full.names = T, 
                         recurs = F, include.dirs = F)
    #--Load local functions
    for (i in prj.func) {
      source(i)
    }
cat('Done', '\n')

sessioninfo::session_info()

cat('Project has been loaded according to config file', '\n', '\n')

