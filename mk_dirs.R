
#-- Create canon folders
  dir_folders <- c('src', 'src/macros', 'log',
               'analysis',
               'output',
               'prj_dbdf',
               'manuscript')
  
  dir_fold_paths <- paste0(here::here(),
                           '/', dir_folders)
  sapply(dir_fold_paths, dir.create, showWarnings=F)