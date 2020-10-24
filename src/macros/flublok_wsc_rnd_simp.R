rnd_simple  <- function(x, .id) {

# Method 1. Simple Randomization  
## Random Assignment - Simple  

id_list <- x %>%
  select(all_of(.id)) %>%
  distinct(.) %>%
  pull(.)

rnd_rtrn <- jumble::rnd_allot(id_list)

return(rnd_rtrn)
}