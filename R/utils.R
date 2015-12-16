

# remove all spaces in column names
rm_space_names <- function(df) df %>% 
  set_colnames(., str_replace_all(names(.), " ", ""))

#' Is the vector sorted?
is_sorted <- function(vec) all(vec == sort(vec))


