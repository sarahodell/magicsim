#' Check if the chromosome has any breakpoints
#' 

no_breaks <- function(donor){
  return(length(donor$breakpoints)==1)
}