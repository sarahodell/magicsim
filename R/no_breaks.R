#' Check if the chromosome has any breakpoints
#' @export

no_breaks <- function(donor){
  return(length(donor$breakpoints)==1)
}
