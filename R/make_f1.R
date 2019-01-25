#' Create an F1 from two parents
#'
#' @param p1 The first parents to cross. A chromosome object created by chrom_init()
#' @param p2 The second parent to cross. A chromosome object created by chrom_init()
#' @param map Dataframe with physical and genetic positions for chromosome trying to be simulated. Of format chr,physical_position,gentic_position
#' @param c chromsome number (default is 10)
#' @return A chromosome object with one chromosme pulled from both of the parents

make_f1 <- function(p1,p2,map,c=10){
  if(runif(1)>=0.5){
    h1_donors=p1$h1$donors
    h2_donors=p2$h1$donors
    h1_breakpoints=p1$h1$breakpoints
    h2_breakpoints=p2$h1$breakpoints
  }
  else{
    h1_donors=p2$h1$donors
    h2_donors=p1$h1$donors
    h1_breakpoints=p2$h1$breakpoints
    h2_breakpoints=p1$h1$breakpoints
  }
  f1=chrom_init(c=c,map,h1_breakpoints = h1_breakpoints,h1_donors=h1_donors,h2_breakpoints=h2_breakpoints,h2_donors=h2_donors)
  return(f1)
}
