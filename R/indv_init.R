#' Intialize an Indv object
#'
#' @param chr Number of chromosome to initialize (int); default is 10
#' @param map Dataframe with physical and genetic positions for chromosome trying to be simulated. Of format chr,physical_position,gentic_position
#' @return A list of lists with attributes chr, h1, and h2, with h1 and h2
#' each containing vectors of breakpoint locations and donor names
#'
#'
#' @export

indv_init <- function(chr,map,h1_donors=c('A'),h1_breakpoints=NULL,h2_donors=c('A'),h2_breakpoints=NULL){
  chroms=list()
  for(c in 1:chr){
    chroms[[c]]=chrom_init(c,map,h1_breakpoints=h1_breakpoints,h1_donors=h1_donors,h2_breakpoints=h2_breakpoints,h2_donors=h2_donors)
  }
  ind = Indv(nChr=chr,chromlist=chroms)
  return(ind)
}
