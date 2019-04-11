#' Intialize a chromosome object (diploid)
#'
#' @param c Chromosome to initialize (int); default is 10
#' @param map Dataframe with physical and genetic positions for chromosome trying to be simulated. Of format chr,physical_position,gentic_position
#' @param h1_breakpoints physical breakpoint locations where donor identity changes on copy one of chromosome; default is NULL (list)
#' @param h2_breakpoints physical breakpoint locations where donor identity changes on copy two of chromosome; default is NULL (list)
#' @param h1_donors donor identities for copy one of chromosome; default is A (list)
#' @param h2_donors donor identities for copy two of chromosome; default is A (list)
#' @return A list of lists with attributes chr, h1, and h2, with h1 and h2
#' each containing vectors of breakpoint locations and donor names
#' @examples
#' chrom_init(10)
#' @export

chrom_init <- function(c=10,map,h1_breakpoints=NULL,h1_donors=c('A'),h2_breakpoints=NULL,h2_donors=c('A')){
  chrom_end=max(map[map$chr==c,]$pos)

  if(is.null(h1_breakpoints) & is.null(h2_breakpoints)){
    h1_breakpoints=c(chrom_end)
    h2_breakpoints=c(chrom_end)
  }
  h1=Chrom(
           chr=c,
          breakpoints=h1_breakpoints,
          donors=h1_donors,
         xo_no=0)
  h2=Chrom(
           chr=c,
           breakpoints=h1_breakpoints,
           donors=h2_donors,
         xo_no=0)
  pair = ChromPair(chr=c,h1=h1,h2=h2)
  return(pair)
}
