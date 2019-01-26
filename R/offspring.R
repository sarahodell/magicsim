#' Generate offspring from a cross between two parents
#'
#' @param p1 The first parents to cross. A chromosome object created by chrom_init()
#' @param p2 The second parent to cross. A chromosome object created by chrom_init()
#' @param chroms List of chromosomes, or one chromosome
#'
#' @return A chromosome object that is the combination of two the two parents with potential crossing over
#' @export

offspring<-function(p1,p2,chroms){
  offspring=list()
  for(c in chroms){
    recomb=ogutmap[ogutmap$chr==c,]
    h1=get_gametes(p1[[c]],recomb,c)
    h2=get_gametes(p2[[c]],recomb,c)
    offspring[[c]]=list(chr=c,h1=h1,h2=h2)
  }
  return(offspring)
}
