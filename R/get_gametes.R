#' Generates a gamete randomly from one of the two chromosomes, with 50% chance of crossing over
#' @param parent list object in format created from chrom_init
#' @param recomb dataframe of genetic and physical map for chrom c
#' @param c Chromosome number
#'
#' @return One chromosome object

get_gametes<-function(parent,recomb,c){
  event=sample(c(1,2,3),1)
  if(event==1){
    result=parent$h1
  }
  else if(event==2){
    result=parent$h2
  }
  else{
    result=crossover(parent,recomb,c)
  }
  return(result)
}
