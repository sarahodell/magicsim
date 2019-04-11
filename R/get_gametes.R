#' Generates a gamete randomly from one of the two chromosomes, with 50 percent chance of crossing over
#' @param parent ChromPair object in format created from chrom_init
#' @param recomb dataframe of genetic and physical map for chrom c
#' @param c Chromosome number
#'
#' @return One chromosome object
#' @export

get_gametes<-function(parent,recomb,c){
  #gametes=c(parent$h1,parent$h1,parent$h2,parent$h2)
  #xo = rpois(1,max(recomb$scaled_cM)/100)
  map_length=max(recomb$scaled_cM)
  xo = rpois(1,map_length/100) # No crossovers 50% of the time
  if(xo==0){
    if(runif(1)<0.5){
      result=parent@h1
    }
    else{
      result=parent@h2
    }
  }
  else{
    if(runif(1)<0.5){
      result=crossover(xo,parent@h2,parent@h1,recomb,c)
    }
    else{
      result=crossover(xo,parent@h1,parent@h2,recomb,c)
    }
  }
  return(result)
}
