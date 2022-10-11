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
  #trying out longer map to increase the number of crossovers
  map_length=max(recomb$scaled_cM) # + max(recomb$scaled_cM)/2
  xo = rpois(1,(map_length/100)) # No crossovers 50% of the time
  if(xo > 4){
    xo=4
  }
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
