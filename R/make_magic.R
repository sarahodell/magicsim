#' make_magic ------------------------------------------------------------------------
#' Creates a MAGIC line in funnel crossing scheme from lines in list object start_pop
#'
#' @param start_pop A Pop object from the starting lines of the MAGIC, should be an even number of lines
#' and listed in the order that crossing should occur
#' @param c Chromosome number (default is 10)
#' @param g_map The genetic map
#'
#' @return An indv object that is the result of funnel crossing the lines in start_pop
#' @export


make_magic<-function(start_pop,c=10,g_map){
  if(start_pop@nIndv==2){
    return(offspring(start_pop[1],start_pop[2],c,g_map))
  }
  magiclist=list()
  count=1
  for(i in seq(1,start_pop@nIndv,2)){
    magiclist[[count]]=offspring(start_pop[i],start_pop[i+1],c,g_map)
    count=count+1
  }
  magic=Pop(nIndv=length(magiclist),indvlist=magiclist)
  return(make_magic(magic,c,g_map))
}


#' make_magic_pop ------------------------------------------------------------------------
#' Creates a MAGIC population in funnel crossing scheme from lines in list object start_pop
#'
#' @param start_pop A Pop object from the starting lines of the MAGIC, should be an even number of lines
#' and listed in the order that crossing should occur
#' @param c Chromosome number (default is 10)
#' @param g_map The genetic map
#' @param popsize The size of the resulting population
#'
#' @return A Pop object that is the result of funnel crossing the lines in start_pop
#' @export


make_magic_pop<-function(start_pop,c=10,g_map,popsize){
  magic=list()
  for(i in 1:popsize){
    magic[[i]]=make_magic(start_pop,c,g_map)
  }
  magicpop=Pop(nIndv=popsize,indvlist=magic)
  return(magicpop)
}
