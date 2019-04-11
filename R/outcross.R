#' Function to outcross population for ngen generations
#'
#' @param pop Pop object representing starting population
#' @param ngen The number of generations of outcrossing
#' @param c The chromosome being simulated
#' @param g_map The genetic map
#' @param endsize The size of the resulting population, if NULL, pop will be same size as starting pop
#'
#' @return A synthetic population that
#' has been randomly outcrossed for ngen generations (list of lists)
#' @export

outcross<-function(pop,ngen,c,g_map,endsize=NULL){
  size=length(pop@indvlist)
  if(is.null(endsize)){
    endsize=size
  }
  new_pop=Pop(nIndv=endsize,indvlist=vector("list",length=endsize))
  if(ngen==1){
    draw=sample(seq(1,size),endsize*2,replace=T)
    count=1
    for(p in seq(1,endsize*2,2)){
      draw1=draw[p]
      draw2=draw[p+1]
      new_pop@indvlist[[count]]=offspring(pop[draw1],pop[draw2],c,g_map)
      count=count+1
    }
    return(new_pop)
  }
  draw=sample(seq(1,size),endsize*2,replace=T)
  count=1
  for(p in seq(1,size*2,2)){
    draw1=draw[p]
    draw2=draw[p+1]
    new_pop@indvlist[[count]]=offspring(pop[draw1],pop[draw2],c,g_map)
    count=count+1
  }
  return(outcross(new_pop,ngen-1,c,g_map,endsize))
}
