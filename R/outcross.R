#' Function to outcross population for ngen generations
#'
#' @param pop Starting population, pop[[i]] is the output of function offspring (list of lists)
#' @param ngen The number of generations of outcrossing
#' @param c The chromosome being simulated
#' @param g_map The genetic map
#'
#' @return A synthetic population of the same size as the initial population, pop that
#' has been randomly outcrossed for ngen generations (list of lists)
#' @export

outcross<-function(pop,ngen,c,g_map){
  size=length(pop)
  new_pop=list()
  if(ngen==1){
    draw=sample(seq(1,size),size*2,replace=T)
    count=1
    for(p in seq(1,size*2,2)){
      draw1=draw[p]
      draw2=draw[p+1]
      new_pop[[count]]=offspring(pop[[draw1]],pop[[draw2]],c,g_map)
      count=count+1
    }
    return(new_pop)
  }
  draw=sample(seq(1,size),size*2,replace=T)
  count=1
  for(p in seq(1,size*2,2)){
    draw1=draw[p]
    draw2=draw[p+1]
    new_pop[[count]]=offspring(pop[[draw1]],pop[[draw2]],c,g_map)
    count=count+1
  }
  return(outcross(new_pop,ngen-1,c,g_map))
}
