#' self -------------------------------------------------------
#'  Self an Indv object
#'
#' @param p1 An Indv object
#' @param c The number of chromosomes
#' @param ngen The number of generations of selfing
#' @param g_map The genetic map
#' @return An Indv object that has been selfed for ngen generations
#'
#'
#' @export
#'

self <- function(p1,c,ngen,g_map){
  if(ngen==1){
    return(offspring(p1,p1,c,g_map))
  }
  selfed=offspring(p1,p1,c,g_map)
  return(self(selfed,c,ngen-1,g_map))
}

#' self_pop -------------------------------------------------------
#'  Self a Pop object
#'
#' @param pop A Pop object
#' @param c The number of chromosomes
#' @param ngen The number of generations of selfing
#' @param g_map The genetic map
#' @return An Pop object of individuals that have been selfed for ngen generations
#'
#'
#' @export
#' 

self_pop <- function(pop,c,ngen,g_map){
  n=pop@nIndv
  selfed=Pop(nIndv=n,indvlist=vector("list",length=n))
  for(i in 1:n){
    selfed@indvlist[[i]]=self(pop[i],c,ngen,g_map)
  }
  return(selfed)
}
