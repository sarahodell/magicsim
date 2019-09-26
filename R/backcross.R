#' backcross -------------------------------------------------------
#'  Backcross an Indv object
#'
#' @param p1 An Indv object, the one to backcross to
#' @param p2 An Indv object
#' @param c The number of chromosomes
#' @param ngen The number of generations of backcrossing
#' @param g_map The genetic map
#' @return An Indv object that has been backcrossed to p1 for ngen generations
#'
#'
#' @export
#'

backcross <- function(p1,p2,c,ngen,g_map){
  if(ngen==1){
    return(offspring(p1,p2,c,g_map))
  }
  bc=offspring(p1,p2,c,g_map)
  return(backcross(p1,bc,c,ngen-1,g_map))
}


#' backcross_pop -------------------------------------------------------
#'  Intialize a Pop object created by backcrossing two Indv objects
#'
#' @param p1 An Indv object, the one to backcross to
#' @param pop A Pop object
#' @param c The number of chromosomes
#' @param n The number of individuals in the population
#' @param ngen The number of generations of backcrossing
#' @param g_map The genetic map
#' @return An Pop object of individuals that have been backcrossed to p1 for ngen generations
#'
#'
#' @export
#'

backcross_pop <- function(p1,pop,c,n,ngen,g_map){
  bc_pop=Pop(nIndv=n,indvlist=vector("list",length=n))
  for(i in 1:n){
    bc_pop@indvlist[[i]]=backcross(p1,pop@indvlist[[i]],c,ngen,g_map)
  }
  return(bc_pop)
}
