#' Intialize a Pop object
#'
#' @param n Number of individuals to initialize (int)
#' #' @param chroms Number of chromosomes
#' @param donors List of names of individuals for founder population
#' @param g_map Dataframe with physical and genetic positions for chromosome trying to be simulated. Of format chr,physical_position,gentic_position
#' @return A a Pop object of indred founder lines
#'
#'
#' @export

pop_init <- function(n=4,chroms,donors=c("A","B","C","D"),g_map,poptype=NULL){
  indvlist=list()
  for(i in 1:n){
    for(c in 1:chroms){
      indvlist[[i]]=indv_init(c,g_map,h1_donors=donors[i],h2_donors=donors[i])
    }
  }
  pop = new("Pop",nIndv=n,indvlist=indvlist)
  if(!is.null(poptype)){
    pop@poptype=poptype
  }
  return(pop)
}
