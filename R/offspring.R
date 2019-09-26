#' Generate offspring from a cross between two parents
#'
#' @param p1 The first parents to cross. An Indv object created by indv_init()
#' @param p2 The second parent to cross. An Indv object created by indv_init()
#' @param chroms The number of chromosomes
#' @param g_map The genetic map
#'
#' @return A chromosome object that is the combination of two the two parents with potential crossing over
#' @export

offspring<-function(p1,p2,chroms,g_map){
  offspring=new("Indv",nChr=chroms,chromlist=vector("list", length=chroms))
  for(c in 1:chroms){
    recomb=g_map[g_map$chr==c,]
    rownames(recomb)=seq(1,dim(recomb)[1])
    h1=get_gametes(p1[c],recomb,c)
    h1=remove_unseen(h1)
    h2=get_gametes(p2[c],recomb,c)
    h2=remove_unseen(h2)
    offspring@chromlist[[c]]=ChromPair(chr=c,h1=h1,h2=h2)
  }
  return(offspring)
}
