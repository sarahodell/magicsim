#' Create an F1 from two parents
#'
#' @param p1 The first parents to cross. An Indv object created by indv_init()
#' @param p2 The second parent to cross. An Indv object created by indv_init()
#' @param g_map Dataframe with physical and genetic positions for chromosome trying to be simulated. Of format chr,physical_position,gentic_position
#' @param chroms chromsome number, default=10
#' @return An Indv object with one chromosome pulled from both of the parents
#' @export

make_f1 <- function(p1,p2,g_map,chroms=10){
  f1=Indv(nChr=chroms,chromlist=vector("list", length = chroms))
  for(c in 1:chroms){
    if(runif(1)>=0.5){
      h1_donors=p1@chromlist[[c]]@h1@donors
      h2_donors=p2@chromlist[[c]]@h1@donors
      h1_breakpoints=p1@chromlist[[c]]@h1@breakpoints
      h2_breakpoints=p2@chromlist[[c]]@h1@breakpoints
    }
    else{
      h1_donors=p2@chromlist[[c]]@h1@donors
      h2_donors=p1@chromlist[[c]]@h1@donors
      h1_breakpoints=p2@chromlist[[c]]@h1@breakpoints
      h2_breakpoints=p1@chromlist[[c]]@h1@breakpoints
    }
    f1@chromlist[[c]]=chrom_init(c=c,g_map,h1_breakpoints = h1_breakpoints,h1_donors=h1_donors,h2_breakpoints=h2_breakpoints,h2_donors=h2_donors)
  }
  return(f1)
}
