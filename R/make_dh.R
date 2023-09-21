#' make_dh ------------------------------------------------------------------------
#' Creates an Indv object of doubled haploid (DH) lines randomly selected from input Pop. For subset of individuals,
#' randomly selects one of the two chromosome pairs to double
#'
#' @param indv An Indv object
#' @param c Chromosome number (default is 10)
#' @param g_map The genetic map
#'
#' @return An Indv object that is completely homozygous
#' @export
#'

make_dh <- function(indv,c=10,g_map){
  new_dh=Indv(nChr=c,chromlist = vector("list",length=c))
  for(i in 1:c){
    if(runif(1)>=0.5){
      h=indv[i]@h1
    }
    else{
      h=indv[i]@h2
    }
    new_dh@chromlist[[i]]=chrom_init(i,g_map,h1_breakpoints=h@breakpoints,h1_donors = h@donors, h2_breakpoints = h@breakpoints, h2_donors = h@donors)
    new_dh@chromlist[[i]]@h1@xo_no=length(new_dh[i]@h1@breakpoints-1)
    new_dh@chromlist[[i]]@h2@xo_no=length(new_dh[i]@h2@breakpoints-1)
  }

  return(new_dh)
}

#' make_dh_pop ------------------------------------------------------------------------
#' Creates a Pop object of doubled haploid (DH) lines randomly selected from input Pop. For subset of individuals,
#' randomly selects one of the two chromosome pairs to double
#'
#' @param start_pop A Pop object
#' @param n The number of DH lines. Default is all of start_pop
#' @param c Chromosome number (default is 10)
#' @param g_map The genetic map
#'
#' @return A Pop object of n individuals that are completely homozygous
#' @export
#'

make_dh_pop <- function(start_pop,n=NULL,c=10,g_map){
  if(is.null(n)){
    n=start_pop@nIndv
  }
  draw=sample(seq(1,start_pop@nIndv),n,replace=F)

  dh_pop=Pop(nIndv=n,indvlist=vector("list",length=n))
  for(d in seq(1,n)){
    line=draw[d]
    # randomly choose one of the two chromosomes
    dh_pop@indvlist[[d]]=make_dh(start_pop[line],c,g_map)
  }
  return(dh_pop)
}
