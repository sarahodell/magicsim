#' remove_unseen -------------------------------------------------------
#'  remove invisible crossovers from Chrom object
#'
#' @param chr An Indv object
#' @return A Chrom object that has had invisible crossovers removed
#'
#'
#' @export
#'

remove_unseen <- function(chr){
  drop=c()
  l=length(chr@donors)
  if(l==1){
    return(chr)
  }
  start=chr@donors[l]
  for(i in (l-1):1){
    if(chr@donors[i]==start){
      drop=c(drop,i)
    }
    start=chr@donors[i]
  }
  if(length(drop)>0){
    chr@donors=chr@donors[-drop]
    chr@breakpoints=chr@breakpoints[-drop]
    chr@xo_no=(length(chr@donors)-1)
  }
  return(chr)
}
