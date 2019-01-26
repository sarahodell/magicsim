#' Count the number of breakpoints per individual
#'
#' @param ind An chromosome object
#' @param hom Whether the indvidual is homozygous or not. Default is FALSE. If true, the number of breakpoints for only one of the chromosomes will be given
#' @return The number of breakpoints in an individual. If hom is F, then it will return a vector of breakpoints on the two chromsomes, otherwise just one value will be given
#' @export

count_breakpoints<-function(ind,hom=F){
  if(hom){
    chrom=ind$h1$donors
    first=chrom[1]
    count=0
    for(i in chrom){
      if(i!=first){
        count=count+1
      }
      first=i
    }
    return(c(count))
  }
  else{
    count_vector=c()
    chrom=ind$h1$donors
    first=chrom[1]
    count=0
    for(i in chrom){
      if(i!=first){
        count=count+1
      }
      first=i
    }
    count_vector=c(count_vector,count)
    chrom=ind$h2$donors
    first=chrom[1]
    count=0
    for(i in chrom){
      if(i!=first){
        count=count+1
      }
      first=i
    }
    return(count_vector)
  }
}
