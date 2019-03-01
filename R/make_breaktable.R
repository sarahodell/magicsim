#' Simulate a crossover between homologous chromosomes of a parent
#' @param xo the number of crossovers
#' @param donor1 list object of one chromosome in format created from chrom_init, crossover starts with this one
#' @param donor2 list object of one chromosome in format created from chrom_init
#' @param recomb dataframe of genetic and physical map for chrom c
#' @param c The chromosome being simulated
#' @param nu Interference intensity (optional: Default 12)
#'
#' @return a list object of one recombinant chromosome
#' @export

make_breaktable <- function(pop,c=10){
  breaks_table=c()
  l=length(pop)
  for(i in seq(1,l)){
    sample=sprintf('Sim%.0f',i)
    chr=c
    ind=pop[[i]]$h1
    first=ind$donors[1]
    start=0
    if(length(ind$breakpoints)==1){
      line=c(sample,chr,start,ind$breakpoints,first,first)
      breaks_table=rbind(breaks_table,line)
    }
    else if(length(unique(ind$donors))==1){
      end_index=length(ind$breakpoints)
      line=c(sample,chr,start,ind$breakpoints[end_index],first,first)
      breaks_table=rbind(breaks_table,line)
    }
    else{
      for(j in seq(1,length(ind$breakpoints))){
        if(ind$donors[j]!=first){
          line=c(sample,chr,start,ind$breakpoints[j],first,first)
          breaks_table=rbind(breaks_table,line)
          first=ind$donors[j]
          start=ind$breakpoints[j]+1
        }
      }
    }
  }
  breaks_table=as.data.frame(breaks_table)
  names(breaks_table)=c('sample','chr','start','end','donor1','donor2')

  return(breaks_table)
}
