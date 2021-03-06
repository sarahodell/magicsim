#'make_subtable --------------------------------------------------------
#'
#'  Create dataframe of breakpoint locations for a single ChromPair object
#' @param chrom A ChromPair object
#' @param sample The sample name
#' @param het Whether or not the ChromPair is heterozygous. Default is TRUE
#'
#' @return a dataframe with the parental donor and location of breakpoints
#' @export

make_subtable <- function(chrom,sample,het=T){
  c=chrom@chr
  breaks_table=c()
  #If heterozygous, look at both
  if(het==T){
    pairs=c(chrom@h1,chrom@h2)

  }
  else{
    pairs=c(chrom@h1)
  }
  h="H1"
  for(p in pairs){
    ind=p
    first=ind@donors[1]
    start=0
    if(length(ind@breakpoints)==1){
      line=c(sample,c,h,start,ind@breakpoints,first)
      breaks_table=rbind(breaks_table,line)
    }
    else if(length(unique(ind@donors))==1){
      end_index=length(ind@breakpoints)
      line=c(sample,c,h,start,ind@breakpoints[end_index],first)
      breaks_table=rbind(breaks_table,line)
    }
    else{
      for(j in seq(1,length(ind@breakpoints))){
        if(ind@donors[j]!=first){
          line=c(sample,c,h,start,ind@breakpoints[j],first)
          breaks_table=rbind(breaks_table,line)
          first=ind@donors[j]
          start=ind@breakpoints[j]+1
        }
        else if(j==length(ind@breakpoints)){
          line=c(sample,c,h,start,ind@breakpoints[j],first)
          breaks_table=rbind(breaks_table,line)
        }
      }
    }
    h="H2"
  }
  breaks_table=as.data.frame(breaks_table,stringsAsFactors=F)
  rownames(breaks_table)=seq(1,dim(breaks_table)[1])
  names(breaks_table)=c('sample','chr','hom','start','end','donor')
  return(breaks_table)
}

#' make_pop_breaktable --------------------------------------------------------
#' Create dataframe of breakpoint locations for a one chromsome for a Pop object
#' @param pop A Pop object
#' @param n The number of individuals to include in the table. Default is all of them.
#' @param c The chromosome number
#' @param het Whether or not the chromosomes are heterozygous. Default is TRUE
#'
#' @return a dataframe with the parental donor and location of breakpoints
#' @export

make_pop_breaktable <- function(pop,n=NULL,c=10,het=T){
  breaks_table=c()
  if(is.null(n)){
    n=length(pop@indvlist)
  }
  for(i in seq(1,n)){
    sample=sprintf('Sim%.0f',i)
    ind=pop[i][c]
    breaks=make_subtable(ind,sample,het)
    breaks_table=rbind(breaks_table,breaks)
  }
  breaks_table=as.data.frame(breaks_table,stringsAsFactors=F)
  rownames(breaks_table)=seq(1,dim(breaks_table)[1])
  names(breaks_table)=c('sample','chr','hom','start','end','donor')
  breaks_table$sample=as.character(breaks_table$sample)
  breaks_table$chr=as.numeric(as.character(breaks_table$chr))
  breaks_table$hom=as.character(breaks_table$hom)
  breaks_table$start=as.numeric(as.character(breaks_table$start))
  breaks_table$end=as.numeric(as.character(breaks_table$end))
  breaks_table$donor=as.character(breaks_table$donor)
  return(breaks_table)
}

#' make_indv_breaktable --------------------------------------------------------
#' Create dataframe of breakpoint locations for a one Indv object for all chromosomes
#' @param indv A Pop object
#' @param het Whether or not the chromosomes are heterozygous. Default is TRUE
#'
#' @return a dataframe with the parental donor and location of breakpoints
#' @export

make_indv_breaktable<-function(indv,het=T){
  breaks_table=c()
  chr=indv@nChr
  for(i in 1:chr){
    ind=indv[i]
    sample="Sim1"
    breaks=make_subtable(ind,sample,het)
    breaks_table=rbind(breaks_table,breaks)
  }
  breaks_table=as.data.frame(breaks_table,stringsAsFactors=F)
  rownames(breaks_table)=seq(1,dim(breaks_table)[1])
  names(breaks_table)=c('sample','chr','hom','start','end','donor')
  breaks_table$sample=as.character(breaks_table$sample)
  breaks_table$chr=as.numeric(as.character(breaks_table$chr))
  breaks_table$hom=as.character(breaks_table$hom)
  breaks_table$start=as.numeric(as.character(breaks_table$start))
  breaks_table$end=as.numeric(as.character(breaks_table$end))
  breaks_table$donor=as.character(breaks_table$donor)
  return(breaks_table)
}
