#' Create dataframe of breakpoint locations for a single ChromPair object
#' @param breaktable A break table from function make_breaktable
#' @param n The number of chromosomes to show. Default is 10
#' @param c The chromosome to plot
#' @param het Whether or not the ChromPair is heterozygous. Default is TRUE
#'
#' @return a dataframe with the parental donor and location of breakpoints
#' @export
#' 

plot_Pop <- function(breaktable,n=10,c=10,het=T){
  sim_list=sprintf("Sim%0.f",seq(1,n))
  sub_sample=breaktable[breaktable$sample %in% sim_list,]
  if(het==F){
    sub_sample=sub_sample[sub_sample$hom=="H1",]
    sub_sample$sample=sprintf("%s_%s",sub_sample$sample,sub_sample$hom)
  }
  else{
    sub_sample$sample=sprintf("%s_%s",sub_sample$sample,sub_sample$hom)
  }
  plot_table=c()
  for (i in 1:dim(sub_sample)[1]){
    row=sub_sample[i,]
    line1=c(row$sample,row$chr,as.numeric(row$start),row$donor1)
    line2=c(row$sample,row$chr,as.numeric(row$end),row$donor1)
    
    plot_table=rbind(plot_table,line1)
    plot_table=rbind(plot_table,line2)
  }
  plot_table=as.data.frame(plot_table,stringsAsFactors=F)
  names(plot_table)=c("sample","chr","pos","donor")
  plot_table$pos=as.numeric(plot_table$pos)
  
  p <- ggplot(plot_table,aes(x=pos/1e6,y=1,color=donor)) + geom_line(aes(color=donor,group=sample)) +
    geom_area(aes(y=1,fill=donor),alpha=5/10) +  facet_grid(sample ~ .) +
    ggtitle("Chromosome Breakpoints") +
    xlab("Position (Mb)")  + guides(color=FALSE) +
    theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
  print(p)
}
