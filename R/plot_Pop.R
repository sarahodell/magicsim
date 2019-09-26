#' plot_Pop ---------------------------------------------------------------
#'
#'Plot one chromosome for multiple individuals in a population
#' @param breaktable A break table from function make_breaktable
#' @param n The number of chromosomes to show. Default is 10
#' @param c The chromosome to plot
#' @param het Whether or not the ChromPair is heterozygous. Default is TRUE
#'
#' @return a dataframe with the parental donor and location of breakpoints
#' @export
#'

plot_Pop <- function(breaktable,n=10,c=10,het=T){
  if(het==F){
    breaktable=breaktable[breaktable$hom=="H1",]
  }
  samples=unique(breaktable$sample)[1:n]
  breaktable=breaktable[breaktable$sample %in% samples,]
  breaktable$sample=sprintf("%s_%s",breaktable$sample,breaktable$hom)
  p <- ggplot(breaktable,aes(x=start/1e6,y=1,color=donor)) +
    geom_segment(aes(xend=end/1e6,color=donor,yend=1),lineend="butt",size=10) +
    facet_grid(sample ~ .) +
    ggtitle("Chromosome Breakpoints") +
    xlab("Position (Mb)")  +
    theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
  print(p)
}
