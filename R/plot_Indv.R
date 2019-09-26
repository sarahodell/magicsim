#' plot_Indv ---------------------------------------------------------------
#' Plot all chromosomes of a single Indv object
#' @param breaktable A break table from function make_breaktable
#' @param het Whether or not the ChromPair is heterozygous. Default is TRUE
#'
#' @return a dataframe with the parental donor and location of breakpoints
#' @export
#'

plot_Indv <- function(breaktable,het=T){
  if(het==F){
    breaktable=breaktable[breaktable$hom=="H1",]
  }
  breaktable$chr_f=factor(sprintf("%s_%s",breaktable$chr,breaktable$hom),levels=c(sprintf("%s_%s",breaktable$chr,breaktable$hom)))
  p <- ggplot(breaktable,aes(x=start/1e6,y=1,color=donor)) +
    geom_segment(aes(xend=end/1e6,color=donor,yend=1),lineend="butt",size=10) +
    facet_grid(chr_f ~ .) +
    ggtitle("Chromosome Breakpoints") +
    xlab("Position (Mb)")  +
    theme(axis.text.y=element_blank(),axis.title.y=element_blank(),axis.ticks.y=element_blank())
  print(p)
}
