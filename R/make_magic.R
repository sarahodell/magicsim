#' Creates a MAGIC population in funnel crossing scheme from lines in list object start_pop
#'
#' @param start_pop A list of chromosome objects from the starting lines of the MAGIC, should be an even number of lines
#' and listed in the order that crossing should occur
#' @param c Chromosome number (default is 10)
#' @param g_map The genetic map
#'
#' @return A list of chromosome objects that are a result of funnel crossing the lines in start_pop
#' @export


make_magic<-function(start_pop,c=10,g_map){
  if(length(start_pop)==2){
    return(offspring(start_pop[[1]],start_pop[[2]],c,g_map))
  }
  magic=list()
  count=1
  for(i in seq(1,length(start_pop),2)){
    magic[[count]]=offspring(start_pop[[i]],start_pop[[i+1]],c,g_map)
    count=count+1
  }
  return(make_magic(magic,c,g_map))
}
