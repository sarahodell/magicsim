#' If the two crossover events are too close to each other,
#' only keep one of the crossover locations
#' @param xo_pos crossover position (int)
#' @param dist physical distance below which interference occurs (default is 10e6 bp)


interference <- function(xo_pos,dist=10e6){
  if(xo_pos[2]-xo_pos[1] < dist){
    choice=sample(c(1,2))
    return(xo_pos[choice])
  }
  else{
    return(xo_pos)
  }
}
