#' If the two crossover events are too close to each other,
#' only keep one of the crossover locations
#' @param xo_pos crossover position (int)
#' @param dist physical distance below which interference occurs (default is 10 Mb)
#' @export


interference <- function(xo_pos,dist=1e5){
  while(sum(diff(xo_pos) < dist) != 0){
    closest=c(which.min(diff(xo_pos)),which.min(diff(xo_pos))+1)
    choice=sample(closest,1)
    xo_pos[choice]=xo_pos[choice]+5e5
  }
  return(xo_pos)
}
