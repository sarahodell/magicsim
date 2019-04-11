# Pop ------------------------------------------------------------------

#' @title Pop
#'
#' @description
#' The raw individuals class contains information on
#'  chromosome breakpoints and donors for one individual.
#'
#' @param x a 'Pop'
#' @param i index of of individuals
#' @param ... additional 'Pop' objects
#'
#' @slot nIndv number of individuals
#' @slot poptype type of population (optional)
#' @slot indvlist list of individuals
#'
#'
#' @export
#'

setClass("Pop",
            slots=c(nIndv="numeric",
                    poptype="character",
                    indvlist="list"
                    )) -> Pop

setValidity("Pop",function(object){
  errors=character()
  if(object@nIndv != length(object@indvlist)){
    errors=c(errors,"object@nIndv != length(object@indvlist)")
  }
  if(length(errors)==0){
    return(TRUE)
  }
  else{
    return(errors)
  }
})

#' @describeIn  Pop Extract Indv by index
setMethod("[",
          signature(x="Pop"),
          function(x,i){
            if(any(abs(i)>x@nIndv)){
              stop("Trying to select invalid individuals")
            }
            return(x@indvlist[[i]])
          }
)
