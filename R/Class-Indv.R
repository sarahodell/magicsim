# Indv ------------------------------------------------------------------

#' @title Indv
#'
#' @description
#' The raw individuals class contains information on
#'  chromosome breakpoints and donors for one individual.
#'
#' @param x a 'Indv'
#' @param ... additional 'Indv' objects
#'
#' @slot nChr number of chromosomes
#' @slot chroms list of ChromPair objects
#' @slot phenolist list of Phenotype objects
#'
#'
#' @export
#'

setClass("Indv",
         slots=c(nChr="numeric",
                 chromlist="list",phenolist="list")) -> Indv

setValidity("Indv",function(object){
  errors=character()
  if(object@nChr != length(object@chromlist)){
    errors=c(errors,"object@nChr != length(object@chromlist)")
  }
  if(length(errors)==0){
    return(TRUE)
  }
  else{
    return(errors)
  }
})

#' @describeIn  Indv Extract Chrom by index
setMethod("[",
          signature(x="Indv"),
          function(x,i){
            if(any(abs(i)>x@nChr)){
              stop("Trying to select invalid chromosomes")
            }
            return(x@chromlist[[i]])
        }
)
