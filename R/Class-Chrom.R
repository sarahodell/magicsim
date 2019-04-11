# Chrom ------------------------------------------------------------------

#' @title Chrom
#'
#' @description
#' The chromosome class contains information on
#'  chromosome breakpoints and donors for
#'  for one individual chromosome.
#'
#' @param x a 'Chrom'
#' @param ... additional 'Chrom' objects
#'
#' @slot chr chromosome number
#' @slot breakpoints vector of physical positions of past recombination events
#' @slot donors vector of parental donor names for positions between breakpoints
#' @slot xo_no integer of number of breakpoints on the chromosome
#'
#'
#' @export
#'

setClass("Chrom",
         slots=list(chr="numeric",
                    breakpoints="numeric",
                    donors="character",
                    xo_no="numeric")) -> Chrom

setValidity("Chrom",function(object){
  errors=character()
  if(length(object@breakpoints) != length(object@donors)){
    errors=c(errors,"length(breakpoints) != length(donors)")
  }
  if(length(errors)==0){
    return(TRUE)
  }
  else{
    return(errors)
  }
})


# ChromPair ------------------------------------------------------------------

#' @title ChromPair
#'
#' @description
#' The chromosome class contains information on
#'  chromosome breakpoints and donors for
#'  for one individual chromosome.
#'
#' @param x a 'ChromPair'
#' @param ... additional 'ChromPair' objects
#'
#' @slot chr chromosome number
#' @slot h1 vector of physical positions of past recombination events
#' @slot h2 vector of parental donor names for positions between breakpoints
#'
#'
#' @export
#'

setClass("ChromPair",
         slots=list(chr="numeric",
                    h1="Chrom",
                    h2="Chrom")) -> ChromPair

setValidity("ChromPair",function(object){
  errors=character()
  if(object@h1@chr != object@h2@chr){
    errors=c(errors,"object@h1@chr) != object@h2@chr")
  }
  if(object@h1@chr != object@chr){
  errors=c(errors,"object@h1@chr) != object@chr")
  }
  if(length(errors)==0){
    return(TRUE)
  }
  else{
    return(errors)
  }
})

