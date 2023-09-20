# Phenotype ------------------------------------------------------------------

#' @title Phenotype
#'
#' @description
#' A phenotype possessed by an individual
#'  
#'  
#'
#' @param x a 'Phenotype'
#' @param ... additional 'Phenotype' objects
#'
#' @slot label phenotype label (character)
#' @slot value phenotype value if numeric (numeric)
#' @slot category phenotype value if categorical (character)
#'
#'
#' @export
#'

setClass("Phenotype",
         slots=list(phenotype="character",
                    value="numeric",
                    category="character",
                    ) -> Phenotype

setValidity("Phenotype",function(object){
  errors=character()
  #if(length(object@breakpoints) != length(object@donors)){
  #  errors=c(errors,"length(breakpoints) != length(donors)")
  #}
  if(length(errors)==0){
    return(TRUE)
  }
  else{
    return(errors)
  }
})
