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
         slots=list(label="character",
                    value="numeric",
                    category="character"
         )) -> Phenotype

setValidity("Phenotype",function(object){
  errors=character()
  if(is.null(object@value) & is.null(object@category)){
    errors=c(errors,"No value or category provided for phenotype")
  }
  if(length(errors)==0){
    return(TRUE)
  }
  else{
    return(errors)
  }
})


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
#'
#'
#' @export
#'

setClass("Indv",
         slots=c(nChr="numeric",
                 chromlist="list")) -> Indv

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

# IndvPheno ------------------------------------------------------------------

#' @title IndvPheno
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
#' @slot pheno Phenotype object (optional)
#'
#'
#' @export
#'
setClass(
  "IndvPheno",
  contains="Indv",
  slots=c(pheno="Phenotype")
) -> IndvPheno

#' @describeIn  IndvPheno Extract Chrom by index
setMethod("[",
          signature(x="IndvPheno"),
          function(x,i){
            if(any(abs(i)>x@nChr)){
              stop("Trying to select invalid chromosomes")
            }
            return(x@chromlist[[i]])
          }
)

