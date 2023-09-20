#' Intialize a Phenotype object
#'
#'#' @param label phenotype label (character)
#' @param value phenotype value if numerical (numeric)
#' @param category phenotype value if categorical (character)

#'
#' @export


pheno_init<-function(label=NULL,value=NULL,category=NULL){
  pheno = new("Phenotype",label=label,value=value,category=category)
  return(pheno)
}
