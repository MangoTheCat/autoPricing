#' Function to create a formula which adds a variable to an input formula
#'
#'
#' @param \code{theFormula} this is the formula that the variable should be added to 
#' @param \code{addVar} this is the variable that you should add
#' @return The output is a formula object which is the input formula added to the variable
#' @note This should really be an internal function
#'
#' @keywords formula add
#' @include autoPricing-package.R
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#
.addVariable <- function(theFormula, addVar){
  theFormula <- as.character(theFormula)
  theRHS <- paste(c(theFormula[3], addVar), collapse = " + ", sep = "")
  theFormula <- as.formula(paste(theFormula[2], " ~ ", theRHS, sep = ""))
  return(theFormula)
}
