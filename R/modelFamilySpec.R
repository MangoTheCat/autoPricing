#' Function to specify the model family/link distributions/functions
#'
#'
#'
#' @param \code{distr} character string denoting the distribution to use options are "poisson", 
#'                    "binomial", "gaussian", "Gamma", "inverse.gaussian", "quasi", "quasibinomial", 
#'                     "quasipoisson", "negative.binomial"
#' @param \code{myLink} character string denoting the link function, e.g. "identity", 
#'                    "log", "logit", "inverse", "1/mu^2"
#' @param \code{theta} should be set to the value of theta if the "negative.binomal" distribution is selected
#' @return The output is an object of class "family"
#' @note This is just a wrapper function for the family() function
#'
#' @keywords addition arithmetic
#' @references
#' http://finzi.psych.upenn.edu/R/library/stats/html/family.html
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#' @examples
#' modelFamilySpec("poisson", "log")
#
.modelFamilySpec <- function(distr = "poisson", myLink = "log", theta = NULL){
  
  # theta is only specified for negative binomial distribution
  if(distr == "negative.binomial"){
    if(is.null(theta)){stop("Cannot proceed without specifying theta")}
    myTheta <- theta
    myFamily <- lapply(myTheta, distr, link = "log")[[1]]
    
  }else{
  
  myFamily <- lapply(myLink, distr)[[1]]
  }
  return(myFamily)
}
  
# Examples of specifications for models
#binomial(link = "logit")
#gaussian(link = "identity")
#Gamma(link = "inverse")
#inverse.gaussian(link = "1/mu^2")
#poisson(link = "log")
#quasi(link = "identity", variance = "constant")
#quasibinomial(link = "logit")
#quasipoisson(link = "log")
# This next one require MASS package
#negative.binomial(theta = stop("'theta' must be specified"), link = "log")
