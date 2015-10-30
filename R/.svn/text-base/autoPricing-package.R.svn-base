#' This package is for carrying out automated glm pricing analysis for frequency and severity 
#'
#' \tabular{ll}{
#' Package: \tab autoPricing\cr
#' Type: \tab Package\cr
#' Version: \tab 0.4\cr
#' Date: \tab 2011-01-12\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' The purpose of this package is to carry out automated GLM analysis for actuarial 
#' pricing. The idea is that it uses the forward or backward algorithms and information criteria to 
#' obtain a frequency and severity model. It allows tables to be specified that map the aggregation 
#' structure of the variable from those currently specified to a less granular set of categories. 
#' This allows those explanatory variables to be dynamically re-specified during the pricing process 
#' if the native variable does not improve the fit of the model. The re-specification of the 
#' variable categories is done if the native variable does not decrease the information criterion; 
#' the mapping table and a Tukey test on the categorical variable is used to aggregate categories 
#' that are statistically and logically similar.
#'
#'
#' The \code{\link{stepIC}} function is the main function for this package, it carrys out the 
#'      step information criterion process.
#'
#'
#' @name autoPricing-package
#' @aliases autoPricing
#' @docType package
#' @title This package is for carrying out automated glm pricing analysis for frequency and severity 
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#' @keywords glm, actuarial pricing, automating
#' @include addVariable.R
#' @include consistCorrection.R
#' @include policyTable.R
#' @include forwardBackSig.R
#' @include getCoeffs.R
#' @include getVarSignificance.R
#' @include modelFamilySpec.R
#' @include plotRatingFactor.R
#' @include plotTimeConsist.R
#' @include stepIC.R
#' @seealso \code{\link{stepIC}}
#'
