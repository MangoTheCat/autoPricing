#' Master function to find out if the variable is significant when you add a variable
#'
#' @param \code{theModel} this is the candidate model
#' @param \code{currVar} this is the current variable being investigated
#' @param \code{alg} this is the algoirthm being used either forward or backward
#' @return The model that will be used and whether the \code{currVar} was significant or not
#' @note This is an internal function and should not be used by the user
#'
#' @keywords glm forward backward
#' @include autoPricing-package.R
#' @seealso \code{\link{glm}}
#' @seealso \code{\link{stepIC}}
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#
.forwardBackSig <- function(theModel, currVar, alg = "forward", ...){
  #
  myWeights <- get("myWeights", envir = funcEnv)
  factLevels <- get("factLevels", envir = funcEnv)
  theData <- get("theData", envir = funcEnv)
  myK <- get("myK", envir = funcEnv)
  handicap <- get("handicap", envir = funcEnv)
  plotCharts <- get("plotCharts", envir = funcEnv)
  theFormula <- theModel$formula
  #
  if(alg == "forward"){
  glm1 <- update(theModel, as.formula(paste("~.+", currVar, sep = "")))
  glm2 <- theModel
  }
  #
  if(alg == "backward"){
  glm1 <- theModel
  glm2 <- update(theModel, as.formula(paste("~.-", currVar, sep = "")))
  }
  
  
  #
  # This is the important part where it decided what happens to 
  #         the variable based on its information criterion
  #
	factorChanged <- FALSE
	assign("factorChanged", factorChanged, envir = funcEnv)
	bSelType <- get("selType", envir = funcEnv)
  if(.getVarSignificance(aGlm1 = glm1, aGlm2 = glm2, selType = bSelType)){
    cat(paste("\nVariable ", currVar, " is significant \n\n", sep = ""))

    return(list(glm1, "Significant"))
    }else{
      if(ncol(factLevels[[currVar]]) > 1){
      
	consolidTable <- factLevels[[currVar]]
	
    #Extract Significance Test for factor level of variable concerned
    variableComparison <- summary(glht(glm1, linfct = eval(parse(
          text = paste("mcp(", currVar, ' = "Tukey")', sep = ""))) ))$test
    compNames <- names(variableComparison$coefficients)
    variableComparison <- data.frame("Comparison" = compNames, "pvalues" = as.vector(variableComparison$pvalue))
    #
    cat("Output of summary(glht(glm, variable = 'Tukey')) in the {multcomp} package \n")
    print(variableComparison)
    cat("\nThis is the mapping table to be used to aggregate the factor levels before retesting the glm \n")
    print(consolidTable)
    poorVars <- as.character(variableComparison[variableComparison$pvalues > 0.05,]$Comparison)
    poorVars <- strsplit(poorVars, " - ")
    #
    factorChanged <- FALSE
    tempData <- get("tempData", envir = funcEnv)
    for(i in seq(along = poorVars)){
    poorVals <- consolidTable[which(consolidTable[,1] %in% poorVars[[i]]), ][,2]
    #
    if(all(poorVals == poorVals[1])){
      levels(tempData[,currVar])[which(levels(tempData[,currVar]) %in% poorVars[[i]])] <- poorVals[1]
      cat(paste("\naltered levels ... ", paste(levels(tempData[,currVar]), collapse = ", "), "\n" ) )
      factorChanged <- TRUE
      }else{}
    }
    cat(paste("\nFactor has changed?  ... ", factorChanged, "\n\n", sep = ""))
    if(factorChanged){
      glm1 <- glm(glm1$formula, data = tempData, family = modelFamily, offset = log(Exposure), weights = myWeights)
	
      if( .getVarSignificance(aGlm1 = glm1, aGlm2 = glm2, selType = bSelType) ){

		cat("Alternative form of the variable ", currVar, " is significant\n")
        assign("tempData", tempData, envir = funcEnv)
		assign("factorChanged", factorChanged, envir = funcEnv)
        return(list(glm1, "Significant"))
      }else{
        if(plotCharts){
        .plotRatingFactor(ratingFactor = as.character(currVar), theCoefficients = 
                                .getCoeffs(myExposure = "Exposure", myGlm = glm1, aCurrVar = as.character(currVar)))
        }
        cat(paste("Variable ", currVar, " is not significant", "\n\n\n", sep = ""))
		assign("factorChanged", factorChanged, envir = funcEnv)
        return(list(glm2, "Not Significant"))}
      
    }else{
      if(plotCharts){
      .plotRatingFactor(ratingFactor = as.character(currVar), theCoefficients = 
                                .getCoeffs(myExposure = "Exposure", myGlm = glm1, aCurrVar = as.character(currVar)))
      }
      cat(paste("Variable ", currVar, " is not significant \n\n\n", sep = ""))
	  assign("factorChanged", factorChanged, envir = funcEnv)
        return(list(glm2, "Not Significant"))}
    
    }else{
      if(plotCharts){
      .plotRatingFactor(ratingFactor = as.character(currVar), theCoefficients = 
                                .getCoeffs(myExposure = "Exposure", myGlm = glm1, aCurrVar = as.character(currVar)))
      }
      cat(paste("No further aggregation can be carried out since mapping table is not specified so variable ", 
                  currVar, " is not significant \n\n\n", sep = ""))
 		assign("factorChanged", factorChanged, envir = funcEnv)
        return(list(glm2, "Not Significant"))
    }
    }
  
}

