#' This function rebalances a raw variable that is significant but not consistent
#'
#' @param \code{glm1} the glm that contains the rating factor to be analysed output from the \code{.forwardBackSig} function
#' @param \code{currVar} the rating factor to be analysed 
#' @return The output is a list of revised glm and a character either \code{"Consistent"} or \code{"Not Consistent"}
#' @note This is an internal function
#'
#' @keywords glm consistency significance
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#
.consistCorrection <- function(glm1 = currentModel, currVar = currentVar){
	
	cat("Significant But Not Consistent Analysis (SBNC) Analysis:\n")
	cat("########################################################\n\n")
	cat("This part of the analysis is taking place because the variable ", currVar, " is significant but not consistent ...\n")
	cat("... so the variable is being rebalanced to see if it will be consistent\n\n")
	tempData <- get("theData", envir = funcEnv)
	myWeights <- get("myWeights", envir = funcEnv)
	factLevels <- get("factLevels", envir = funcEnv)
	timeVar <- get("timeVar", envir = funcEnv)
	exposureName <- get("exposureName", envir = funcEnv)
	consistThresh <- get("consistThresh", envir = funcEnv)
	
	glm2 <- update(theModel, as.formula(paste("~.-", currVar, sep = "")))
	
	#myForm2 <- as.character(glm2$formula)[c(2,1,3)]
	#ratingFactors <- strsplit(myForm2[3], " \\+ ")[[1]]
	#myForm2 <- paste(paste(myForm2[1:2], collapse = " "), paste(ratingFactors[ratingFactors != currVar], collapse = " + "))
	#glm2 <- glm(formula(myForm2), data = tempData, family = glm2$family, offset = log(Exposure), weights = myWeights)
	#
	#####################################
	
		consolidTable <- factLevels[[currVar]]
		#Extract Significance Test for factor level of variable concerned
		variableComparison <- summary(glht(glm1, linfct = eval(parse(
										text = paste("mcp(", currVar, ' = "Tukey")', sep = ""))) ))$test
		compNames <- names(variableComparison$coefficients)
		variableComparison <- data.frame("Comparison" = compNames, "pvalues" = as.vector(variableComparison$pvalue))
		#
		cat("Output of summary(glht(glm, variable = 'Tukey')) in the {multcomp} package\n")
		print(variableComparison)
		cat("\nThis is the mapping table to be used to aggregate the factor levels before retesting the glm\n")
		print(consolidTable)
		poorVars <- as.character(variableComparison[variableComparison$pvalues > 0.05,]$Comparison)
		poorVars <- strsplit(poorVars, " - ")
		#
		factorChanged <- FALSE
		for(i in seq(along = poorVars)){
			poorVals <- consolidTable[which(consolidTable[,1] %in% poorVars[[i]]), ][,2]
			#
			if(all(poorVals == poorVals[1])){
				levels(tempData[,currVar])[which(levels(tempData[,currVar]) %in% poorVars[[i]])] <- poorVals[1]
				cat("altered levels ... ", paste(levels(tempData[,currVar]), collapse = ", "), "\n" )
				factorChanged <- TRUE
			}else{}
		}#end for
		cat(paste("\n\nFactor has changed?  ... ", factorChanged, "\n", sep = ""))
		if(factorChanged){
			glm1 <- glm(glm1$formula, data = tempData, family = modelFamily, offset = log(Exposure), weights = myWeights)
			#
			#print(paste("Extended Model IC ", round(extractAIC(glm1, k = myK)[2]), " sub model ", 
		    #	round(extractAIC(glm2, k = myK)[2]), sep = ""))
			bSelType <- get("selType", envir = funcEnv)
			#aicBetter <- (round(extractAIC(glm1, k = myK)[2]) + handicap < round(extractAIC(glm2, k = myK)[2]))
			if( .getVarSignificance(aGlm1 = glm1, aGlm2 = glm2, selType = bSelType) ){
				cat(paste("An altered form of variable ", currVar, " is significant with IC: ", "\n", sep = ""))
				assign("tempData", tempData, envir = funcEnv)
				assign("factorChanged", factorChanged, envir = funcEnv)
				.plotRatingFactor(ratingFactor = as.character(currVar), theCoefficients = 
								.getCoeffs(myExposure = "Exposure", myGlm = glm1))
				currentModelTime <- formula(paste(paste(as.character(glm1$formula)[c(2,1,3)], collapse = ""), 
								" + ", paste(timeVar, ":", currVar, sep = ""), sep = ""))
				consistOut <- .plotTimeConsist(ratingFactor = as.character(currVar), timeDef = timeVar, 
						theGlm = currentModelTime, constThresh = consistThresh, expVar = exposureName,
						showPlots = TRUE)
				if(consistOut > consistThresh){
					cat(paste("Atered variable ", currVar, " is significant and consistent", "\n\n", sep = ""))
					return(list(glm1, "Consistent"))}else{
					cat(paste("Atered variable ", currVar, " is significant but not consistent", "\n\n", sep = ""))
					return(list(glm2, "Not Consistent"))}
			}else{
				if(plotCharts){
					.plotRatingFactor(ratingFactor = as.character(currVar), theCoefficients = 
									.getCoeffs(myExposure = "Exposure", myGlm = glm1))
				}
				cat(paste("Atered initial significant variable ", currVar, " is not significant -- odd!!", "\n\n", sep = ""))
				assign("factorChanged", factorChanged, envir = funcEnv)
				return(list(glm2, "Not Consistent"))}
			
		}else{
			if(plotCharts){
				.plotRatingFactor(ratingFactor = as.character(currVar), theCoefficients = 
								.getCoeffs(myExposure = "Exposure", myGlm = glm1))
			}
			cat(paste("Variable ", currVar, " is not changed therefore not consistent", "\n\n", sep = ""))#
			assign("factorChanged", factorChanged, envir = funcEnv)
			return(list(glm2, "Not Consistent"))
		}
	
}
