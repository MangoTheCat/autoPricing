#' Function for formatting coefficients
#'
#' @param \code{myExposure} the name of the column of the dataset that has the exposure (in years)
#' @param \code{myGlm} the glm object
#' @param \code{theForm} should be the formula if theGlm$formula is NULL
#' @param \code{sourceData} should be the source data if theGlm$data is NULL
#' @param \code{aCurrVar} this is the current variable being analysed
#' @return The output is a nicely formatted table of the coefficients
#' @note This formula is used extensively in the \code{stepIC} function
#'
#' @keywords glm coefficients
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#
.getCoeffs <- function(myExposure = "Exposure", myGlm = theGlm, theForm = NULL, sourceData = NULL, aCurrVar){

	theData <- myGlm$data
	
	if(is.null(myGlm$formula)){myGlm$formula <- theForm}
	if(is.null(myGlm$data)){theData <- sourceData}
	
	
	myVars <- gsub(" ", "", unlist(strsplit(as.character(myGlm$formula)[3], "\\+")))
	
	getMyVars <- function(numVars = 1){
	  return(myVars[which(lapply(strsplit(myVars, ":"), length) == numVars)])  
	}
	
	singleVars <- getMyVars(numVars = 1)
	singleVars <- singleVars[singleVars != 1]
	singleVars <- singleVars[singleVars != 0]

#
	# Creating the newdata
	restOfFactors <- singleVars[singleVars != aCurrVar]
	newData <- lapply(restOfFactors, function(x){ levels(theData[,x])[1] })
	names(newData) <- restOfFactors
	newData[[aCurrVar]] <- levels(theData[, aCurrVar])
	newData <- data.frame(do.call(cbind, newData))
	newData[,myExposure] <- 1
	
	# These are the predicted Coefficients
	aCoeffs <- predict(myGlm, newdata = newData, type = "term", se.fit = TRUE)
	bCoeffs <- data.frame("Estimate" = aCoeffs$fit[, aCurrVar], "StdError" = aCoeffs$se.fit[, aCurrVar])
	# Re-basing the fitted Coefficient
	bCoeffs[,1] <- bCoeffs[, 1] - bCoeffs[1, 1]
	bCoeffs <- data.frame("Categories" = levels(theData[,aCurrVar]), bCoeffs)
	#names(bCoeffs)[1] <- "Categories"
	
	# This is to get the percentage exposure
	tabFormula <- as.formula(paste(myExposure, "~", aCurrVar))
	propExposure <- xtabs(tabFormula, data = theData[,c(aCurrVar, myExposure)])
	propExposure <- 100*propExposure/sum(propExposure)
	bCoeffs$PercentageExposure <- propExposure
	
	return(bCoeffs)

}
