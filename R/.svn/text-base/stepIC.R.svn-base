#'  Function for forward and backward model selection algorithm using information criteria to obtain frequency and severity models for 
#'  actuarial pricing models the routine also dynamically adjusts the categories for variables that are not significant if an adequate 
#'  mapping table is provided. This function is intended for analysing categorical explanatory varibles.
#'
#'
#' @param \code{ratingFact} a character vector denoting the column headers of rating factors in your table (\code{theData}).
#'                          Please be aware that the package \code{autoPricing} does not currently support interaction terms in your model.
#' @param \code{countVar} a character string denoting the name of the claims count column
#' @param \code{sevVar} a character string denoting the column header of the severity variable
#' @param \code{factLevels} this is a list of matrices (or data.frames) that denote the mapping of the rating 
#'                          factors from their current categories to their logical aggregatable variables 
#' @param \code{timeVar} a character denoting the column header of the year variable. This variable must be specified because
#'                          time consistency analysis is carried out by using interaction terms with the year variable.
#' @param \code{selType} a character string denoting whether the information criterion used to denote for model comparisons should be "AIC" 
#'                          (Akaike Information Criterion) or "BIC" (Bayesian Information Criterion). Other selection types are
#' 							available, "Chisq" for Chi-Squared, and "F" for F-Test but these are not recommended for multiple comparison reasons.
#' @param \code{consistThresh} Set this to the threshold for median consistency, denoting that the model fit of interaction
#'                            of the variables with Year is consistent to \code{consistThresh} or greater before it is accepted
#' @param \code{theData} this is the data set that will be used for the analysis containing the rating factors, exposure, 
#'                            severity, claim counts, and year
#' @param \code{analysisType} flag indicating whether the analysis is for a "frequency" or "severity" model
#' @param \code{myDistr} this is a character string denoting which distribution should be used in the analysis e.g. "poisson", "Gamma"
#' @param \code{theLink} this is the link function to be used in the analysis
#' @param \code{theAlg} this is the algorithm to be used either "forward" or "backward".
#' @param \code{exposureName} this is a character string denoting the column name of the exposure (in Years)
#' @param \code{handicap} extra penalty for the information criterion, it is added to the IC of the candiate 
#'                            model to alter how dificult it is to accept variables
#' @return The output is the frequency or severity model from the chosen algorithm \code{theAlg}
#' @param \code{myDocumentTitle} This is a character string for the title of the document
#' @param \code{plotCharts} this is a logical variable as to whether the charts should be plotted
#' @param \code{randomiseOrder} this is whether the order of the stepwise analysis should be randomised or not.
#' 							This feature may be useful to check the model which is selected if variables are
#' 							tested in a random randomised order
#' @param \code{justFit} Determines whether the automatic fitting process is used or whether the specified model is simply fitted
#' @note please pay attention to the structure of the data requirements, since it is different from how 
#'                           actuarial data for GLM analysis is usually shaped and use the provided dataset "policyTable" as a guide
#'                          for how the data ashould be formatted.
#'
#' @keywords glm actuarial pricing automation backward forward
#' @include autoPricing-package.R
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#' @examples
#'
#' #Loading the data
#' data(policyTable)
#'
#' #Preparing rating factor names and mapping tables 
#' myRatingFactors <- c("BonusMalus", "WeightClass", "Region", "Age", "Mileage", "Usage")
#' ratingFactorLevels <- lapply(myRatingFactors, function(x){matrix(as.character(levels(policyTable[,x])))})
#' names(ratingFactorLevels) <- myRatingFactors
#' ratingFactorLevels$Mileage <- cbind(ratingFactorLevels$Mileage, c("0-12500", "0-12500", "> 12500"))
#' ratingFactorLevels$BonusMalus <- cbind(ratingFactorLevels$BonusMalus, as.character(sort(rep(LETTERS[1:7], 2))))
#' weightClass <- c("650-935", "650-935", "650-935", "650-935", "1030-1315", "1030-1315", "1030-1315", "1030-1315", "1410-1600", "1410-1600", "1410-1600")
#' ratingFactorLevels$WeightClass <- cbind(ratingFactorLevels$WeightClass, weightClass)
#' 
#' #Example 1: Executing forward algorithm for poisson risk model
#' outputModelForwardFreq <- stepIC(ratingFact = myRatingFactors, countVar = "NoClaims", 
#'                   sevVar = "GrossIncurred", factLevels = ratingFactorLevels, timeVar = "Year", selType = "BIC", 
#'                   consistThresh = 60, theData = policyTable, analysisType = "frequency",
#'                   myDistr = "poisson", theLink = "log", exposureName = "Exposure",
#'                   handicap = 0, myDocumentTitle  = "Automated Pricing GLM", theAlg = "forward", 
#'                   plotCharts = TRUE)
#'
#'
#' # Example 2: Writing process to PDF and log file for documentation purposes
#' myFolder <- getwd()
#'
#' pdf(file = file.path(myFolder, "GLMOutput.pdf"), height = 7, width = 11)
#' par(mfrow = c(1,1), cex.main = 1, cex.axis = .9, cex.lab = 1, cex = 1)
#' sink(file = file.path(myFolder, "GLMOutput.doc"))
#' outputModelForwardFreq <- stepIC(ratingFact = myRatingFactors, countVar = "NoClaims", 
#'                   sevVar = "GrossIncurred", factLevels = ratingFactorLevels, timeVar = "Year", selType = "BIC", 
#'                   consistThresh = 60, theData = policyTable, analysisType = "frequency",
#'                   myDistr = "poisson", theLink = "log", exposureName = "Exposure",
#'                   handicap = 0, myDocumentTitle  = "Automated Pricing GLM", theAlg = "forward", 
#'                   plotCharts = TRUE)
#' sink()
#' dev.off()
#
stepIC <- function(ratingFact, countVar, 
                     sevVar, factLevels = 1, timeVar, selType = "BIC",
                     consistThresh = 60, theData, analysisType = "frequency",
                     myDistr = "poisson", theLink = "log", theAlg = "forward", 
                     exposureName = "Exposure", handicap = 0, 
                     plotCharts = TRUE, myDocumentTitle  = "Automated Pricing GLM",
					 randomiseOrder = FALSE, justFit = FALSE, ...){

	# Specifying the distribution and link functions
	assign("modelFamily", .modelFamilySpec(distr = myDistr, myLink = theLink, theta = NULL), envir = .GlobalEnv)
				 
	# Aggregating the data ...
  #
  factorList <- lapply(c(timeVar, ratingFact), function(x){y <- theData[,x];return(y)})
  names(factorList) <- c(timeVar, ratingFact)
  theData <- aggregate(theData[,c(countVar, sevVar, exposureName)], by = factorList, FUN = sum)
  names(theData)[names(theData) == exposureName] <- c("Exposure")
  #
  # reformating the data for severity models
  if(analysisType == "severity"){
    theData <- theData[theData[,sevVar] > 0, ]
    averageClaim <- theData[,sevVar]/theData[,countVar]
    theData$averageClaim <- averageClaim
    depVar <- "averageClaim"
    myWeights <- theData[,countVar]
  }
  if(analysisType == "frequency"){
    depVar <- countVar
    myWeights <- rep(1, nrow(theData))
  }
  #
  # assigning data into protected Environment
  assign("funcEnv", new.env(), envir = .GlobalEnv)
  assign("theData", theData, envir = funcEnv)
  assign("tempData", theData, envir = funcEnv)
  assign("timeVar", timeVar, envir = funcEnv)
  assign("handicap", handicap, envir = funcEnv)
  assign("myWeights", myWeights, envir = funcEnv)
  assign("selType", selType, envir = funcEnv)
  #assign("exposure", theData[,exposureName], envir = funcEnv)
  assign("exposureName", exposureName, envir = funcEnv)
  assign("plotCharts", plotCharts, envir = funcEnv)
  assign("consistThresh", consistThresh, envir = funcEnv)
  #

# NOTE WELL, this is a primary jumping off point, it divides the path of the analysis into auto-fit and simple model fit

	## This is the if statement that determines whether the model will go throught the auto-fit selection
	#				or whether it will simply just fit the model engaged with the parameter "justFit". 
	if(justFit == TRUE){
		cat("The justFit parameter has been set to ", justFit, "therefore the auto-fitting process will not be carried out and the specified model will be fully fitted.\n")
		finalModel <- glm(formula(paste(depVar, " ~ ", paste(c(timeVar, ratingFact), collapse = " + "), sep = "")), data = theData, 
				family = modelFamily, offset = log(Exposure), weights = get("myWeights", envir = funcEnv))
	}else{ 
		
cat("\n", selType, " will be used as model selection criterion in this investigation.\n")
if(!(selType == "AIC" | selType == "BIC")){
	cat("Warning: Information criteria based techniques are not being used for this stepwise model selection process. This is not recommended.\n\n")
}

  # Document Title:
  #
  myDocumentTitle <- paste(myDocumentTitle, " for ", analysisType, " \n(Model Distr: ", myDistr, 
                           ", Link: ", theLink, ")", "\nMethod = ", theAlg, sep = "")
  
  cat("************************************************************************************************************\n")
  cat(paste("This analysis is a ",  analysisType, " analysis using a ", myDistr, " distribution and a ", theLink, " link", 
              ", for the ", theAlg, " algorithm \n", sep = ""))
  cat("************************************************************************************************************\n\n")
  #print("")
  #print("")
  # If you do not have the correct number of factor Levels
  if(length(ratingFact) != length(ratingFactorLevels)){
    factLevels <- as.list(rep(1, length(ratingFact)))
    cat("Warning: factors levels spec have been reset since they have the wrong number of levels \n")
    }
  assign("factLevels", factLevels, envir = funcEnv)
  origData <- theData 
  
  if(theAlg == "forward"){
  prevForms <- paste(depVar, " ~ ", paste(c(timeVar, ratingFact), collapse = " + ", sep = ""), sep = "")
  prevModel <- glm(formula(paste(depVar, " ~ ", timeVar, sep = "")), data = theData, 
                                        family = modelFamily, offset = log(Exposure), weights = myWeights)
  }
  if(theAlg == "backward"){
  prevForms <- paste(depVar, " ~ ", paste(c(timeVar, ratingFact), collapse = " + ", sep = ""), sep = "")
  prevModel <- glm(formula(prevForms), data = theData, 
                                        family = modelFamily, offset = log(Exposure), weights = myWeights)
  }
  
  #if(selType == "AIC"){myK <- 2}
  if(selType == "BIC"){myK <- log(nrow(theData))}else{myK <- 2}
  
  assign("myK", myK, envir = funcEnv)
  # One way to order variables
  cat("Beginning one-way analyses to determine order that variables will be analysed \n")
  cat("********************************************************************************\n\n")
  
  aBaseModel <- glm(formula(paste(depVar, " ~ ", timeVar, sep = "")), data = theData, 
		  family = modelFamily, offset = log(Exposure), weights = get("myWeights", envir = funcEnv))
  
  theIC <- lapply(ratingFact, function(x){
			  aModel <- update(aBaseModel, as.formula(paste("~.+", x, sep = "")))
			  output <- data.frame("RatingFactor" = x, "IC" = extractAIC(aModel, k = myK)[2])
			  cat(paste(paste(as.character(aModel$formula)[c(2,1,3)], collapse = " "), "\n", sep = ""))
			  return(output)})
  ICTable <- do.call(rbind, theIC)
  
	if(!randomiseOrder){
  if(theAlg == "forward"){ICTable <- ICTable[order(ICTable$IC, decreasing = FALSE),]}
  if(theAlg == "backward"){ICTable <- ICTable[order(ICTable$IC, decreasing = TRUE),]}}else{
  icOrder <- sample(rownames(ICTable), size = length(rownames(ICTable)))
  ICTable <- ICTable[icOrder,]
	}

  ratingFact <- as.character(ICTable$RatingFactor)
  rownames(ICTable) <- 1:nrow(ICTable)
  cat("\nAnalysis order ...\n")
  print(ICTable)
  #
  # Plotting Model Details
  if(plotCharts){
  grid.arrange(
  tableGrob(data.frame(myDocumentTitle), show.rownames = FALSE, show.colnames = FALSE, gpar.corefill = gpar(fill="white", 
                                          col = NA, row = NA), gpar.coretext=gpar(col = "black", cex = 3)),
  tableGrob(ICTable,
  show.csep=TRUE, show.rsep=TRUE, show.box=TRUE, separator="grey", name="test", gp=gpar(fontsize=12, lwd=2), 
            equal.width=FALSE, grep=TRUE, global=TRUE),
  nrow = 2)
  }
  #
  myReturn <- TRUE

  loopCount <- 1
  
  # while loop
  while(length(ratingFact) > 0){

  currentVar <- ratingFact[1]
  
  cat(paste("\nAnalysing the rating factor ... ", currentVar, " \n\n", sep = ""))

  # Find out if the variable is significant:
  
  currentModel <- .forwardBackSig(theModel = prevModel, currVar = currentVar, alg = theAlg)
  theData <- get("tempData", envir = funcEnv)
  assign("theData", get("tempData", envir = funcEnv), envir = funcEnv)
  if(currentModel[[2]] == "Significant"){
    currentModel <- currentModel[[1]]
    ICPass <- TRUE
    ratingFact <- ratingFact[currentVar != ratingFact]
    #assign("ratingFact", ratingFact, envir = funcEnv)
  }else{
    currentModel <- currentModel[[1]]
    ICPass <- FALSE
    ratingFact <- ratingFact[currentVar != ratingFact]
    #assign("ratingFact", ratingFact, envir = funcEnv)
  }
  
  if(ICPass){
  currentModelTime <- formula(paste(paste(as.character(currentModel$formula)[c(2,1,3)], collapse = ""), 
                                    " + ", paste(timeVar, ":", currentVar, sep = ""), sep = ""))
############################################################################################
  
  # Plotting Factor
  myCoeffs <- data.frame(summary(currentModel)$coeff)
  names(myCoeffs) <- c("Estimate", "StdError", "ZValue","P(Z > |z|)")
  if(plotCharts){
  .plotRatingFactor(ratingFactor = as.character(currentVar), theCoefficients = 
                                .getCoeffs(myExposure = exposureName, myGlm = currentModel, 
										aCurrVar = as.character(currentVar)))
  }
  consistOut <- .plotTimeConsist(ratingFactor = as.character(currentVar), timeDef = timeVar, 
                                theGlm = currentModelTime, constThresh = consistThresh, expVar = exposureName,
                                showPlots = plotCharts)
  consistency <- (consistOut >= consistThresh)
  factorChanged <- get("factorChanged", envir = funcEnv)
  
  # This is run if rating factor is significant, not consistent, 
	# and not changed and there is a mapping table. We already know that it is significant
  if(!consistency & !factorChanged & ncol(factLevels[[currentVar]]) > 1){
	  
	  aConsistencyTest <- .consistCorrection(glm1 = currentModel, currVar = currentVar)
	  theData <- get("tempData", envir = funcEnv)
	  cat("\n########################################################\n")
	  cat("End of SBNC Analysis\n")
	  currentModel <- aConsistencyTest[[1]]
	  if(aConsistencyTest[[2]] == "Consistent"){consistency <- TRUE}else{consistency <- FALSE}
	  myCoeffs <- data.frame(summary(currentModel)$coeff)
	  names(myCoeffs) <- c("Estimate", "StdError", "ZValue","P(Z > |z|)")
  }
  
  
  if(ICPass & consistency){
    cat("Model is consistent and has improved IC\n")
    prevModel <- currentModel
  }else{
    cat(paste("Information Criteria pass: ", ICPass, " & consistency: ", consistency, "\n", sep = ""))
  }#End Consistency Pass
  
  }else{
    cat("Current variable has failed so final model is sub model\n")
    prevModel <- currentModel
  }

  #print("")
  cat(paste("\nThe loop count is ... ", loopCount, "\n\n", sep = ""))
  cat(paste("Finishing variable ... ", currentVar, "\n", sep = ""))
  #print("")
  loopCount <- loopCount + 1
  }# end while
#
  # Final check to see if all the variable are significant
  cat(paste("Carrying out the final checks, the IC for the candidate model is ", 
              round(extractAIC(prevModel, k = myK)[2]),", the full candidate formula is ...\n", sep = ""))
  finalRatingFactors <- gsub(" ", "", strsplit(paste(prevModel$formula)[3], "\\+")[[1]][-1])
  cat(paste(as.character(prevModel$formula)[c(2,1,3)], collapse = " "), "\n")
  #
  cat("The sub models are\n\n")
  
  cSelType <- selType
  for(i in finalRatingFactors){
	  cat("Testing rating factor ", i, " .....\n")
	  cat("#########################################\n")
	  subModel <- update(prevModel, as.formula(paste("~.-", i, sep = "")))
	  if(!.getVarSignificance(aGlm1 = prevModel, aGlm2 = subModel, selType = cSelType)){
		  prevModel <- subModel
		  cat("Variable ", i, " is not significant in the final check so it has been eliminated\n")
		  cat("####################################################################################\n\n")
	  }else{
		  cat("Variable ", i, " is significant in the final check so it will be included in the model\n")
		  cat("##########################################################################################\n\n")
	  }
	  
  }
  
  finalModel <- prevModel
  
  #############################################################################
#
cat("\nThe final model summary follows ...\n")
cat("************************************************************************************************\n")
print(summary(finalModel)$coeff)
#
#print("")
#print("")
# Do a complete dump of diagnostics here, AIC, BIC, Chisq, and F if Type = severity
cat("\n\n*********************************************************************************************************\n")
cat("The final valid model is ... ", as.character(finalModel$formula)[c(2,1,3)], "\n")
cat("*********************************************************************************************************\n")

cat("AIC for the final model is ", round(AIC(finalModel)), "\n")
cat("BIC for the final model is ", round(BIC(finalModel)), "\n")
cat("Chi-Squared ANOVA for the final model is ... \n")
print(anova(finalModel, test = "Chisq"))

# This is a nicer coefficient table than the default
raingFactorList <- gsub(" ", "", strsplit(as.character(finalModel$formula)[3], split = "\\+")[[1]])
coeffsOut <- lapply(raingFactorList, function(x){data.frame("Factor" = x, .getCoeffs(myExposure = "Exposure", myGlm = finalModel, aCurrVar = x))})
coeffsOut <- do.call(rbind, coeffsOut)
rownames(coeffsOut) <- 1:nrow(coeffsOut)
assign("coeffsOut", coeffsOut, envir = funcEnv)
cat("\nOutputting print friendly model output table\n\n")
print(coeffsOut)

cat("\n***********************************************************************************************\n")
cat("End of Model Summary\n")
cat("***********************************************************************************************\n")
#print("")
#
finalModelTable <- data.frame("Formula" = paste(as.character(finalModel$formula)[c(2,1,3)], collapse = " "), 
		"AIC" = AIC(finalModel), "BIC" = BIC(finalModel))
#
finalTitle <- paste("Final Model for ",  analysisType, " analysis \nusing a ", myDistr, " distribution\n and a ", 
                    theLink, " link for the ", theAlg, " algorithm", sep = "")
if(plotCharts){
grid.arrange(
tableGrob(data.frame(finalTitle), show.rownames = FALSE, show.colnames = FALSE, gpar.corefill = gpar(fill="white", 
                                          col = NA, row = NA), gpar.coretext=gpar(col = "black", cex = 3)),
tableGrob(finalModelTable, show.rownames = FALSE, show.csep=TRUE, show.rsep=TRUE, 
show.box=TRUE, separator="grey", name="test", gp=gpar(fontsize=12, lwd=2), equal.width=FALSE, grep=TRUE, global=TRUE),
nrow = 2
)
}
} # end of justFit it else
# Final Coefficient Table
return(finalModel)
rm(list = ls());gc()
}
