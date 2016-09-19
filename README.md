---------------------------------------------------------------------------------------
Date: 12/01/2012
Author: Chibisi Chima-Okereke
Package: autoPricing
Version: 0.4
#
This is the first iteration of the package
---------------------------------------------------------------------------------------
```
#Loading the data
data(policyTable)

#Preparing rating factor names and mapping tables 
myRatingFactors <- c("BonusMalus", "WeightClass", "Region", "Age", "Mileage", "Usage")
ratingFactorLevels <- lapply(myRatingFactors, function(x){matrix(as.character(levels(policyTable[,x])))})
names(ratingFactorLevels) <- myRatingFactors
ratingFactorLevels$Mileage <- cbind(ratingFactorLevels$Mileage, c("0-12500", "0-12500", "> 12500"))
ratingFactorLevels$BonusMalus <- cbind(ratingFactorLevels$BonusMalus, as.character(sort(rep(LETTERS[1:7], 2))))
weightClass <- c("650-935", "650-935", "650-935", "650-935", "1030-1315", "1030-1315", "1030-1315", "1030-1315", "1410-1600", "1410-1600", "1410-1600")
ratingFactorLevels$WeightClass <- cbind(ratingFactorLevels$WeightClass, weightClass)

#Example 1: Executing forward algorithm for poisson risk model
outputModelForwardFreq <- stepIC(ratingFact = myRatingFactors, countVar = "NoClaims", 
                  sevVar = "GrossIncurred", factLevels = ratingFactorLevels, timeVar = "Year", selType = "BIC", 
                  consistThresh = 60, theData = policyTable, analysisType = "frequency",
                  myDistr = "poisson", theLink = "log", exposureName = "Exposure",
                  handicap = 0, myDocumentTitle  = "Automated Pricing GLM", theAlg = "forward", 
                  plotCharts = TRUE)
# Example 2: Writing process to PDF and log file for documentation purposes
myFolder <- getwd()
pdf(file = file.path(myFolder, "GLMOutput.pdf"), height = 7, width = 11)
par(mfrow = c(1,1), cex.main = 1, cex.axis = .9, cex.lab = 1, cex = 1)
sink(file = file.path(myFolder, "GLMOutput.doc"))
outputModelForwardFreq <- stepIC(ratingFact = myRatingFactors, countVar = "NoClaims", 
                  sevVar = "GrossIncurred", factLevels = ratingFactorLevels, timeVar = "Year", selType = "BIC", 
                  consistThresh = 60, theData = policyTable, analysisType = "frequency",
                  myDistr = "poisson", theLink = "log", exposureName = "Exposure",
                  handicap = 0, myDocumentTitle  = "Automated Pricing GLM", theAlg = "forward", 
                  plotCharts = TRUE)
sink()
dev.off()
```