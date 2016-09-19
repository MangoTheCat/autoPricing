# autoPricing

> The purpose of this package is to carry out automated GLM
    analysis for actuarial pricing. The idea is that it uses the
    forward or backward algorithms and information criteria to obtain a
    frequency and severity model. It allows tables to be specified that
    map the aggregation structure of the variable from those currently
    specified to a less granular set of categories. This allows those
    explanatory variables to be dynamically re-specified during the
    pricing process if the native variable does not improve the fit of
    the model. The re-specification of the variable categories is done
    if the native variable does not decrease the information criterion;
    the mapping table and a Tukey test on the categorical variable is
    used to aggregate categories that are statistically and logically
    similar.
    
## Installation
Installation from github requires the devtools package to be installed.

## Usage

```R
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

## Licence
GPL 2 © [Mango Solutions](https://github.com/mangothecat)