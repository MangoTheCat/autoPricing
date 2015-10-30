\name{stepIC}
\alias{stepIC}
\title{Function for forward and backward model selection algorithm using information criteria to obtain frequency and severity models for...}
\usage{stepIC(ratingFact, countVar, sevVar, factLevels=1, timeVar,
    selType="BIC", consistThresh=60, theData, analysisType="frequency",
    myDistr="poisson", theLink="log", theAlg="forward",
    exposureName="Exposure", handicap=0, plotCharts=TRUE,
    myDocumentTitle="Automated Pricing GLM", ...)}
\description{Function for forward and backward model selection algorithm using information criteria to obtain frequency and severity models for 
actuarial pricing models the routine also dynamically adjusts the categories for variables that are not significant if an adequate 
mapping table is provided. This function is intended for analysing categorical explanatory varibles.}
\value{The output is the frequency or severity model from the chosen algorithm \code{theAlg}}
\note{please pay attention to the structure of the data requirements, since it is different from how 
actuarial data for GLM analysis is usually shaped and use the provided dataset "policyTable" as a guide
for how the data ashould be formatted.}
\keyword{glm}
\keyword{actuarial}
\keyword{pricing}
\keyword{automation}
\keyword{backward}
\keyword{forward}
\author{Chibisi Chima-Okereke \email{cchima-okereke@mango-solutions.com}}
\arguments{\item{\code{ratingFact}}{a character vector denoting the column headers of rating factors in your table (\code{theData}).
Please be aware that the package \code{autoPricing} does not currently support interaction terms in your model.}
\item{\code{countVar}}{a character string denoting the name of the claims count column}
\item{\code{sevVar}}{a character string denoting the column header of the severity variable}
\item{\code{factLevels}}{this is a list of matrices (or data.frames) that denote the mapping of the rating 
factors from their current categories to their logical aggregatable variables}
\item{\code{timeVar}}{a character denoting the column header of the year variable. This variable must be specified because
time consistency analysis is carried out by using interaction terms with the year variable.}
\item{\code{IC}}{a character string denoting whether the information criterion used should be "AIC" 
(Akaike Information Criterion) or "BIC" (Bayesian Information Criterion)}
\item{\code{consistThresh}}{Set this to the threshold for median consistency, denoting that the model fit of interaction
of the variables with Year is consistent to \code{consistThresh} or greater before it is accepted}
\item{\code{theData}}{this is the data set that will be used for the analysis containing the rating factors, exposure, 
severity, claim counts, and year}
\item{\code{analysisType}}{flag indicating whether the analysis is for a "frequency" or "severity" model}
\item{\code{myDistr}}{this is a character string denoting which distribution should be used in the analysis e.g. "poisson", "Gamma"}
\item{\code{theLink}}{this is the link function to be used in the analysis}
\item{\code{theAlg}}{this is the algorithm to be used either "forward" or "backward".}
\item{\code{exposureName}}{this is a character string denoting the column name of the exposure (in Years)}
\item{\code{handicap}}{extra penalty for the information criterion, it is added to the IC of the candiate 
model to alter how dificult it is to accept variables}
\item{\code{myDocumentTitle}}{This is a character string for the title of the document}
\item{\code{plotCharts}}{this is a logical variable as to whether the charts should be plotted}}
\examples{#Loading the data
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
dev.off()}
