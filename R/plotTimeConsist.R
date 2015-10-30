#' Function for plotting consistency
#'
#' @param \code{ratingFactor} a character string denoting the rating factor to carry out time consistency analysis
#' @param \code{timeDef} a character string denoting the Year variable
#' @param \code{theGlm} a glm object to be analysed
#' @param \code{constThresh} a numeric denoting the threshold percentage of consistency
#' @param \code{expVar} set this to the exposure column name
#' @param \code{showPlots} logical for whether to plot the variables or not
#' @return The output is a numeric denoting the median consistency of the rating factor with year
#' @note ...
#' @seealso \code{\link{stepIC}} 
#'
#' @keywords consistency glm time
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#
.plotTimeConsist <- function(ratingFactor = "BonusMalus", timeDef = "Year", theGlm = myGlm, 
                                                          constThresh = 60, expVar = "Exposure", showPlots = TRUE){
#
exposureName <- get("exposureName", envir = funcEnv)
theData <- get("theData", envir = funcEnv)
names(theData)[names(theData) == exposureName] <- "Exposure"
myWeights <- get("myWeights", envir = funcEnv)
theGlm <- glm(theGlm, data = theData, family = get("modelFamily", envir = funcEnv), offset = log(Exposure), weights = myWeights)
myTerms <- gsub(" ", "", strsplit(as.character(theGlm$formula)[3], "\\+")[[1]])
myTerms <- unlist(lapply(myTerms, function(x){x <- strsplit(x, ":")[[1]]; if(length(x) > 1){return(NULL)}else{return(x)}}))
myTerms <- myTerms[!is.na(myTerms)]
myTerms <- myTerms[!(myTerms %in% c(timeDef, ratingFactor))]
newData <- expand.grid(timeDef = levels(theGlm$data[,get("timeDef")]), ratingFactor = levels(theGlm$data[,get("ratingFactor")]), expVar = 1)
names(newData) <- c(timeDef, ratingFactor, expVar)
#
if(length(myTerms) > 0){
myTerms <- lapply(myTerms, function(x){y <- data.frame(levels(theGlm$data[,x])[1]); names(y) <- x; return(y)})
myTerms <- do.call(cbind, myTerms)
#
newData <- data.frame(newData, myTerms)
}
myWeights <- rep(1, nrow(newData))
newData <- data.frame(newData, myWeights)
tempSum <- predict(theGlm, newdata = newData, type = "terms", se.fit = FALSE)
# Summing over the exposure year, rating factor, and year:rating factor interaction terms
tempSum <- tempSum[,c(timeDef, ratingFactor, paste(timeDef, ":", ratingFactor, sep = ""))]
#
newData <- newData[,c(timeDef, ratingFactor)]
newData$Coefficients <- apply(tempSum, 1, sum)
#
myFormula <- formula(paste("Coefficients", " ~ ", timeDef, " + ", ratingFactor, sep = ""))
testSum <- xtabs(myFormula, data = newData)
# Defining Colors
# EMB Yellow
embYellow <- c(250, 255, 102)/255
embYellow <- rgb(embYellow[1], embYellow[2], embYellow[3], alpha = 0.7)
#
myColors <- timPalette(length(levels(theGlm$data[,timeDef])))
myProps <- prop.table(xtabs(formula(paste(expVar, " ~ ", timeDef," + ", 
                                       ratingFactor, sep = "")), data = theGlm$data))*100
#
fullYRange <- c(0, max(2*apply(myProps, 2, sum))) 
yRange <- c(fullYRange[2]/2, fullYRange[2])
xRange <- range(testSum)
if(xRange[2] > 0){xRange[2] <- xRange[2]*1.2}else{xRange[2] <- xRange[2] - .2*xRange[2] }
#
convertScale <- function(conPoints = 1, parm2Perc = TRUE){
  
 myM <- diff(yRange)/diff(xRange)
 myC <- yRange[1] - myM*xRange[1]
 
 if(parm2Perc == TRUE){outPut <- myM*conPoints + myC}else{
   
   if(parm2Perc == FALSE){outPut <- (conPoints - myC)/myM}else{outPut <- NULL}
 }
return(outPut)
}
#
#
fullYRange <- c(fullYRange[1], 1.4*ceiling(fullYRange[2]))
plotCoeffs <- convertScale(testSum)
#
if(showPlots == TRUE){
par(mar=c(5,4,4,5) + .1)
xCoords <- barplot(myProps, col = myColors[1:nrow(myProps)], ylim = fullYRange, yaxt = "n", 
        main = paste("Consistency Plot for ", ratingFactor, " rating factor", sep = ""), legend = rownames(myProps))

for(i in 1:nrow(plotCoeffs)){
lines(xCoords, plotCoeffs[i,], lwd = 2, col = myColors[i])
points(xCoords, plotCoeffs[i,], cex = 1, lwd = 1.2, pch = 21, bg = myColors[i], col = "black")
#print(i)  
axis(4)
mtext("Exposure %", side = 4, line = 3)
axis(2, labels = c("", round(convertScale(axTicks(2), parm2Perc = FALSE), 1)[-1]), at = axTicks(2))
mtext(paste("Predicted Values for model including Year:", as.character(ratingFactor)," interaction", sep = ""), side = 2, line = 3)
}# end for
}#end show plot
# Calculating consistency
if(ncol(testSum) > 2){diffMatrix <- data.frame(t(apply(testSum, 1, diff)))}else{
  diffMatrix <- data.frame(apply(testSum, 1, diff))
}
names(diffMatrix) <- paste(colnames(testSum)[1:(length(colnames(testSum)) - 1)], colnames(testSum)[2:(length(colnames(testSum)))], sep = " - ")

myCondition <- function(x){x[which(x > 0)] <- 1
                           x[which(x < 0)] <- -1
                           x[which(x == 0)] <- 0
                           return(x)}

consistencyMatrix <- apply(diffMatrix, 2, myCondition)

myProps <- function(x){
percConsist <- max(data.frame(prop.table(table(x)))[,2])
#
return(percConsist)
}
#
myConsist <- apply(consistencyMatrix, 2, myProps)*100
totConsist <- round(median(myConsist))
myConsist <- round(myConsist)
#
if(showPlots == TRUE){
myXs <- barplot(myConsist, border = "NA", col = "white", ylim = c(0, 120),
                main = "Percentage consistency between adjacent levels", 
                ylab = "Percentage", xlab = "Comparison")
points(myConsist ~ myXs, cex = 1, lwd = 1.2, pch = 21, bg = myColors[1], col = "black")
lines(myConsist ~ myXs, cex = 1, lwd = 1.2, col = myColors[1])
abline(h = constThresh, lty = 2, col = "grey")
#
if(totConsist >= constThresh){text(x = 1, y = constThresh*1.05, labels = "           Consistency Pass")}else{
text(x = 1, y = constThresh*1.05, labels = "            Consistency Fail")}
axis(1, labels = FALSE, at = myXs)
box()
}#end if plot
myCoeffs <- data.frame(summary(theGlm)$coeff)
names(myCoeffs) <- c("Estimate", "StdError", "ZValue","P(Z > |z|)")
#
return(totConsist)
} # end of plotting function
#
