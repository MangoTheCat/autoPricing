#' This is a function to plot rating factors
#'
#' @param \code{ratingFactor} a character string of the rating factor to be plotted
#' @param \code{theCoefficients} output from the \code{getCoeffs()} function 
#' @return There is no output to this function
#' @note This is an internal function
#'
#' @keywords plotting rating factors
#' @author Chibisi Chima-Okereke \email{cchima-okereke@@mango-solutions.com}
#
.plotRatingFactor <- function(ratingFactor = "BonusMalus", theCoefficients = myCoeffs){
#

# Defining Colors
# EMB Yellow
embYellow <- c(250, 255, 102)/255
embYellow <- rgb(embYellow[1], embYellow[2], embYellow[3], alpha = 0.7)
myColors <- colors()[c(26, 451, 142, 83, 254, 491, 454, 32, 381, 494, 121, 548,
                                                504, 150, 493, 591, 115, 135, 75, 393)]
#
theCoefficients$High95 <- theCoefficients$Estimate + theCoefficients$StdError*1.96
theCoefficients$Low95 <- theCoefficients$Estimate - theCoefficients$StdError*1.96
#
fullYRange <- c(0, max(2*theCoefficients$PercentageExposure))
yRange <- c(fullYRange[2]/2, fullYRange[2])
xRange <- range(c(theCoefficients$Estimate, theCoefficients$High95, theCoefficients$Low95))
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
fullYRange <- c(fullYRange[1], 1.2*ceiling(fullYRange[2]))
#
#Plotting
#
#windows()
par(mar = c(5,4,4,5) + .1)
xCoords <- barplot(theCoefficients$PercentageExposure, col = embYellow, ylim = fullYRange, yaxt = "n", 
        main = paste("Predicted Values for the ", ratingFactor, " rating factor", sep = ""), names.arg = theCoefficients$Level1)
lines(xCoords, convertScale(theCoefficients$Estimate, parm2Perc = TRUE), lwd = 2, col = "blue")
lines(xCoords, convertScale(theCoefficients$High95, parm2Perc = TRUE), lwd = 2, col = colors()[33], lty = 2)
lines(xCoords, convertScale(theCoefficients$Low95, parm2Perc = TRUE), lwd = 2, col = colors()[33], lty = 2)
points(xCoords, convertScale(theCoefficients$Estimate, parm2Perc = TRUE), cex = 1.7, lwd = 2, pch = 21, bg = "blue", col = "brown")
axis(4)
mtext("Exposure %", side = 4, line = 3)
#axis(2, labels = c("", round(convertScale(axTicks(2), parm2Perc = FALSE), 2)[-1]), at = axTicks(2))
axis(2, labels = c(round(convertScale(axTicks(2), parm2Perc = FALSE), 2)), at = axTicks(2))
mtext("Fitted Coefficient (Base = 0)", side = 2, line = 3)
} # end of plotting function
