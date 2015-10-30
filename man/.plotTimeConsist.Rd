\name{.plotTimeConsist}
\alias{.plotTimeConsist}
\title{Function for plotting consistency...}
\usage{.plotTimeConsist(ratingFactor="BonusMalus", timeDef="Year",
    theGlm=myGlm, constThresh=60, expVar="Exposure", showPlots=TRUE)}
\description{Function for plotting consistency}
\value{The output is a numeric denoting the median consistency of the rating factor with year}
\note{...}
\seealso{\code{\link{stepIC}}}
\keyword{consistency}
\keyword{glm}
\keyword{time}
\author{Chibisi Chima-Okereke \email{cchima-okereke@mango-solutions.com}}
\arguments{\item{\code{ratingFactor}}{a character string denoting the rating factor to carry out time consistency analysis}
\item{\code{timeDef}}{a character string denoting the Year variable}
\item{\code{theGlm}}{a glm object to be analysed}
\item{\code{constThresh}}{a numeric denoting the threshold percentage of consistency}
\item{\code{expVar}}{set this to the exposure column name}
\item{\code{showPlots}}{logical for whether to plot the variables or not}}
