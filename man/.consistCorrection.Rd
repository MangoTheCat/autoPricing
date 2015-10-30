\name{.consistCorrection}
\alias{.consistCorrection}
\title{This function rebalances a raw variable that is significant but not consistent...}
\usage{.consistCorrection(glm1=currentModel, currVar=currentVar)}
\description{This function rebalances a raw variable that is significant but not consistent}
\value{The output is a list of revised glm and a character either \code{"Consistent"} or \code{"Not Consistent"}}
\note{This is an internal function}
\keyword{glm}
\keyword{consistency}
\keyword{significance}
\author{Chibisi Chima-Okereke \email{cchima-okereke@mango-solutions.com}}
\arguments{\item{\code{glm1}}{the glm that contains the rating factor to be analysed output from the \code{.forwardBackSig} function}
\item{\code{currVar}}{the rating factor to be analysed}}
