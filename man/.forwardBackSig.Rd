\name{.forwardBackSig}
\alias{.forwardBackSig}
\title{Master function to find out if the variable is significant when you add a variable...}
\usage{.forwardBackSig(theModel, currVar, alg="forward", ...)}
\description{Master function to find out if the variable is significant when you add a variable}
\value{The model that will be used and whether the \code{currVar} was significant or not}
\note{This is an internal function and should not be used by the user}
\keyword{glm}
\keyword{forward}
\keyword{backward}
\seealso{\code{\link{glm}}}
\seealso{\code{\link{stepIC}}}
\author{Chibisi Chima-Okereke \email{cchima-okereke@mango-solutions.com}}
\arguments{\item{\code{theModel}}{this is the candidate model}
\item{\code{currVar}}{this is the current variable being investigated}
\item{\code{alg}}{this is the algoirthm being used either forward or backward}}
