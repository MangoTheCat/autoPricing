\name{getCoeffs}
\alias{getCoeffs}
\title{Function for formatting coefficients...}
\usage{getCoeffs(myExposure="Exposure", myGlm=theGlm, theForm, sourceData)}
\description{Function for formatting coefficients}
\value{The output is a nicely formatted table of the coefficients}
\note{This formula is used extensively in the \code{forwardBackStepAIC} function}
\keyword{glm}
\keyword{coefficients}
\author{Chibisi Chima-Okereke \email{cchima-okereke@mango-solutions.com}}
\arguments{\item{\code{myExposure}}{the name of the column of the dataset that has the exposure (in years)}
\item{\code{myGlm}}{the glm object}
\item{\code{theForm}}{should be the formula if theGlm$formula is NULL}
\item{\code{sourceData}}{should be the source data if theGlm$data is NULL}}
