\name{.modelFamilySpec}
\alias{.modelFamilySpec}
\title{Function to specify the model family/link distributions/functions...}
\usage{.modelFamilySpec(distr="poisson", myLink="log", theta)}
\description{Function to specify the model family/link distributions/functions}
\value{The output is an object of class "family"}
\note{This is just a wrapper function for the family() function}
\keyword{addition}
\keyword{arithmetic}
\references{http://finzi.psych.upenn.edu/R/library/stats/html/family.html}
\author{Chibisi Chima-Okereke \email{cchima-okereke@mango-solutions.com}}
\arguments{\item{\code{distr}}{character string denoting the distribution to use options are "poisson", 
"binomial", "gaussian", "Gamma", "inverse.gaussian", "quasi", "quasibinomial", 
"quasipoisson", "negative.binomial"}
\item{\code{myLink}}{character string denoting the link function, e.g. "identity", 
"log", "logit", "inverse", "1/mu^2"}
\item{\code{theta}}{should be set to the value of theta if the "negative.binomal" distribution is selected}}
\examples{modelFamilySpec("poisson", "log")}
