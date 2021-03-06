% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/observed.R
\name{observed}
\alias{observed}
\title{Declares observed variables in a blatent model}
\usage{
observed(distribution = "bernoulli", link = "probit")
}
\arguments{
\item{distribution}{Specifies the distribution of the observed variable(s) to which the function points. Defaults to \code{"bernoulli"}.
 Distributions currently available are:
\itemize{
  \item \code{"bernoulli"}: Specifies each variable follows a Bernoulli distribution.
}}

\item{link:}{Specifies the link function used for any observed variable model where the observed variable is predicted.
  Defaults to \code{"probit"}. Link functions currently available are:
\itemize{
  \item \code{"probit"}: Uses a probit link function. Available for variables where \code{distribution = "bernoulli"} only.
}}
}
\description{
Used in \code{\link{blatentSyntax}} to declare the distribution and link function for observed variables
  as an unevaluated function call. Sets specifications used in estimation.
}
