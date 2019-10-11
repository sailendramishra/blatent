#' @title Declares observed variables in a blatent model
#'
#' @description Used in \code{\link{blatentSyntax}} to declare the distribution and link function for observed variables
#'   as an unevaluated function call. Sets specifications used in estimation.
#'
#' @param distribution Specifies the distribution of the observed variable(s) to which the function points. Defaults to \code{"bernoulli"}.
#'  Distributions currently available are:
#' \itemize{
#'   \item \code{"bernoulli"}: Specifies each variable follows a Bernoulli distribution.
#' }
#'
#' @param link: Specifies the link function used for any observed variable model where the observed variable is predicted.
#'   Defaults to \code{"probit"}. Link functions currently available are:
#' \itemize{
#'   \item \code{"probit"}: Uses a probit link function. Available for variables where \code{distribution = "bernoulli"} only.
#' }
#'
observed = function(distribution = 'bernoulli', link = 'probit'){


  if (distribution == "bernoulli"){
    generationFunction = generateBernoulliWrap
    if (link == "probit"){
      linkFunction = qnorm
      inverseLinkFunction = pnorm
    }
  } else {
    # for later
  }

  return(
    list(
      distribution = distribution,
      generationFunction = generationFunction,
      inverseLinkFunction = inverseLinkFunction,
      isLatent = FALSE,
      isObserved = TRUE,
      linkFunction = linkFunction,
      link = link,
      structure = NA,
      type = NA,
      unit = NA
    )
  )

}
