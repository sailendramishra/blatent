#' @title Declares latent variables in a blatent model
#'
#' @description Used in \code{\link{blatentSyntax}} to declare latent variables as an unevaluated function call.
#'   Sets specifications used in estimation.
#'
#' @param unit Attaches the unit (person) ID number or label to observations in data. Currently only allows "rows" which indicates each
#'   row of the data is a separate unit in the model. Defaults to \code{"rows"}.
#'
#' @param distribution Specifies the distribution of the latent variable(s) to which the function points. Defaults to \code{"bernoulli"}.
#'  Distributions currently available are:
#' \itemize{
#'   \item \code{"bernoulli"}: Specifies each variable follows a Bernoulli distribution.
#' }
#'
#' @param structure Specifies the type of distributional structure for the latent variables. Defaults to \code{"univariate"}.
#'  Stuctures current available are:
#' \itemize{
#'   \item \code{"univariate"}: Specifies each variable is modeled using a univariate (marginal or conditional) distribution.
#' }
#'
#' @param link: Specifies the link function used for any latent variable model where the latent variable is predicted.
#'   Defaults to \code{"probit"}. Link functions currently available are:
#' \itemize{
#'   \item \code{"probit"}: Uses a probit link function. Available for variables where \code{distribution = "bernoulli"} only.
#' }
#'
#' @param type: Specifies the type of latent variable to be estimated. Defaults to \code{"ordinal"}.
#'   Types currently available are:
#' \itemize{
#'   \item \code{"ordinal"}: Specifies that latent variables have ordinal categories. Available for variables where
#'     \code{distribution = "bernoulli"} only.
#' }
#'
#'
latent <- function(unit = "rows", distribution = "bernoulli", structure = "univariate", link = "probit",
                   type = "ordinal"){

  if (structure == "univariate"){
    jointName = NULL # must keep as NULL to prevent joint distributions with singular variables
    if (distribution == "bernoulli"){
      generationFunction = generateBernoulliWrap
      if (link == "probit"){
        linkFunction = qnorm
        inverseLinkFunction = pnorm
      }
    }
  } else if (structure == "joint") {
    if (distribution == "mvbernoulli"){
      generationFunction = rdirichlet
      linkFunction = identityLinkInvLink
      inverseLinkFunction = identityLinkInvLink
    }
  }

  return(
    list(
      distribution = distribution,
      generationFunction = generationFunction,
      inverseLinkFunction = inverseLinkFunction,
      isLatent = TRUE,
      isObserved = FALSE,
      jointName = jointName,
      linkFunction = linkFunction,
      link = link,
      structure = structure,
      type = type,
      unit = unit
    )
  )

}

