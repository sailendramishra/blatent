#' @title Creates simulation specifications for simulating data in blatent
#'
#' @description Sets the specifications for the generation of the types of parameters in the simulation. Currently comprised
#' of a list of unevaluated expressions (encapsulated in quotation marks; not calls for ease of user input) that will be evaluated by
#' simulation function to generate parameters. Input must be in the form of a random number generation function to be called, surrounded by quotation marks.
#'
#' @param observedIntercepts The data generating function for all intercepts for observed variables. Defaults to \code{"runif(n = 1, min = -2, max = 2)"}.
#' @param observedMainEffects The data generating function for the main effects for observed variables. Defaults to \code{"runif(n = 1, min = 0, max = 2)"}.
#' @param observedInteractions The data generating function for all interactions for observed variables. Defaults to \code{"runif(n = 1, min = -2, max = 2)"}.
#' @param latentIntercepts The data generating function for all intercepts for latent variables. Defaults to \code{"runif(n = 1, min = -1, max = 1)"}.
#' @param latentMainEffects The data generating function for the main effects for latent variables. Defaults to \code{"runif(n = 1, min = -1, max = 1)"}.
#' @param latentInteractions The data generating function for all interactions for latent variables. Defaults to \code{"runif(n = 1, min = -0.5, max = 0.5)"}.
#'
#'
#' @export
setDefaultSimulatedParameters <- function(observedIntercepts = "runif(n = 1, min = -2, max = 2)",
                                          observedMainEffects = "runif(n = 1, min = 0, max = 2)",
                                          observedInteractions = "runif(n = 1, min = -2, max = 2)",
                                          latentIntercepts = "runif(n = 1, min = -1, max = 1)",
                                          latentMainEffects  = "runif(n = 1, min = -1, max = 1)",
                                          latentInteractions = "runif(n = 1, min = -0.5, max = 0.5)"){

  defaultSimulatedParameters <- list(observedIntercepts = observedIntercepts, observedMainEffects = observedMainEffects, observedInteractions = observedInteractions,
                                     latentIntercepts = latentIntercepts, latentMainEffects = latentMainEffects, latentInteractions = latentInteractions)
  return(defaultSimulatedParameters)
}
