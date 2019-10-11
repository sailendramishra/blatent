#' @title Provides a list of posterior predictive model checks to be run following estimation of
#'        a Bayesian latent variable model.
#'
#' @description Currently two types of posterior predictive model checks (PPMCs) are available: bivariate and
#'              tetrachoric.
#'
#' @param bivariate If \code{TRUE}, runs bivariate PPCMs producing the Pearson Chi-square statistic
#'                  for two-way tables comparing observed and simulated data for all pairs of items.
#'                  Defaults to \code{TRUE}.
#'
#' @param tetrachoric If \code{TRUE}, calculates tetrachoric correlations between observed and simulated
#'                    data for all pairs of items. Defaults to \code{TRUE}.
#'
#' @return A list of named values containing a logical value for each parameter above.
#'
#' @export
setDefaultPosteriorPredictiveChecks = function(bivariate = TRUE, tetrachoric = TRUE){

  defaultPosteriorPredictiveChecks = list(bivariate = bivariate, tetrachoric = tetrachoric)
  return(defaultPosteriorPredictiveChecks)

}
