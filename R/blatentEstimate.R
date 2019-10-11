#' Use blatent to estimate a Bayesian latent variable model. Currently supports estimation of an LCDM (Loglinar Cognitive
#' Diagnosis Model)-like model using a probit link function.
#'
#' @description
#' Blatantly runs Bayesian latent variable models.
#'
#' @param dataMat A data frame containing the data used for the analysis.
#' @param modelText A character string that contains the specifications for the model to be run. See \code{\link{blatentSyntax}}
#'                  or more information about syntax formatting.
#' @param priorsList A list of priors to be placed on parameters of the model. Defaults to NULL. Currently only accepts NULL.
#'                   All priors not set in priorsList will be set in options using \code{\link{blatentControl}} via the
#'                   \code{\link{setDefaultPriors}} function.
#' @param options A list of options for estimating the model. Use the \code{\link{blatentControl}} function to specify the options.
#'                See \code{\link{blatentControl}} for more information and default values.
#'
#' @return A blatentModel object (an R6 class).
#'
#' @export
blatentEstimate = function(dataMat, modelText, priorsList = NULL, options = blatentControl()){

  # parse model syntax
  specs = initializeSpecs(modelText = modelText, dataMat = dataMat, options = options)

  # set up data frame for modeling
  data = createModelData(specs = specs)

  # set up object for variables in graph
  variables = createVariables(specs = specs, data = data, options = options)

  # add underlying variables to data
  # data = addUnderlyingVariablesToData(data = data, variables = variables, specs = specs)

  #create parameter list that will build  chains later
  specs$parameters = unlist(lapply(
    X = variables,
    FUN = function(x) {
      if (!is.null(x$beta))
        return(rownames(x$beta))
    }
  ))

  # create chain holders
  blankChain = createBlankChainHolders(specs = specs, options = options)

  temp = createFortranObjects(specs = specs, options = options, data = data, variables = variables, blankChain = blankChain)

  # write data to disk for fortan-based program
  write.table(x = temp$fspecs,     file = paste0(filePath, "fspecs.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(x = temp$fvariables, file = paste0(filePath, "fvariables.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(x = temp$fpriors,    file = paste0(filePath, "fpriors.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(x = temp$fdata,      file = paste0(filePath, "fdata.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(x = temp$finit,      file = paste0(filePath, "finit.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(x = temp$fmodel,     file = paste0(filePath, "fmodel.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(x = temp$fmodelrows, file = paste0(filePath, "fmodelrows.csv"), quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")

  temp = NULL

  # for each chain, call the program and give it the chain number
  vchain = rep(list(NULL), options$nChains)
  uchain = rep(list(NULL), options$nChains)
  chain=1

  for (chain in 1:options$nChains){
    system(paste0(filePath, "./GPDCMRunning ", filePath, " ", chain))
    vchain[[chain]] = read.csv(file = paste0(filePath, "vChain-", chain, ".csv"), header = FALSE)


    # strip off remaining NA column
    vchain[[chain]] = vchain[[chain]][,1:(ncol(vchain[[chain]])-1)]

    # add parameter names
    names(vchain[[chain]]) = c("chain", "iteration", specs$parameters)
    vchain[[chain]] = coda::mcmc(vchain[[chain]][,3:ncol(vchain[[chain]])])

    uchain[[chain]] = read.csv(file = paste0(filePath, "uChain-", chain, ".csv"), header = FALSE)
  }
  vchain = as.mcmc.list(vchain)
  model = blatentModel$new(data = dataMat, specs = specs, options = options, chain = vchain, variables = variables)

  model$parameterSummary = makeChainSummary(chain = vchain)
  # prepare summary of chain estimates here


  return(model)



}
