createBlankChainHolders = function(specs, options){
  blankChain = list(variables = data.frame(matrix(data = 0, nrow = options$nSampled, ncol = length(specs$parameters))))
  names(blankChain$variables) = specs$parameters

  # create list of person parameters and data
  if (length(specs$latentVariables) > 0) {
    specs$unitNames = rownames(data)
    if (is.null(specs$unitNames)) specs$unitNames = rep(paste0("row", 1:specs$nUnits))

    blankChain$units = data.frame(matrix(data = 0, nrow = options$nSampled, ncol = length(specs$unitNames)))
    names(blankChain$units) = specs$unitNames

  }


  if (specs$posteriorPredictive) {
    nVars = 0
    colNames = NULL
    # if (options$posteriorPredictiveChecks$proportion) {
    #   nVars = nVars + length(specs$observedVariables)
    #   colNames = c(colNames,
    #                paste0(specs$observedVariables, "_PPCproportion"))
    # }
    #
    # if (options$posteriorPredictiveChecks$univariate) {
    #   nVars = nVars + length(specs$observedVariables) + 1
    #   colNames = c(
    #     colNames,
    #     paste0(specs$observedVariables, "_PPCinivariate"),
    #     "TotalPPCunivariate"
    #   )
    # }

    if (options$posteriorPredictiveChecks$bivariate) {
      nVars = nVars + choose(length(specs$observedVariables), 2) + 1
      colNames = c(colNames, unlist(
        lapply(
          X = combn(specs$observedVariables, 2, simplify = FALSE),
          FUN = paste0,
          "_PPCbivariate",
          collapse = ""
        )
      ), "TotalPPCbivariate")
    }




    blankChain$posteriorChecks = matrix(data = 0,
                                        nrow = options$nSampled,
                                        ncol = nVars)
    colnames(blankChain$posteriorChecks) = colNames

  }

  return(blankChain)
}
