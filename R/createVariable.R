createVariable = function(modelNumber, specs, data, options){
  #assumes all variables are same type

  currentVariable = as.character(terms(specs$modelVector[[modelNumber]])[[2]])
  currentModel = modelNumber

  variable = list(
    variableName = currentVariable,
    variableNumber = modelNumber,
    predicts = NULL
  )

  if (any(specs$latentVariables == currentVariable)){
    variable$isLatent = TRUE
    variable$distributionSpecs = eval(parse(text = specs$latentsInput[which(names(specs$latentsInput) == currentVariable)]))
  } else {
    variable$isLatent = FALSE
    variable$distributionSpecs = eval(parse(text = specs$distributionsInput[which(names(specs$distributionsInput) == currentVariable)]))
  }

  #get formula for prediction
  variable$formula = specs$modelVector[[currentModel]]
  variable$formulaRHS = variable$formula[-2] # get only ~ and RHS: used for generating predictions

  #get names associated with parameters
  tempParam = colnames(model.matrix(variable$formula, data = data))
  variable$nParam = length(tempParam)
  if (variable$nParam > 0){
    variable$paramNames = paste(currentVariable, tempParam, sep=".")
    variable$predNames = tempParam
    variable$beta = matrix(data = 0, nrow = variable$nParam, ncol = 1)

    # for prior distribution
    variable$betaMean = matrix(data = options$defaultPriors$normalMean, nrow = variable$nParam, ncol = 1) #default prior mean 0
    variable$betaCov = matrix(data = options$defaultPriors$normalCovariance, nrow = variable$nParam, variable$nParam)
    diag(variable$betaCov) = options$defaultPriors$normalVariance
    rownames(variable$beta) = variable$paramNames

    # check if parameters are listed in priorsList
    if (any(names(specs$priorsList) %in% variable$paramNames)){
      relevantPriors = specs$priorsList[which(names(specs$priorsList) %in% variable$paramNames)]

      # loop through and add prior components
      for (param in 1:length(relevantPriors)){
        # get location
        paramLoc = which(variable$paramNames == names(relevantPriors)[param])

        # get type
        if ("mean" %in% names(relevantPriors[[param]])){
          variable$betaMean[paramLoc, 1] = relevantPriors[[param]]$mean
        }

        if ("variance" %in% names(relevantPriors[[param]])){
          variable$betaCov[paramLoc, paramLoc] = relevantPriors[[param]]$variance
        }
      }

    }

  } else {
    variable$paramNames = NULL
    variable$predNames = NULL
    variable$beta = NULL
    variable$betaMean = NULL
    variable$betaCov = NULL
  }
  tempParam = NULL

  # for saving mean and covariance matrix used in generating betas
  variable$initializeParameters = options$defaultInitializeParameters

  #determine if any observations are missing:
  variable$missing = which(is.na(data[variable$variableName]))
  variable$observed = which(!is.na(data[variable$variableName]))
  variable$N = length(variable$observed)
  variable$Nmissing = length(variable$missing)

  # formerly from createVariablePrediction
  variable$predicts = NULL

  # determine all other variables where this variable is a predictor
  model =1
  for (model in 1:length(specs$modelVector)){
    preds = all.vars(terms(specs$modelVector[[model]])[[3]])
    dv = all.vars(terms(specs$modelVector[[model]])[[2]])

    if (currentVariable %in% preds){
      variable$predicts = c(variable$predicts, dv)
    }
  }


  return(variable)
}
