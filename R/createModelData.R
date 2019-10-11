createModelData = function(specs){
  data = specs$inputData[specs$observedVariables]

  # append data with latent variables, if any
  if (specs$nLatents > 0){
    data = cbind(data, matrix(data = 0, nrow = specs$nUnits, ncol = specs$nLatents))
    names(data)[(specs$nObservedVariables+1):(specs$nObservedVariables+specs$nLatents)] = specs$latentVariables
  }

  # if (specs$nCategoricalLatents > 0){
  #   data = cbind(data, rep(0, specs$nUnits))
  #   names(data)[length(names(data))] = "classnumber_"
  # }

  return(data)
}
