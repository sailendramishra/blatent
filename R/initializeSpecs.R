#' @export
initializeSpecs = function(modelText,
                           dataMat,
                           options) {

  specs = list(
    StartTime = Sys.time(),
    EndTime = NULL,
    modelText = modelText,
    latentVariables = NULL,
    parameters = NULL,
    inputData = dataMat,
    priorsList = NULL,
    posteriorPredictive = any(unlist(
      lapply(
        X = options$posteriorPredictiveChecks,
        FUN = function(x)
          if (x) {
            return(TRUE)
          } else {
            return(FALSE)
          }
      )
    )),
    cl = NULL
  )

  parsedText = parseModelSyntax(modelText = modelText)
  specs$distributionsInput = parsedText$distributions
  specs$latentsInput = parsedText$latents

  # get lists variables from parsedText
  if (length(parsedText$latents) > 0){
    specs$latentVariables = names(parsedText$latents)
    specs$nLatentVariables = length(parsedText$latents)
  } else {
    specs$latentVariables = NULL
    specs$nLatentVariables = 0
  }

  specs$allDVs = unlist(lapply(X = parsedText$modelVector, FUN = function(x) return(as.character(terms(x)[[2]]))))
  specs$allPredictors = lapply(X = parsedText$modelVector, FUN = function(x) return(all.vars(terms(x)[[3]])))
  specs$allVariables = unique(c(unlist(specs$allDVs), unlist(specs$allPredictors)))

  specs$observedVariables = specs$allVariables[which(!(specs$allVariables %in% specs$latentVariables))]
  specs$nObservedVariables = length(specs$observedVariables)
  specs$modelVector = parsedText$modelVector

  specs$nUnits = nrow(specs$inputData)

  # get specs for Latent Variables (used in generation)
  specs$latentVector =
    specs$modelVector[
      unlist(lapply(X = specs$allDVs, FUN = function(x) {
        if (any(x %in% specs$latentVariables)) {
          return(TRUE)
        } else {
          return(FALSE)}
      }))]
  specs$latentOrder = determineDependencies(modelVector = specs$latentVector)

  specs$nObservedVariables = length(specs$observedVariables)
  specs$nLatents = length(specs$latentVariables) # until differentiable latents are included
  specs$nCategoricalLatents = length(specs$latentVariables)

  # get order for prediction based on dependencies in graph
  specs$predOrder = determineDependencies(modelVector = specs$modelVector)

  # get order for observed variables only
  specs$obsOrder = specs$predOrder[which(specs$predOrder %in% specs$observedVariables)]

  # get total iterations per chain
  specs$iterationsPerChain = options$nBurnin + options$nSampled*options$nThin
  specs$iterationsTotal = specs$iterationsPerChain*options$nChains

  #get names of underlying variables:

  specs$underlyingVariables = paste0("z_", c(specs$observedVariables))

  # add non-joint distribution latents to underlyingVariables
  if (length(parsedText$latentJointDists) > 0){

    addList = which(!specs$latentVariables %in% c(names(parsedText$latentJointDists), unique(parsedText$latentJointDists)))
    if (length(addList)>0) specs$underlyingVariables = c(specs$underlyingVariables, paste0("z_", specs$latentVariables[addList]))

  } else {
    specs$underlyingVariables = c(specs$underlyingVariables, paste0("z_", specs$latentVariables))
  }

  #parse priors (if any)

  if (!is.null(parsedText$priors)){
    specs$priorsList = sapply(X = priors, FUN = parsePriors, simplify = FALSE)
  }

  return(specs)
}
