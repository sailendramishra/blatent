createVariables = function(specs, data, options) {
  if (is.null(specs$cl)) {
    variables = lapply(
      X = 1:length(specs$modelVector),
      FUN = createVariable,
      specs = specs,
      data = data,
      options = options
    )
  } else {
    parallel::clusterExport(cl = specs$cl,
                            varlist = c("specs"),
                            envir = environment())
    variables = parallel::parLapply(
      cl = specs$cl,
      X = 1:length(specs$modelVector),
      fun = createVariable,
      specs = specs,
      data = data,
      options = options
    )
  }

  # get names of variables and name them within object
  varNames = lapply(
    X = variables,
    FUN = function(x)
      return(x$variableName)
  )
  names(variables) = varNames

  return(variables)
}
