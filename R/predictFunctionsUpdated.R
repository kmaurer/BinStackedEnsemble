#'Predict using an ensemble classifier
#'@description Predict using an ensemble classifier created from this package
#'
#'@param ensemble An ensemble classifier built with \code{buildWeightedEnsemble}
#'@param testData Data to predict with the ensemble
#'
#'@return Predictions for the provided data
#'@export
predictEnsemble <- function(ensemble, testData){

  n <- nrow(testData)                                        # how many predictions to make
  K <- length(levels(ensemble$trainPreds$true_class))        # the number of classes
  M <- length(ensemble$modelList)                            # the number of models

  ##
  ## Calculate model weights
  ##
  modelWeights <- NULL
  if(ensemble$weightType == "unweighted"){
    modelWeights <- array(1,c(n,K,M))
  }else if(ensemble$weightType == "weighted"){
    modelWeights <- weighted(ensemble$trainPreds, M, n)
  }else if(ensemble$weightType == "bin weighted"){
    modelWeights <- bin_weighted(ensemble$binFeatures, ensemble$binType,
                                 ensemble$nbins, ensemble$trainPreds,
                                 testData, M, K)
  }else{
    print("Provide a valid weightType")
    return(NULL)
  }

  ##
  ## Make predictions
  ##
  model_metric <- make_model_metric_array(ensemble$combinationRule,
                                          ensemble$modelList,
                                          testData,
                                          ensemble$trueClasses)

  pred <- rep(NA,nrow(testData))
  for(i in 1:nrow(testData)){
    combination_class_results <- rep(NA,K)
    for(k in 1:K){
      combination_class_results[k] <- modelWeights[i,k,] %*% model_metric[i,k,]
    }
    # Assign predicted classes based on maximized combination rule
    pred[i] <- ensemble$trueClasses[which.max(combination_class_results)]
  }
  pred <- factor(pred,levels=ensemble$trueClasses)
  names(pred) <- row.names(testData)
  return(pred)
}






