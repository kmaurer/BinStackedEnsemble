#'Predict using an ensemble classifier
#'@description Predict using an ensemble classifier created from this package
#'
#'@param ensemble An ensemble classifier built with \code{buildWeightedEnsemble}
#'@param test_data Data to predict with the ensemble
#'
#'@return Predictions for the provided data
#'@export
predictEnsemble <- function(ensemble, test_data){

  n <- nrow(test_data)                                        # how many predictions to make
  K <- length(levels(ensemble$trainPreds$true_class))        # the number of classes
  M <- length(ensemble$model_list)                            # the number of models

  ##
  ## Calculate model weights
  ##
  modelWeights <- NULL
  if(ensemble$weightType == "unweighted"){
    modelWeights <- array(1,c(n,M))
  }else if(ensemble$weightType == "weighted"){
    modelWeights <- weighted(ensemble$trainPreds, M, n)
  }else if(ensemble$weightType %in% c("bin weighted","bin dictator") ){
    modelWeights <- bin_weighted(ensemble$bin_features, ensemble$bin_type,
                                 ensemble$nbins, ensemble$trainPreds,
                                 test_data, M, K)
    if(ensemble$weightType == "bin dictator") modelWeights <- bin_dictator_weighted(modelWeights)
  }else{
    print("Provide a valid weightType")
    return(NULL)
  }

  ##
  ## Make predictions
  ##
  model_votes <- make_model_metric_array(ensemble$comb_rule,
                                          ensemble$model_list,
                                          test_data,
                                          ensemble$trueClasses)

  pred <- rep(NA,nrow(test_data))
  for(i in 1:nrow(test_data)){
    combination_class_results <- rep(NA,K)
    for(k in 1:K){
      combination_class_results[k] <- modelWeights[i,] %*% model_votes[i,k,]
    }
    # Assign predicted classes based on maximized combination rule
    pred[i] <- ensemble$trueClasses[which.max(combination_class_results)]
  }
  pred <- factor(pred,levels=ensemble$trueClasses)
  names(pred) <- row.names(test_data)
  return(pred)
}


#'Function to evaluated the ensemble test accuracy against member accuracies
#'@description Function to evaluated the ensemble test accuracy against member accuracies
#'
#'@param model_list A list of RWeka Models
#'@param test_data Data to predict with the ensemble
#'
#'@return Predictions for the provided data
eval_ensemble_members <- function(model_list, test_data){
  acc_df <- data.frame(model = names(model_list),
                       accuracy = NA)
  for(m in 1:length(model_list)){
    acc_df$accuracy[m] <- RA(table(test_data$true_class, predict(model_list[[m]], test_data[,-which(names(test_data)=="true_class")])))
  }
  return(acc_df)
}


#'Function to evaluated the ensemble test accuracy against member accuracies
#'@description Function to evaluated the ensemble test accuracy against member accuracies
#'
#'@param ensemble An ensemble classifier built with \code{buildWeightedEnsemble}
#'@param test_data Data to predict with the ensemble
#'
#'@return Predictions for the provided data
eval_ensemble <- function(ensemble, test_data){
  acc_df <- data.frame(model = "Ensemble",
                       accuracy = RA(table(test_data$true_class, predictEnsemble(ensemble, test_data[,-which(names(test_data)=="true_class")]))) )
  acc_df <- rbind(acc_df,eval_ensemble_members(ensemble$model_list,test_data))
  return(acc_df)
}


#'Function to calculate classifier accuracy based on confusion matrix
#'@description Function to calculate classifier accuracy based on confusion matrix
#'
#'@param confusion_matrix confusion matrix from a classifier
#'
#'@return correct classification rate (numeric vector of length 1)
RA <- function(confusion_matrix){
  row_dim<-dim(confusion_matrix)[1]
  s1<-1
  diag_sum<-0
  accuracy<-0
  while(s1<=row_dim)
  {
    s2<-1
    while(s2<=row_dim)
    {
      if(s1==s2) {
        diag_sum<-diag_sum+confusion_matrix[s1,s2]
      }
      s2<-s2+1 }
    s1<-s1+1 }
  accuracy<-diag_sum/sum(confusion_matrix)
  return(accuracy)
}



