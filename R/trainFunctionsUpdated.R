#' Build a weighted ensemble
#'
#' @description This function will create a weighted ensemble from
#' RWeka models
#'
#' @param train_data Data on which to train the ensemble
#' @param modelList A list of pre trained models (using the same data as \code{train})
#' @param weightType How the ensemble should be weighted; "unweighted", "weighted", "bin weighted"
#' @param comb_rule How the ensemble predictions should be comined; "average posterior","majority vote"
#' @param bin_type How bins should be created when \code{weightType} is "bin weighted"; "average posterior","majority vote"
#' @param bin_features The name of two numeric bins when \code{weightType} is "bin weighted"
#' @param nbins The number of bins to create when \code{weightType} is "bin weighted"
#'
#' @return A trained ensemble that can be used to predict new data points
#' @export
buildWeightedEnsemble <- function(train_data = NULL, modelList = NULL, weightType = NULL, comb_rule = NULL, bin_type = NULL, bin_features = NULL, nbins = NULL){

  ##
  ## Check that necessary information is provided
  ##
  if(is.null(train_data)){
    print("Please provide train_data")
    return(NULL)
  }
  if(is.null(modelList)){
    print("Please provide modelList")
    return(NULL)
  }
  if(!(weightType %in% c("unweighted", "weighted", "bin weighted", "bin dictator"))){
    print("Please provide valid weightType")
    return(NULL)
  }
  if(is.null(comb_rule)){
    print("Please provide Combination Rule")
    return(NULL)
  }
  if(is.null(bin_type)){
    print("Please provide bin_type")
    return(NULL)
  }

  ##
  ## Perform model predictions on training data
  ##
  true_classes <- levels(train_data[,"true_class"])                      # vector of true class labels for reference
  K <- length(true_classes)                                         # number or classes
  train_preds <- make_preds(train_data, modelList, true_classes)         # predict training data to estimate models' accuracies
  train <- cbind(train_data,train_preds)                                 # add predictions to training data


  ##
  ## Store ensemble information here
  ##
  ensemble <- list(weightType = weightType,
                   comb_rule = comb_rule,
                   bin_type = bin_type,
                   bin_features = bin_features,
                   nbins = nbins,
                   trueClasses = true_classes,
                   trainPreds = train,
                   modelList = modelList)

  return(ensemble)
}

#' Predict classes using member models
#'
#' @description Function for making predictions of classes using member models.
#'
#' @param model_storage_list A list holding RWeka models
#' @param data A data frame to predict
#' @param true_classes Array holding the order of the true labels
#' @return \code{preds}
make_preds <- function(data, model_storage_list, true_classes){
  preds <- as.data.frame(matrix(0, ncol = length(model_storage_list), nrow = dim(data)[1]))
  names <- c()
  for(m in 1:length(model_storage_list)){
    preds[,m] <- factor(predict(model_storage_list[[m]], type = "class", newdata = data, levels = true_classes))
    names <- c(names, paste("preds", m, sep = ""))
  }
  colnames(preds) <- names
  row.names(preds) <- row.names(data)
  return(preds)
}








