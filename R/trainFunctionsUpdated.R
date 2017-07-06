#' Build a weighted ensemble
#'
#' @description This function will create a weighted ensemble from
#' RWeka models
#'
#' @param train Data on which to train the ensemble
#' @param modelList A list of pre trained models (using the same data as \code{train})
#' @param weightType How the ensemble should be weighted; "unweighted", "weighted", "bin weighted"
#' @param combinationType How the ensemble predictions should be comined; "average posterior","majority vote"
#' @param bin_type How bins should be created when \code{weightType} is "bin weighted"; "average posterior","majority vote"
#' @param binFeatures The name of two numeric bins when \code{weightType} is "bin weighted"
#' @param nbins The number of bins to create when \code{weightType} is "bin weighted"
#'
#' @return A trained ensemble that can be used to predict new data points
#' @export
buildWeightedEnsemble <- function(train = NULL, modelList = NULL, weightType = NULL, combinationType = NULL, bin_type = NULL, binFeatures = NULL, nbins = NULL){

  ##
  ## Check that necessary information is provided
  ##
  if(is.null(train)){
    print("Please provide train")
    return(NULL)
  }
  if(is.null(modelList)){
    print("Please provide modelList")
    return(NULL)
  }
  if(!(weightType %in% c("unweighted", "weighted", "bin weighted"))){
    print("Please provide valid weightType")
    return(NULL)
  }
  if(is.null(combinationType)){
    print("Please provide combinationType")
    return(NULL)
  }
  if(is.null(bin_type)){
    print("Please provide bin_type")
    return(NULL)
  }

  ##
  ## Perform model predictions on training data
  ##
  true_classes <- levels(train[,"true_class"])                      # vector of true class labels for reference
  K <- length(true_classes)                                         # number or classes
  train_preds <- make_preds(train, modelList, true_classes)         # predict training data to estimate models' accuracies
  train <- cbind(train,train_preds)                                 # add predictions to training data


  ##
  ## Store ensemble information here
  ##
  ensemble <- list(weightType = weightType,
                   combinationRule = combinationRule,
                   bin_type = bin_type,
                   binFeatures = binFeatures,
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
  for(i in 1:length(model_storage_list)){
    preds[,i] <- factor(predict(model_storage_list[[i]], type = "class", newdata = data, levels = true_classes))
    names <- c(names, paste("preds", i, sep = ""))
  }
  colnames(preds) <- names
  row.names(preds) <- row.names(data)
  return(preds)
}








