make_train_preds <- function(train_data,model_list){
  train_data <- as.data.frame(train_data)                                # Protect against data.frame hybrids with unintended attributes
  true_classes <- levels(train_data[,"true_class"])                      # vector of true class labels for reference
  K <- length(true_classes)                                              # number or classes
  #!# THIS IS WHY OUR EXPERIMENTS TAKE SOOOOO LONG!!! Do not need to refit CV training predictions just to change weighting structure!
  train_preds <- make_preds(train_data, model_list, true_classes)         # predict training data to estimate models' accuracies
  return(cbind(train_data,train_preds))
}


#' Build a weighted ensemble list
#'
#' @description Gather all parameters of a weighted ensemble of RWeka models into formated list
#'
#' @param train_data Data on which to train the ensemble
#' @param model_storage_list A list of pre trained models (using the same data as \code{train})
#' @param weightType How the ensemble should be weighted; "unweighted", "weighted", "bin weighted"
#' @param comb_rule How the ensemble predictions should be comined; "average posterior","majority vote"
#' @param bin_type How bins should be created when \code{weightType} is "bin weighted"; "average posterior","majority vote"
#' @param bin_features The name of two numeric bins when \code{weightType} is "bin weighted"
#' @param nbins The number of bins to create when \code{weightType} is "bin weighted"
#'
#' @return A trained ensemble that can be used to predict new data points
#' @expo
make_ensemble <- function(train_preds = NULL, model_list = NULL, weightType = NULL, comb_rule = NULL, bin_type = "standard", bin_features = NULL, nbins = NULL){
  true_classes <- levels(train_preds[,"true_class"])                      # vector of true class labels for reference
  ensemble <- list(weightType = weightType,
                   comb_rule = comb_rule,
                   bin_type = bin_type,
                   bin_features = bin_features,
                   nbins = nbins,
                   trueClasses = true_classes,
                   trainPreds = train_preds,
                   model_list = model_list)

  return(ensemble)
}

#' Function for building model list given Weka names
#'
#' @description Function for making predictions of classes using member models.
#'
#' @param model_types A vector of Weka model type names
#' @param data A data frame to predict
make_model_list <- function(model_types, data, ...){
  model_list <- list(NULL)
  for(m in 1:length(model_types)){
    mod_m_fct <- RWeka::make_Weka_classifier(model_types[m])
    model_list[[m]] <- mod_m_fct(true_class ~., data)
  }
  names(model_list) <- model_types
  return(model_list)
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
  # need to generate cross validated predictions
  for(m in 1:length(model_storage_list)){
    preds[,m] <- cv_preds(names(model_storage_list)[m], data, true_classes, cv_k=10)
    names <- c(names, paste("preds", m, sep = ""))
  }
  colnames(preds) <- names
  row.names(preds) <- row.names(data)
  return(preds)
}

#' K-fold CV Predictions
#'
#' @description Function for making K-fold CV predictions of classes using member models from RWeka
#'
#' @param mod An RWeka model object
#' @param data A data frame to predict
#' @param true_classes Vector holding the order of the true labels
#' @param cv_k Number of Folds in CV. Default=10
#' @return \code{preds}
cv_preds <- function(model_type, dat, true_classes, cv_k=10){
  n <- nrow(dat)
  pred_classes <- factor(rep(NULL,n), levels = true_classes)
  cv_index <- cv_cohorts(nrow(dat), cv_k)
  for(k in 1:cv_k){
    # generate train/test based on CV folds
    test_index <- (cv_index==k)
    train_cv <- dat[-test_index,]
    test_cv <- dat[test_index,]
    # fit model to train
    mod_fct <- RWeka::make_Weka_classifier(model_type)
    mod <- mod_fct(true_class ~., train_cv)
    # test on withheld fold
    pred_classes[test_index] <- factor(predict(mod, type = "class", newdata = test_cv), levels = true_classes)
  }    
  return(pred_classes)
}


# CV cohort generator
#' @description Function for making CV fold indeces
#'
#' @param data_size Sample size in data being cross validated
#' @param cv_k Number of Folds in CV. Default=10
#' @return \code{preds}
cv_cohorts <- function(data_size,cv_K){
  if(data_size %% cv_K == 0){ # if perfectly divisible
    cv_cohort <- sample(rep(1:cv_K, each=(data_size %/%cv_K)))
  } else { # if not perfectly divisible
    cv_cohort <- sample(c(rep(1:(data_size %% cv_K), each=(data_size%/%cv_K + 1)),
                              rep((data_size %% cv_K + 1):cv_K,each=(data_size%/%cv_K)) ) )
  }
  return(cv_cohort)
}







