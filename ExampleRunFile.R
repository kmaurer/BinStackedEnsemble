##
## Function to calculate accuracy
##
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

##
## Load train and test data
##
train <- binClassifieR::phoneme[1:2500,]
test <- binClassifieR::phoneme[2501:5000,]


##
## Build models to stack
##
naiveBayes <- RWeka::make_Weka_classifier("weka.classifiers.bayes.NaiveBayes")
model1 <- naiveBayes(true_class ~., train)

randomForest <- RWeka::make_Weka_classifier("weka.classifiers.trees.RandomForest")
model2 <- randomForest(true_class ~., train)

logistic <- RWeka::make_Weka_classifier("weka.classifiers.functions.Logistic")
model3 <- logistic(true_class ~., train)

modelList <- list(model1, model2, model3)

modelList[[1]]




#--------------------------------------------------------------------------
# Iris example
##
names(iris)[5] <- "true_class"
train_index <- c(sample(1:50, 30),sample(51:100, 30),sample(101:150, 30))
train <- iris[train_index, ]
test <- iris[-train_index, ]


##
## Build models to stack
##
my_models <- c("weka.classifiers.bayes.NaiveBayes", "weka.classifiers.trees.RandomForest")

naiveBayes <- RWeka::make_Weka_classifier("weka.classifiers.bayes.NaiveBayes")
model1 <- naiveBayes(true_class ~., train)

randomForest <- RWeka::make_Weka_classifier("weka.classifiers.trees.RandomForest")
model2 <- randomForest(true_class ~., train)

bagging <- RWeka::make_Weka_classifier("weka.classifiers.meta.Bagging")
model3 <- bagging(true_class ~., train)

svm <- RWeka::make_Weka_classifier("weka.classifiers.functions.SMO")
model4 <- svm(true_class ~., train)
  
modelList <- list(model1, model2, model3, model4)

modelList[[1]]

predict(modelList[[4]], test)
##
## train
##
weightType <- "bin weighted"
# combinationRule <- "majority vote"
combinationRule <- "average posterior"
# bin_type <- "quantile"
bin_type <- "standard"
binFeatures <- c("Petal.Length", "Petal.Width")
nbins <- 2
weightedEnsemble <- buildWeightedEnsemble(train, modelList, weightType, combinationRule, bin_type, binFeatures, nbins)


##
## test
##
head(test[,-5])
predictEnsemble(weightedEnsemble, test[,-5])

s1 <- paste("Ensemble accuracy: ", RA(table(test$true_class, predictEnsemble(weightedEnsemble, test[,-5]))), sep = "")
s2 <- paste("Model 1 accuracy: ", RA(table(test$true_class, predict(model1, test[,-6]))), sep = "")
s3 <- paste("Model 2 accuracy: ", RA(table(test$true_class, predict(model2, test[,-6]))), sep = "")
s4 <- paste("Model 3 accuracy: ", RA(table(test$true_class, predict(model3, test[,-6]))), sep = "")
s5 <- paste("Model 4 accuracy: ", RA(table(test$true_class, predict(model4, test[,-6]))), sep = "")

s1
s2
s3
s4
s5

# Hi 


## train
##
weightType <- "bin weighted"
combinationRule <- "majority vote"
# combinationRule <- "average posterior"
# bin_type <- "quantile"
bin_type <- "standard"
binFeatures <- c("Aa", "Ao")
nbins <- 2
weightedEnsemble <- buildWeightedEnsemble(train, modelList, weightType, combinationRule, bin_type, binFeatures, nbins)


##




