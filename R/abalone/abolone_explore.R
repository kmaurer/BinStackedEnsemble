setwd("~/GitHub/BinStackedEnsemble/R")
source("binningFunctionsUpdated.R")
source("predictFunctionsUpdated.R")
source("trainFunctionsUpdated.R")
source("weightingFunctionsUpdated.R")
source("SimulationStudyFunctions.R")

# experimental "bin-fitted" functions
source("ModelsWithinBins.R")

# source experimental Iterative Quantile binning functions
source("C:\\Users\\maurerkt\\Documents\\GitHub\\IQbinR\\IterativeQuantileBinningSupportFunctions.R")
source("C:\\Users\\maurerkt\\Documents\\GitHub\\IQbinR\\IterativeQuantileBinning.R")

options(java.parameters = "-Xmx2g")

############################################## abalone data
library(data.table)
abalone <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')
abalone<-as.data.frame(abalone)
names(abalone)[1]<-"true_class"
abalone$true_class<-as.factor(abalone$true_class)
true_classes <- levels(abalone$true_class)

library(randomForest)
myforest <- randomForest(true_class ~ . , data=abalone)
myforest
importance(myforest)


set.seed(12345)
cv_index <- make_cv_cohorts(abalone,10)
member_perform <- list(NULL)
for(cv_k in 1:10){
  train <- abalone[cv_index!=cv_k,]
  test <- abalone[cv_index==cv_k,]
  model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                   "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
  modelList <- make_model_list(model_types, train)
  member_perform[[cv_k]] <-eval_ensemble_members(modelList, test)
}
fold_accuracies <- matrix(unlist(lapply(member_perform, function(x) as.matrix(x[,2]))),ncol=10,byrow=F)
fold_weight <- sapply(1:10, function(x) sum(cv_index==x))/nrow(abalone)

cv_member_accuracies <- data.frame(member=model_types,accuracy=fold_accuracies %*% fold_weight)
cv_member_accuracies

set.seed(12345)
cv_test_abalone <- cv_testing_all_ensembles(abalone, model_types, 
                                            bin_features_all=c("V5","V7","V8"), nbins_all=2:4,
                                            equal_bins=FALSE, cv_K=10)
set.seed(12345)
cv_test_abalone_bin_fitted <- cv_testing_all_bin_fitted_ensembles(abalone, model_types, 
                                                                  bin_features_all=c("V5","V7","V8"), nbins_all=2:4,
                                                                  equal_bins=TRUE, cv_K=10, true_classes=true_classes)

# save(cv_test_abalone, cv_test_abalone_bin_fitted, file="abalone_results_all.Rdata")

#-------------------------------------------------------------------------------------

load(file="abalone_results_all.Rdata")

which.max()
library(tidyverse)
  df1 <- cv_test_abalone$unbinned_results %>%
    mutate(bin_type="unbinned") %>%
    select(weight_type,comb_type,bin_type,accuracy)%>%
    as.data.frame()

  df2 <- cv_test_abalone$rect_binned_results %>%
    group_by(weight_type, comb_type,bin_type) %>%
    summarize(accuracy = max(accuracy),
              nbins_name=nbins_name[which.max(accuracy)],
              bin_pair_name=bin_pair_name[which.max(accuracy)]) %>%
    select(weight_type,comb_type,bin_type,accuracy)%>%
    as.data.frame()
  
  df3 <- cv_test_abalone$iq_binned_results %>%
    group_by(weight_type, comb_type,bin_type) %>%
    summarize(accuracy = max(accuracy),
              nbins_name=nbins_name[which.max(accuracy)],
              bin_pair_name=bin_pair_name[which.max(accuracy)])%>%
    select(weight_type,comb_type,bin_type,accuracy)%>%
    as.data.frame()

  cv_member_accuracies
  rbind(df1,df2,df3) %>% arrange(comb_type, weight_type,desc(accuracy))

  
  
  
library(data.table)
names(abalone) <- c("true_class","length","diameter","height","whole_wgt","shuck_wgt","viscera_wgt","shell_wgt","rings")


iq_def <- iterative_quant_bin(data=abalone, bin_cols=c("shell_wgt","shuck_wgt"),
                    nbins=c(2,2), output="both")

# Plot for ARA application
bin_aggs_bounds <- as.data.frame(iq_def$bin_def$bin_bounds)
ggplot() +
  geom_point(aes(x=shell_wgt,y=shuck_wgt,color=true_class),data=abalone)+
  geom_rect(aes(xmin=V1,xmax=V2,ymin=V3,ymax=V4),fill=NA,color="black",size=.75,data=bin_aggs_bounds) +
  theme_bw() +
  xlab("Shell Weight (g)") + ylab("Shucked Weight (g)") +
  ggtitle("Iteratively Quantile Binned Abalone Data")


