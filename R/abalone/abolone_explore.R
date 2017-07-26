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



library(gridExtra)
library(tidyverse)
library(stringr)
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

### Find stable CV estimates of member accuracies
n_folds <- 1000
cv_index <- make_cv_cohorts(abalone,n_folds)
member_perform <- list(NULL)
for(cv_k in 1:n_folds){
  train <- abalone[cv_index!=cv_k,]
  test <- abalone[cv_index==cv_k,]
  model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                   "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
  modelList <- make_model_list(model_types, train)
  member_perform[[cv_k]] <-eval_ensemble_members(modelList, test)
}
fold_accuracies <- matrix(unlist(lapply(member_perform, function(x) as.matrix(x[,2]))),ncol=n_folds,byrow=F)
fold_weight <- sapply(1:n_folds, function(x) sum(cv_index==x))/nrow(abalone)

cv_member_accuracies <- data.frame(member=model_types,accuracy=fold_accuracies %*% fold_weight)
cv_member_accuracies

### Find stable CV estimates of non-binned ensemble accuracies
set.seed(12345)
nfolds=1000
true_classes <- levels(abalone$true_class)
cv_index <- cv_cohorts(nrow(abalone),nfolds)
results_list <- list(NULL)
timer <- Sys.time()
for(fold in 1:nfolds){
  train_data <- abalone[cv_index!=fold, ]
  test_data <- abalone[cv_index==fold, ]
  model_list <- make_model_list(model_types, train_data)
  unbinned_results <- unbinned_testing(train_data, test_data, model_list,true_classes)
  results_list[[fold]] <- unbinned_results
}
Sys.time()-timer

results_list_all <- results_list[[1]]
cv_weights <- sapply(1:nfolds, function(x) sum(cv_index==x))
cv_ensemble_accuracies <- sapply(1:nrow(results_list[[1]]), function(i) weighted.mean(sapply(1:nfolds, function(fold) results_list[[fold]]$accuracy)[i,],w=cv_weights))
cv_ensemble_accuracies

set.seed(12345)
cv_test_abalone <- cv_testing_all_ensembles(abalone, model_types, 
                                            bin_features_all=c("V5","V7","V8"), nbins_all=2:4,
                                            equal_bins=FALSE, cv_K=10)

set.seed(12345)
cv_test_abalone_5_7 <- cv_testing_all_ensembles(abalone, model_types, 
                                                  bin_features_all=c("V5","V7"), nbins_all=2,
                                                  equal_bins=FALSE, cv_K=50)

set.seed(12345)
cv_test_abalone_bin_fitted <- cv_testing_all_bin_fitted_ensembles(abalone, model_types, 
                                                                  bin_features_all=c("V5","V7","V8"), nbins_all=2:4,
                                                                  equal_bins=TRUE, cv_K=10, true_classes=true_classes)

# save(cv_test_abalone, cv_test_abalone_bin_fitted, file="abalone_results_all.Rdata")

#-------------------------------------------------------------------------------------

load(file="abalone_results_all.Rdata")

abalone_bin_results <- read.csv("abalone/AbaloneResults.csv",stringsAsFactors = FALSE)
head(abalone_bin_results)
abalone_bin_results <- abalone_bin_results %>%
  mutate(comb_type = factor(comb_type, levels=c("majority vote","average posterior")),
         nbins_name = factor(nbins_name),
         weight_type = factor(weight_type, levels=c("bin weighted","bin dictator")),
         bin_type = factor(bin_type, levels=c("standard","quantile","iterative quantile")))
head(abalone_bin_results)
str(abalone_bin_results)
# add factor with better labels for facet titles
abalone_bin_results$bin_type_pretty <-  factor(abalone_bin_results$bin_type, 
                                               labels=c("Standard Binning - 28 feature pairs","Quantile Binning - 28 feature pairs","Iterative Quantile Binning - 56 (ordered) feature pairs"))
abalone_bin_results$weight_type_pretty <-  factor(abalone_bin_results$weight_type,
                                                  labels=c("Bin Weighted","Bin Dictator Weighted"))


abalone_bin_results %>% 
  group_by(weight_type,comb_type,bin_type,nbins_name) %>%
  summarize(count=n()) %>% 
  as.data.frame()

##### all bin weighted accuracies
unbinned_results <- cv_test_abalone$unbinned_results
unbinned_results$wt <- unbinned_results$weight_type
unbinned_results <- select(unbinned_results, -weight_type)

MVdata<-abalone_bin_results%>%filter(comb_type=="majority vote")
MVgraph<-ggplot()+
  geom_line(aes(x=nbins_name, y=accuracy, group=bin_pair_name), data=MVdata)+
  geom_hline(aes(yintercept=accuracy,color=wt),size=1,data=unbinned_results[unbinned_results$comb_type=="majority vote",])+
  geom_text(aes(x=4.01,y=accuracy+c(-.001,.001),size=16, color=wt,label=str_to_title(wt)),
            hjust=0,data=unbinned_results[unbinned_results$comb_type=="majority vote",])+
  facet_grid(weight_type_pretty~bin_type_pretty)+
  lims(y=range(MVdata$accuracy))+
  labs(title="Voting by Simple Majority",
       subtitle="Accuracy Estimates of Ensembles Employing a Majority Vote to Classify Abalone",
       caption="Estimates Based on 10-Fold Cross Validation",
       x="Number of Bins per Variable in Feature Pair", y="Ensemble Accuracy Rate")+
  theme_bw() + theme(legend.position = "none")
MVgraph

APdata<-abalone_bin_results%>%filter(comb_type=="average posterior")
APgraph<-ggplot()+
  geom_line(aes(x=nbins_name, y=accuracy, group=bin_pair_name), data=APdata)+
  geom_hline(aes(yintercept=accuracy,color=wt),size=1,data=unbinned_results[unbinned_results$comb_type=="average posterior",])+
  geom_text(aes(x=4.1,y=accuracy+c(.001,-.001),size=16,color=wt,label=str_to_title(wt)),
            hjust=1,data=unbinned_results[unbinned_results$comb_type=="average posterior",])+
  facet_grid(weight_type_pretty~bin_type_pretty)+
  lims(y=range(APdata$accuracy))+
  labs(title="Voting with Posterior Probabilities",
       subtitle="Accuracy Estimates of Ensembles Employing a Posterior Probability Voting to Classify Abalone",
       caption="Estimates Based on 10-Fold Cross Validation",
       x="Number of Bins per Variable in Feature Pair", y="Ensemble Accuracy Rate")+
  theme_bw() + theme(legend.position = "none")
APgraph
###################
topdata<-abalone_bin_results%>%
  group_by(weight_type, comb_type, bin_type, nbins_name)%>%
  mutate(percentage=accuracy/max(accuracy))%>%
  filter(percentage==1)

topdata<-topdata[, c(-7)]



unbin_acc <- cv_test_abalone$unbinned_results
## plot 
ggplot()+
  geom_line(aes(x=nbins_name, y=accuracy, linetype=weight_type, color=bin_type, group=interaction(weight_type, bin_type)), data=topdata)+
  facet_grid(comb_type~.) + 
  theme_bw()+
  geom_hline(aes(yintercept = accuracy),size=1,data=unbin_acc)+
  annotate(geom="text",aes(y=accuracy, label=weight_type),x=1,size=1,data=unbin_acc)

  
  
  
library(data.table)
names(abalone) <- c("true_class","length","diameter","height","whole_wgt","shuck_wgt","viscera_wgt","shell_wgt","rings")


iq_def <- iterative_quant_bin(data=abalone, bin_cols=c("viscera_wgt","whole_wgt"),
                    nbins=c(3,2), output="both")

# Plot for ARA application
bin_aggs_bounds <- as.data.frame(iq_def$bin_def$bin_bounds)
ggplot() +
  geom_point(aes(x=viscera_wgt,y=whole_wgt,color=true_class),data=abalone)+
  geom_rect(aes(xmin=V1,xmax=V2,ymin=V3,ymax=V4),fill=NA,color="black",size=.75,data=bin_aggs_bounds) +
  theme_bw() +
  # xlab("Shell Weight (g)") + ylab("Shucked Weight (g)") +
  ggtitle("Iteratively Quantile Binned Abalone Data")


