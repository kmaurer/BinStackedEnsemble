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

#packages needed
library(gridExtra)
library(tidyverse)
library(stringr)
library(data.table)

# data and results needed
abalone <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')
abalone<-as.data.frame(abalone)
names(abalone)[1]<-"true_class"
abalone$true_class<-as.factor(abalone$true_class)
true_classes <- levels(abalone$true_class)

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
                                                  labels=c("Bin Accuracy Weighted","Bin Dictator Weighted"))

### JSM Plots ###
# Need example for building binweighted ensemble step by step
set.seed(12345)
train_index <- sample(1:nrow(abalone), round(nrow(abalone)*9/10))
train <- abalone[train_index,]
test <- abalone[-train_index,]

# Make 4 models using RWeka: Naive Bayes, Random Forest, Bagging and Support Vector Machine 
model_types <- c("weka.classifiers.bayes.NaiveBayes","weka.classifiers.trees.RandomForest",
                 "weka.classifiers.meta.Bagging","weka.classifiers.functions.SMO")
modelList <- make_model_list(model_types, train)
M=4
K=3
true_classes <- levels(abalone$true_class)

# Specify binning parameters
comb_rule <- "majority vote"
bin_type <- "standard"
weight_type <- "bin weighted"
bin_features <- c("V8","V2")
nbins <- c(2,2)

# start 10-fold CV of ensemble partition into ensemble training and ensemble testing data
# 
# make 10-fold cross validated predictions from each member WITHIN training data on which to evaluate accuracy used in bin weights
train_preds <- make_train_preds(train,modelList,true_classes)
ensemble <- make_ensemble(train_preds = train_preds, model_list = modelList, weightType = weight_type,
                          comb_rule = comb_rule, bin_type = bin_type, bin_features = bin_features, nbins = nbins)

# Calculate the bin weights
bin_train <- bin_nd(data=train_preds, bin_features=bin_features, nbins=nbins, bin_type=bin_type, output="both")

## Collect training accurcies of each bin using the cross validated predictions in train_data_preds
# any region without existing data differs to overall model accuracies for weights
B = nrow(bin_train$bin_def$bin_centers)
model_accuracies <- sapply(paste("preds",1:M,sep=""), function(x){
  sum(train_preds$true_class==train_preds[,x])/nrow(train_preds)
})
bin_accuracy_array <- matrix(rep(model_accuracies,B),c(M,B), dimnames=list(1:M,1:B))
for(m in 1:M){
  for(b in unique(bin_train$bin_data$bin_index)){
    inBin <- which(bin_train$bin_data$bin_index==b)
    bin_accuracy_array[m,as.numeric(as.character(b))] <- sum(train_preds$true_class[inBin]==train_preds[,paste("preds",m,sep="")][inBin])/length(inBin)
  }
}
bin_acc_V8_V2 <- data.frame(bin_index=1:4,bin_accuracy_array) %>%
  # rename(C1=X1,C2=X2,C3=X3,C4=X4) %>%
  gather(key="model",value="accuracy", X1:X4) %>%
  mutate(model=as.numeric(as.factor(model)))

bin_acc_V8_V2 <- left_join(bin_acc_V8_V2,bin_train$bin_def$bin_centers, by="bin_index")
# bin_bounds_V8_V2 <- bin_train$bin_def$bin_bounds

# Apply bin weights and member predictions to ensemble testing data to form ensemble predictions
predictEnsemble(ensemble, test)



### Plot the bin accuracies
# use training data from V8_V2 example
train_V8_V2 <- train
names(train_V8_V2) <- c("true_class","length","diameter","height","whole_wgt","shuck_wgt","viscera_wgt","shell_wgt","rings")
train_V8_V2$true_class <- factor(train_V8_V2$true_class, levels=c("I","F","M"))
levels(train_V8_V2$true_class) <- c("Infantile","Adult Female","Adult Male")
head(train_V8_V2)



load(file="data_for_jsm_plots.Rdata")
library(tidyverse)
library(stringr)

# plot 2X2 binning of shell weight and length for example 
# all measurements were divided by 200 for "ANN scaling purposes"
ggplot() +
  geom_point(aes(x=shell_wgt*200,y=length*200,color=true_class),data=train_V8_V2)+
  theme_bw() +
  labs(x="Shell Weight (g)",y="Shell Length (mm)",
       title="Binned Abalone Data",
       subtitle="Partitioning Feature Space with Shell Weight by Length Bins") +
  geom_hline(yintercept = mean(range(train_V8_V2$length*200)))+
  geom_vline(xintercept = mean(range(train_V8_V2$shell_wgt*200)))+
  scale_color_brewer("Sex",palette="Set2")

# organize label layer data
bin_acc_labels <- bin_acc_V8_V2 %>%
  mutate(mod_acc = paste0(" ~omega[",model,bin_index,"] == ~'",round(accuracy,2),"'")) %>%
  rename(shell_wgt=V8,length=V2)
text_shift <- .025
bin_acc_labels$length_jit <- bin_acc_labels$length + rep(c(-2*text_shift,-text_shift,0,text_shift),each=4)


# Add bin weights in text to same plot for next slide
ggplot() +
  geom_point(aes(x=shell_wgt*200,y=length*200,color=true_class),data=train_V8_V2)+
  theme_bw() +
  labs(x="Shell Weight (g)",y="Shell Length (mm)",
       title="Binned Abalone Data",
       subtitle="Partitioning Feature Space with Shell Weight by Length Bins") +
  geom_hline(yintercept = mean(range(train_V8_V2$length*200)))+
  geom_vline(xintercept = mean(range(train_V8_V2$shell_wgt*200)))+
  geom_text(aes(x=shell_wgt*200,y=length_jit*200,label=mod_acc),size=6,color="blue",data=bin_acc_labels,parse=TRUE) +
  scale_color_brewer("Sex",palette="Set2")




# library(data.table)
# abalone <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')
# abalone<-as.data.frame(abalone)
# names(abalone)[1]<-"true_class"
# abalone$true_class<-as.factor(abalone$true_class)
# true_classes <- levels(abalone$true_class)
# names(abalone) <- c("true_class","length","diameter","height","whole_wgt","shuck_wgt","viscera_wgt","shell_wgt","rings")
# abalone$true_class <- factor(abalone$true_class, levels=c("I","F","M"))
# levels(abalone$true_class) <- c("Infantile","Adult Female","Adult Male")
#
# 
# 
# 
# JSM_results_data<-abalone_bin_results%>%filter(comb_type=="majority vote", weight_type=="bin weighted",bin_type=="standard" )

# simplified version of accuracy for JSM with only standard binning and bin weighting and majority vote
# unbinned_results_jsm <- cv_test_abalone$unbinned_results %>%
#   mutate(wt=weight_type) %>%
#   select(-weight_type) %>%
#   filter(comb_type=="majority vote")

# Use example binning with shell weight (V8) X whole weight (V2) bins
shell_wgt_results <- JSM_results_data[JSM_results_data$bin_pair_name=="V8 X V2",]
shell_wgt_results
ggplot()+
  geom_line(aes(x=nbins_name, y=accuracy, group=bin_pair_name),color="blue",size=1.1, data=shell_wgt_results)+
  geom_hline(aes(yintercept=accuracy,color=wt),size=2,data=unbinned_results_jsm)+
  geom_text(aes(x=3,y=accuracy+c(-.001,.001),size=16, color=wt,label=paste(str_to_title(wt),"Ensemble")),
            hjust=0,data=unbinned_results_jsm[unbinned_results_jsm$comb_type=="majority vote",])+
  lims(y=range(JSM_results_data$accuracy))+
  labs(title="Binned-Weighted Ensemble Accuracy",
       subtitle="Partitioning Feature Space with Shell Weight by Length Bins",
       caption="Estimates Based on 10-Fold Cross Validation",
       x="Number of Binned Partitions", y="Ensemble Accuracy Rate")+
  theme_bw() + theme(legend.position = "none") +
  geom_point(aes(x=nbins_name, y=accuracy),color="blue",size=4,data=shell_wgt_results[which.max(shell_wgt_results$accuracy),])+
  geom_text(aes(x=nbins_name, y=accuracy),color="blue",hjust=-.03,vjust=.3, label="Accuracy of Example \n     from Previous Slides",data=shell_wgt_results[which.max(shell_wgt_results$accuracy),])

# show all bin-weighted ensemble accuracies for standard binning and majority vote

ggplot()+
  geom_line(aes(x=nbins_name, y=accuracy, group=bin_pair_name),size=.7, data=JSM_results_data)+
  geom_line(aes(x=nbins_name, y=accuracy, group=bin_pair_name),color="blue",size=1.1, data=shell_wgt_results)+
  geom_hline(aes(yintercept=accuracy,color=wt),size=2,data=unbinned_results_jsm)+
  geom_text(aes(x=3,y=accuracy+c(-.001,.001),size=16, color=wt,label=paste(str_to_title(wt),"Ensemble")),
            hjust=0,data=unbinned_results_jsm[unbinned_results_jsm$comb_type=="majority vote",])+
  geom_point(aes(x=nbins_name, y=accuracy),color="blue",size=4,data=shell_wgt_results[which.max(shell_wgt_results$accuracy),])+
  lims(y=range(JSM_results_data$accuracy))+
  labs(title="Binned-Weighted Ensemble Accuracy",
       subtitle="For all 28 possible binned pairs of features",
       caption="Accuracy Estimates Based on 10-Fold Cross Validation",
       x="Number of Binned Partitions", y="Ensemble Accuracy Rate")+
  theme_bw() + theme(legend.position = "none")


# save(abalone, train_V8_V2,bin_acc_labels,JSM_results_data,unbinned_results_jsm,bin_acc_V8_V2,file="data_for_jsm_plots.Rdata")



#----------------------------------------------------------------------------------------------------







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
nsimloops=20
cv_ensemble_accuracies <- matrix(rep(NA,4*nsimloops),nrow=nsimloops)
for(j in 1:nsimloops){
nfolds=10
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
print(Sys.time()-timer)
cv_weights <- sapply(1:nfolds, function(x) sum(cv_index==x))
cv_ensemble_accuracies[j,] <- sapply(1:nrow(results_list[[1]]), function(i) weighted.mean(sapply(1:nfolds, function(fold) results_list[[fold]]$accuracy)[i,],w=cv_weights))
print(j)
}
##### all bin weighted accuracies
unbinned_results <- cv_test_abalone$unbinned_results
unbinned_results$wt <- unbinned_results$weight_type
unbinned_results <- select(unbinned_results, -weight_type)
unbinned_results$accuracy <- sapply(1:4, function(x) mean(cv_ensemble_accuracies[,x]))


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
                                                  labels=c("Bin Accuracy Weighted","Bin Dictator Weighted"))

# save(abalone_bin_results,unbinned_results,file="abalone_jsm2017.Rdata")

abalone_bin_results %>% 
  group_by(weight_type,comb_type,bin_type,nbins_name) %>%
  summarize(count=n()) %>% 
  as.data.frame()

unbinned_results <- cv_test_abalone$unbinned_results %>%
  mutate(wt=weight_type) %>%
  select(-weight_type)

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

  
############################################## abalone data
library(data.table)
abalone <- fread('https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data')
abalone<-as.data.frame(abalone)
names(abalone)[1]<-"true_class"
abalone$true_class<-as.factor(abalone$true_class)
true_classes <- levels(abalone$true_class)
  
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


