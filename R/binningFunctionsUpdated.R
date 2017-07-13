#' Standard rectangular 1d binning
#'
#' @description Standard rectangular 1d binning
rect_bin_1d <- function(xs, origin, width, output="data"){
  bin_bounds <- origin + width*(0:(ceiling(diff(range(xs))/width)))
  bin_centers <- origin + width*(1:( ceiling(diff(range(xs))/width)) - 0.5)
  bin_data <- rep(bin_centers[1],length(xs))
  for (i in 2:length(bin_centers)){
    bin_data[bin_bounds[i] < xs] <- bin_centers[i]
  }
  if(output=="data") return(bin_data)
  if(output=="definition") return(list(bin_centers=round(bin_centers,10),bin_bounds=round(bin_bounds,10)))
  if(output=="both") return(list(bin_data=bin_data,bin_centers=round(bin_centers,10),bin_bounds=round(bin_bounds,10)))
}

#' Quantile 1d binning
#'
#' @description Used for binning the counts values by quantile
#' define vector of counts and number of bin
#'
quant_bin_1d <- function(xs, nbin, output="data",jit=0){
  if(jit > 0)  xs <- xs + runif(length(xs),-jit,jit)
  quants <- quantile(xs, seq(0, 1, by=1/(2*nbin)))
  bin_centers <- quants[seq(2,length(quants)-1, by=2)]
  bin_bounds <- quants[seq(1,length(quants)+1, by=2)]
  if(jit > 0) bin_bounds[c(1,length(bin_bounds))] <- bin_bounds[c(1,length(bin_bounds))]+c(-jit,jit)
  bin_data <- rep(bin_centers[1],length(xs))
  if(output=="definition") {
    return(list(bin_centers=bin_centers,bin_bounds=bin_bounds))
  } else{
    for (i in 2:length(bin_centers)){
      bin_data[bin_bounds[i] < xs] <- bin_centers[i]
    } 
    if(output=="data") return(bin_data)
    if(output=="both") return(list(bin_data=bin_data,bin_centers=bin_centers,bin_bounds=bin_bounds))
  }
}

#' Univariate binning
#'
#' @description Univariate binning based on bins defined by other data set
#'
bin_1d_by_def <- function(new_data_vec, bin_definition){
  new_data_bins <- rep(bin_definition$bin_centers[1], length(new_data_vec))
  for(i in 2:length(bin_definition$bin_centers)){
    new_data_bins[new_data_vec > bin_definition$bin_bounds[i]] <- bin_definition$bin_centers[i]
  }
  names(new_data_bins) <- names(new_data_vec)
  return(new_data_bins)
}

#' Make multidimensional binned data or bin definitions (for independent dimensions)
#'
#' @description From provided bin features, create multidimensional bin definition
#'
#' @param data data used to define bins
#' @param bin_features The features to bin on.  Must be two!
#' @param bin_type The type of bin to use
#' @param nbins The number of bins to use
bin_nd <- function(data, bin_features, nbins, bin_type="standard", output="definition"){
  if(length(bin_features)!=length(nbins)){
    print("Must specify a number of bins for each bin feature")
    return(NULL)
  }
    bin_data <- data.frame(rep(NA,nrow(data)))
    bin_centers=list(NULL)
    bin_bounds=list(NULL)
    for(j in 1:length(nbins)){
      xs <- data[,bin_features[j]]
      #!# update this to use output="both" more efficiently
      if(bin_type=="standard"){
        bin_data[,j] <- rect_bin_1d(xs, min(xs), diff(range(xs))/nbins[j], output="data")
        bin_def_j <- rect_bin_1d(xs, min(xs), diff(range(xs))/nbins[j], output="definition")
      } 
      if(bin_type=="quantile"){
        bin_data[,j] <- quant_bin_1d(xs, nbins[j], output="data")
        bin_def_j <- quant_bin_1d(xs, nbins[j], output="definition")
      }
      bin_centers[[j]] <- bin_def_j$bin_centers
      bin_bounds[[j]] <- bin_def_j$bin_bounds
    }
    # make dataframe of multivariate bin centers
    bin_centers <- expand.grid(bin_centers)
    # match names of columns and list objects to bin_features
    names(bin_data) <- bin_features
    names(bin_bounds) <- bin_features
    names(bin_centers) <- bin_features
    # add index to bin centers
    bin_centers$bin_index <- 1:nrow(bin_centers)
    bin_def <- list(bin_features=bin_features, bin_centers=bin_centers, bin_bounds=bin_bounds)
    bin_data <- merge(round_df(bin_data,6),round_df(bin_centers,6), all.x=TRUE)
    # return proper
    if(output=="bin_data") return(bin_data)
    if(output=="definition") return(bin_def)
    if(output=="both") return(list(bin_data=bin_data,bin_def=bin_def))
}
# bin_nd(data=iris, bin_features=c("Petal.Length","Petal.Width","Sepal.Width"), nbins=c(2,3,4), bin_type="quantile", output="bin_data") 
# bin_nd_def <- bin_nd(iris, c("Petal.Length","Petal.Width","Sepal.Width"),nbins=c(2,3,4), bin_type="standard", output="definition")
# bin_nd(iris, c("Petal.Length","Petal.Width","Sepal.Width"), c(2,3,4), bin_type="standard", output="both")



#' Bin new data based on n-dimensional binning definition
#'
#' @description From provided bin features, create multidimensional bin definition
#'
#' @param new_data data being binned
#' @param bin_nd_def binning definition based on the bin_nd function
bin_nd_by_def <- function(new_data, bin_nd_def){
  bin_data <- sapply(bin_nd_def$bin_features, function(x){
    bin_1d_by_def(new_data[,x],bin_definition=list(bin_centers=unique(bin_nd_def$bin_centers[,x]),bin_bounds=bin_nd_def$bin_bounds[[x]]))
  })
  bin_indeces <- merge(bin_data, bin_nd_def$bin_centers, all.x=TRUE)[["bin_index"]]
 return(list(data=new_data, bin_data=bin_data,bin_indeces=bin_indeces))
}
# bin_nd_def <- bin_nd(iris[-c(1:10),], c("Petal.Length","Petal.Width","Sepal.Width"), c(2,3,4), bin_type="standard", output="definition")
# bin_nd_by_def(iris[1:10,],bin_nd_def)



#------------------------------------------------------------------------------------------------



#' Create feature pair data frame
#'
#' @description Based on https://gist.github.com/avsmith/e6f4f654451da139230b to round all numeric variables
#' @param x data frame 
#' @param digits number of digits to round
#' 
round_df <- function(x, digits=2) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


#' Create feature pair data frame
#'
#' @description From provided feature pairs, create a data frame that holds additional information.
#'
#' @param bin_features The features to bin on.  Must be two!
#' @param bin_type The type of bin to use
#' @param nbins The number of bins to use
#' @return \code{featurePairs}
make_feature_pair_df <- function(bin_features, bin_type, nbins){
  featurePairs <- as.data.frame(t(bin_features))
  if(bin_type == "standard" | bin_type=="both"){
    featurePairs$fullLabel1 <- paste(featurePairs[,1],"_standard_binned_",nbins,sep="")
    featurePairs$fullLabel2 <- paste(featurePairs[,2],"_standard_binned_",nbins,sep="")
  }
  if(bin_type == "quantile" | bin_type=="both"){
    featurePairs$fullLabel1 <- paste(featurePairs[,1],"_quantile_binned_",nbins,sep="")
    featurePairs$fullLabel2 <- paste(featurePairs[,2],"_quantile_binned_",nbins,sep="")
  }
  return(featurePairs)
}



#' Add bin centers for selected variables
#'
#' @description Add bin centers for selected variables
#'
#' @param dat Data that will have bin centers added
#' @param bin_features Variables to bin on, must be numeric
#' @param bin_type "standard", "quantile", "both"
#' @param nbins The number of bins to create
#'
#' @return Return the training data with bin centers added.
add_bin_features <- function(dat,bin_features,bin_type="both", nbins){
  if(length(bin_features) > 0){
    # Create bins for numberic features as specified with (bin_type,nbinFeatures,nbins)
    if(bin_type == "standard" | bin_type=="both"){
      bin_dat <-  as.data.frame( sapply(dat[,bin_features],function(x) rect_bin_1d(x,min(x),diff(range(x))/nbins,output="centers") ) )
      names(bin_dat) <- paste(names(bin_dat),"_standard_binned_",nbins,sep="")
      dat <- cbind(dat, bin_dat)
    }
    if(bin_type == "quantile" | bin_type=="both"){
      bin_dat <- as.data.frame( sapply(dat[,bin_features],function(x) quant_bin_1d(x,nbins,output="centers")) )
      names(bin_dat) <- paste(names(bin_dat),"_quantile_binned_",nbins,sep="")
      dat <- cbind(dat, bin_dat)
    }
  }
  return(dat)
}



#' Make bin index
#'
#' @description make bin index set for ALL POSSIBLE bins in training data
#' featPair <- featurePairs[1,] TODO NEED TO FIX INDEXING FOR BINS, CANNOT SKIP STORAGE OF BIN INDECES THAT ARE EMPTY SINCE THEY ARE NEEDED FOR NEW OBSERVATIONS
#' featPair <- featurePairs[1,]   FEATURE PAIRS ONLY HOLDS ONE NOW
#'
#' @param train_data Training data from which to build bins
#' @param featurePairs Data frame with one row that has original binning features' names, and then augmented feature names
#' @param bin_type "standard", "quantile"
#' @param nbins The number of bins
make_train_bins_index <- function(train_data, featurePairs, bin_type="standard", nbins){
  bin_features <- c(levels(featurePairs[,1])[as.numeric(featurePairs[,1])], levels(featurePairs[,2])[as.numeric(featurePairs[,2])])
  bin_labels <- as.character(featurePairs[3:4])
  if(length(bin_features) > 0){
    # Create bins for numberic features as specified with (binType,nbinFeatures,nbins)
    if(bin_type == "standard" ){
      all_bin_defs <- lapply(train_data[,bin_features],function(x) rect_bin_1d(x,min(x),diff(range(x))/nbins,output="definition") )
    }
    if(bin_type == "quantile" ){
      all_bin_defs <- lapply(train_data[,bin_features],function(x) quant_bin_1d(x,nbins,output="definition"))
    }
  }

  # Index bins
  bin_feature_index <- tidyr::separate(data.frame(bin_cross = levels(interaction(all_bin_defs[[as.character(bin_features[1])]]$bin_centers,
                                                                 all_bin_defs[[as.character(bin_features[2])]]$bin_centers, sep="---"))),
                                bin_cross, into=bin_labels, sep="---" , convert=TRUE)
  bin_feature_index$index <- as.factor(1:(nrow(bin_feature_index)))
  return(bin_feature_index)
}

#' Bin test data
#'
#' @description Bin test data based on parameters defined by training data
#' this will be used to assign values for test data sets to bins defined by training data
#'
#' @param train_data Training data
#' @param test_data Test data
bin_test_by_train <- function(train_data,test_data,bin_features,bin_type, nbins){
  bin_test <- sapply(bin_features, function(x){
    if(bin_type == "standard") bin_definition <- rect_bin_1d(train_data[,x],min(train_data[,x]),(diff(range(train_data[,x])))/nbins,"definition")
    if(bin_type == "quantile") bin_definition <- quant_bin_1d(train_data[,x],nbins,"definition")
    bin_1d_by_def(test_data[,x],bin_definition)
  })
  colnames(bin_test) <- paste(colnames(bin_test),"_",bin_type,"_binned_",nbins,sep="")
  return(bin_test)
}

