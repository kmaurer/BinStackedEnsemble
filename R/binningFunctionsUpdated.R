#' Create feature pair data frame
#'
#' @description From provided feature pairs, create a data frame that holds additional information.
#'
#' @param binnedFeatures The features to bin on.  Must be two!
#' @param bin_type The type of bin to use
#' @param nbins The number of bins to use
#' @return \code{featurePairs}
make_feature_pair_df <- function(binnedFeatures, bin_type, nbins){
  featurePairs <- as.data.frame(t(binnedFeatures))
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
#' @param binnedFeatures Variables to bin on, must be numeric
#' @param bin_type "standard", "quantile", "both"
#' @param nbins The number of bins to create
#'
#' @return Return the training data with bin centers added.
add_bin_features <- function(dat,binnedFeatures,bin_type="both", nbins){
  if(length(binnedFeatures) > 0){
    # Create bins for numberic features as specified with (bin_type,nbinFeatures,nbins)
    if(bin_type == "standard" | bin_type=="both"){
      bin_dat <-  as.data.frame( sapply(dat[,binnedFeatures],function(x) rect_bin_1d(x,min(x),diff(range(x))/nbins,output="centers") ) )
      names(bin_dat) <- paste(names(bin_dat),"_standard_binned_",nbins,sep="")
      dat <- cbind(dat, bin_dat)
    }
    if(bin_type == "quantile" | bin_type=="both"){
      bin_dat <- as.data.frame( sapply(dat[,binnedFeatures],function(x) quant_bin_1d(x,nbins,output="centers")) )
      names(bin_dat) <- paste(names(bin_dat),"_quantile_binned_",nbins,sep="")
      dat <- cbind(dat, bin_dat)
    }
  }
  return(dat)
}

#' Standard rectangular 1d binning
#'
#' @description Standard rectangular 1d binning
rect_bin_1d <- function(xs, origin, width, output="centers"){
  bin_bounds <- origin + width*(0:(ceiling(diff(range(xs))/width)))
  bin_centers <- origin + width*(1:( ceiling(diff(range(xs))/width)) - 0.5)
  data_bins <- rep(bin_centers[1],length(xs))
  for (i in 2:length(bin_centers)){
    data_bins[bin_bounds[i] < xs] <- bin_centers[i]
  }
  if(output=="centers") return(data_bins)
  if(output=="definition") return(list(bin_centers=round(bin_centers,10),bin_bounds=round(bin_bounds,10)))
  if(output=="both") return(list(data_bins=data_bins,bin_centers=round(bin_centers,10),bin_bounds=round(bin_bounds,10)))
}

#' Quantile 1d binning
#'
#' @description Used for binning the counts values by quantile
#' define vector of counts and number of bin
#'
quant_bin_1d <- function(xs, nbin, output="centers"){
  quants <- quantile(xs, seq(0, 1, by=1/(2*nbin)))
  bin_centers <- quants[seq(2,length(quants)-1, by=2)]
  bin_bounds <- quants[seq(1,length(quants)+1, by=2)]
  data_bins <- rep(bin_centers[1],length(xs))
  for (i in 2:length(bin_centers)){
    data_bins[bin_bounds[i] < xs] <- bin_centers[i]
  }
  if(output=="centers") return(data_bins)
  if(output=="definition") return(list(bin_centers=round(bin_centers,10),bin_bounds=round(bin_bounds,10)))
  if(output=="both")  return(list(data_bins=data_bins,bin_centers=round(bin_centers,10),bin_bounds=round(bin_bounds,10)))
}

#' Univariate binning
#'
#' @description Univariate binning based on bins defined by other data set
#'
bin_by_definition <- function(new_data_vec, bin_definition){
  new_data_bins <- rep(bin_definition$bin_centers[1], length(new_data_vec))
  for(i in 2:length(bin_definition$bin_centers)){
    new_data_bins[new_data_vec > bin_definition$bin_bounds[i]] <- bin_definition$bin_centers[i]
  }
  names(new_data_bins) <- names(new_data_vec)
  return(new_data_bins)
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
  binnedFeatures <- c(levels(featurePairs[,1])[as.numeric(featurePairs[,1])], levels(featurePairs[,2])[as.numeric(featurePairs[,2])])
  bin_features <- as.character(featurePairs[3:4])
  if(length(binnedFeatures) > 0){
    # Create bins for numberic features as specified with (binType,nbinFeatures,nbins)
    if(bin_type == "standard" ){
      all_bin_defs <- lapply(train_data[,binnedFeatures],function(x) rect_bin_1d(x,min(x),diff(range(x))/nbins,output="definition") )
    }
    if(bin_type == "quantile" ){
      all_bin_defs <- lapply(train_data[,binnedFeatures],function(x) quant_bin_1d(x,nbins,output="definition"))
    }
  }

  # Index bins
  bin_feature_index <- tidyr::separate(data.frame(bin_cross = levels(interaction(all_bin_defs[[as.character(binnedFeatures[1])]]$bin_centers,
                                                                 all_bin_defs[[as.character(binnedFeatures[2])]]$bin_centers, sep="---"))),
                                bin_cross, into=bin_features, sep="---" , convert=TRUE)
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
bin_test_by_train <- function(train_data,test_data,binnedFeatures,bin_type, nbins){
  bin_test <- sapply(binnedFeatures, function(x){
    if(bin_type == "standard") bin_definition <- rect_bin_1d(train_data[,x],min(train_data[,x]),(diff(range(train_data[,x])))/nbins,"definition")
    if(bin_type == "quantile") bin_definition <- quant_bin_1d(train_data[,x],nbins,"definition")
    bin_by_definition(test_data[,x],bin_definition)
  })
  colnames(bin_test) <- paste(colnames(bin_test),"_",bin_type,"_binned_",nbins,sep="")
  return(bin_test)
}

