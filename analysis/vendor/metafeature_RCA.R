# This R file loads the packages and functions used in "Coping With Plenitude: A Computational Approach to Selecting the Right Algorithm". 
# It should be run before simulation.R and analyses.R. 
# Functions pertaining to correlation class analysis and the simulations run were graciously provided by Andrei Boutyline.

## Load required packages ##
# List of packages for session
packages_to_load = c("proxy",
			         "DescTools",
			         "mfe",
			         "minerva",
			         "intrinsicDimension",
			         "readstata13",
			         "Rcpp",
			         "fossil",
			         "data.table",
			         "igraph", 
			         "caret",
			         "glmnet",
			         "rbenchmark",
			         "lavaan", 
			         "fpc", 
			         "entropy", 
			         "boot", 
			         "lsa",
			         "scales", 
			     	 "acepack")

# Install CRAN packages (if not already installed)
already_installed <- packages_to_load %in% installed.packages()
if(length(packages_to_load[!already_installed]) > 0) install.packages(packages_to_load[!already_installed])

# Load packages into session 
lapply(packages_to_load, require, character.only = TRUE)

sourceCpp("f_outer.cpp") # compile C++ code and make it available in R

f <- function(m) {
  # Helper function for symmetrizing a matrix.
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}

changeX <- function(string){
  # Helper function for calculating change matrix for two respondents
  changemat <- outer(string,string,"-")
  diag(changemat) <- NA
  return(changemat)
}

gen_change_list <- function(df){
  # Helper function which applies changeX to every pair of df in a data set
  changeX_list <- vector("list", nrow(df))
  for ( i in 1:nrow(df)){
    changeX_list[[i]] <- changeX(df[i,])
  }
  return(changeX_list)
}

recursive_gen_change_list <- function(df){
  changeX_list <- vector("list", nrow(df))
  for ( i in 1:nrow(df)){
    changeX_list[[i]] <- f_outer(f_outer(as.numeric(df[i,])))
  }
  return(changeX_list)
}
relationality <- function(Xi, Xj){
  # Helper function for calculating relationality a la Goldberg 2011
  kx = ncol(Xi)
  ky = ncol(Xj)
  
  lambda <- ifelse(Xi*Xj >= 0, 1, -1)
  
  dist <- 1-abs(abs(Xi) - abs(Xj))
  dist = lambda * dist
  
  to_keep = which(colSums(is.na(dist)) < kx)
  if(length(to_keep) > 0){
    dist = dist[to_keep, to_keep]
  }
  
  kx = ncol(dist)
  
  relationalities = sum(dist, na.rm = TRUE)
  
  relationalities = (relationalities)/(kx*(kx-1))
  
  return(relationalities)
}

rca.dist <- function(data){
  # Helper function for calculating distances between all pairs of individuals
  data <- as.matrix(data)
  change_x_list <- gen_change_list(data)
  distmat <- matrix(nrow = nrow(data), ncol = nrow(data))
  for (i in 1:(nrow(distmat)-1)){
    for ( j in (i+1):nrow(distmat)){
      distmat[i,j] <- relationality(change_x_list[[i]], change_x_list[[j]])
    }
  }
  
  distmat <- f(distmat)
  
  return(distmat)
}

recursive.rca.dist <- function(data){
  
  data <- as.matrix(data)
  change_x_list <- recursive_gen_change_list(data)
  change_x_list = lapply(change_x_list, FUN = function(x) ifelse(x[] > 0, 1, x[]))
  change_x_list = lapply(change_x_list, FUN = function(x) ifelse(x[] < 0, -1, x[]))
  
  distmat <- matrix(nrow = nrow(data), ncol = nrow(data))
  for (i in 1:(nrow(distmat)-1)){
    for( j in (i+1):nrow(distmat)){
      distmat[i,j] <- relationalityC(change_x_list[[i]], change_x_list[[j]])
    }
  }
  
  distmat = f(distmat)
  
  return(distmat)
}


corr.dist <- function (dtf, zero.action = "ownclass") {

  # Floating point imprecision may make 0-variance df appear to have variance slightly higher than 0.
  zeros <- which(apply(dtf, 1, var) <= 0.000000001)
  
  if (zero.action[1] == "drop" & (length(zeros) > 0)) {
    dtf <- dtf[-zeros,]
  }
  
  rv <- abs(cor(t(dtf)))
  
  attributes(rv)$zeros <- zeros
  attributes(rv)$zero.action <- zero.action[1]
  attributes(rv)$dtf <- dtf
  
  if ((zero.action[1] == "ownclass") & length(zeros) > 0) {
    rv[zeros,] <- 0
    rv[,zeros] <- 0
    rv[zeros,zeros] <- 1
  }
  
  diag(rv) <- 0
  
  return (rv)
}

ace.dist <- function (dtf, zero.action = "ownclass") {
  require(acepack)
  
  # Floating point imprecision may make 0-variance df appear to have variance slightly higher than 0.
  zeros <- which(apply(dtf, 1, var) <= 0.000000001)
  
  if (zero.action[1] == "drop" & (length(zeros) > 0)) {
    dtf <- dtf[-zeros,]
  }
  
  distmat <- matrix(nrow = nrow(dtf), ncol = nrow(dtf))
  for (i in 1:(nrow(distmat)-1)){
    for ( j in (i+1):nrow(distmat)){
      distmat[i,j] <- ace(dtf[i,], dtf[j,])$rsq
    }
  }
  
  rv <- f(distmat)
  
  attributes(rv)$zeros <- zeros
  attributes(rv)$zero.action <- zero.action[1]
  attributes(rv)$dtf <- dtf
  
  if ((zero.action[1] == "ownclass") & length(zeros) > 0) {
    rv[zeros,] <- 0
    rv[,zeros] <- 0
    rv[zeros,zeros] <- 1
  }
  
  diag(rv) <- 0
  
  return (rv)
}

filter.insignif <- function (corr, N.vars, pcutoff = 0.05) {
  # Helper function.
  # Filter significances at p <= pcutoff (two-tailed).
  corr <- abs(corr)
  
  if (any(diag(corr) != 0))
    stop("Non-zero elements on the diagonal. diag(corr) <- 0 before running this function.")
  
  suppressWarnings(tvalues <- corr * sqrt ((N.vars-2) / (1 - corr^2)))
  if (any(is.infinite(tvalues))) {
    tvalues[is.infinite(tvalues)] <- 9999 # a very big number
  }
  cutoff <- abs(qt(pcutoff / 2, N.vars))
  
  isolates.pre <- sum(apply(corr, 1, sum) == 0)
  corr[tvalues < cutoff] <- 0
  isolates.post <- sum(apply(corr, 1, sum) == 0)
  
  if (isolates.post > isolates.pre) {
    warn1 <- paste ("Significance filtering left", isolates.post - isolates.pre, "df with no non-zero ties. The CCA result will contain at least one small degenerate class.")
    warning(warn1)
  }
  
  return (corr)
}


evaluate.metafeatures <- function(df, num_vars = 10){

  df <- as.matrix(df)

  right_half_kurt = function(x){

    # Function for calculating right half Kurtosis
    return(Kurt(x[x > mean(x, na.rm = T)]))

  }

  # Function for evaluating the different metafeatures used in the paper
  # Correlation based
  row_cors = cor(t(df))
  mean_row_cor = mean(row_cors, na.rm = T)
  row_cor_kurtosis = Kurt(row_cors)
  right_tail_cor_kurt = Kurt(row_cors[row_cors > mean(row_cors, na.rm = T)])
  
  # matrix based
  overallKurt = Kurt(as.numeric(df), na.rm = T)
  overall_right_kurt = Kurt(df > mean(df, na.rm = T))
  
  overallSD <- sd(df, na.rm = T)
  
  var_col_first_PC <- var(prcomp(df)$x[,1])
  var_col_second_PC <- var(prcomp(df)$x[,2])
  skew_col_first_PC <- Skew(prcomp(df)$x[,1])
  skew_col_second_PC <- Skew(prcomp(df)$x[,2])
  
  var_row_first_PC <- var(prcomp(t(df))$x[,1])
  var_row_second_PC <- var(prcomp(t(df))$x[,2])
  skew_row_first_PC <- Skew(prcomp(t(df))$x[,1])
  skew_row_second_PC <- Skew(prcomp(t(df))$x[,2])
  
  skewness <- Skew(df)
  
  # df based
  df_new = as.data.frame(df)
  
  mean_column_kurt = mean(sapply(df_new, Kurt), na.rm = T)
  
  mean_sd_ratio = mean(sapply(df_new, FUN = function(x) sd(x, na.rm = T))/sd(df, na.rm = T), na.rm = T)
    
  # mfe vals
  df_new$class = sample(1:4, nrow(df_new), replace = T)
  
  nrOut = statistical(class ~ ., df_new, features="nrOutliers", by.class=F)$nrOutliers

  intrinsicDemnsionality <- pcaLocalDimEst(df, ver = 'fan')$dim.est
  intrinsicDemnsionalityProp <- intrinsicDemnsionality/num_vars

  return(data.frame(OverallSD = overallSD, OverallKurt = overallKurt, OverallRightKurt = overall_right_kurt, 
              VarColPC1 = var_col_first_PC, SkewColPC1 = skew_col_first_PC, SkewColPC2 = skew_col_second_PC, 
              Skewness = skewness, 
              Mean_Row_Corr = mean_row_cor, Row_Corr_Kurtosis = row_cor_kurtosis,
              CorrRightKurt = right_tail_cor_kurt, Mean_Col_Kurtosis = mean_column_kurt, Mean_SD_Ratio = mean_sd_ratio, PercentOut = ((nrOut/num_vars) * 100),
              intrinsicDemnsionalityProp = intrinsicDemnsionalityProp))
}

make_prediction = function(coefficient_vals, observed_values) {
  scaled_coefficients = coefficient_vals * c(1, as.numeric(observed_values))
  predict_out = sum(scaled_coefficients)
  return(predict_out)
}
predict.accuracies <- function(models, mf_vals){
	predicted_vals = lapply(models, FUN = function(x) as.numeric(make_prediction(x$coefficients, mf_vals)))
	return(unlist(predicted_vals))
}

select.method = function(data, models, n = 5){
	mf_vals <- evaluate.metafeatures(data)
	accuracies <- predict.accuracies(models, mf_vals)
	accuracies <- sort(accuracies, decreasing = T)
	return(accuracies[1:n])
}

metaRCA <- function(data, measure = "Recursive Relationality"){

  print(paste0("Running RCA with the distance measure ", measure))
	
	data <- as.data.frame(data)
	data_scaled <- data.frame(sapply(data, as.numeric))	
	data_scaled <- rescale.for.RCA(data_scaled)

	if (measure %in% c("eJaccard", "Podani", "Euclidean", "Cosine", "eDice")) {
		if (measure %in% c("eJaccard", "Cosine", "eDice")) { # oca rescale
			data_scaled = apply(data_scaled, 2, FUN = function(x) x - median(min(x):max(x)))
		}
		results = simil(data_scaled, method = measure)
  		results = as.matrix(results)
  		if (measure != "Cosine") {
  			results = results-median(results, na.rm = T)
  		}
	} else if (measure == "Correlation") {
		results = corr.dist(data_scaled)
    results <- filter.insignif (results, ncol(data_scaled), pcutoff = 0.05)
	} else if (measure == "ACE") {
		results = ace.dist(data_scaled)
		results[] = ifelse(is.nan(results[]), 0, results[])
    	results <- filter.insignif (results, ncol(data_scaled), pcutoff = 0.1)
	} else if (measure == "Original Relationality") {
		results = rca.dist(data_scaled)
		results = results - median(results, na.rm = T)
	} else if (measure == "Recursive Relationality") {
		if(nrow(data) > 2000){
			warning("Recursive Relationality is slow and your data is large! Consider using a different measure.")
		}
		results = recursive.rca.dist(data_scaled)
	} else {
		stop("Distance measure must be one of: eJaccard, eDice Podani, Euclidean, Cosine, Correlation, ACE, Original Relationality, or Recursive Relationality")
	}

	results = as.matrix(results)

	diag(results) = 0

	if (measure == "Recursive Relationality") {
		distmat = results * results
	} else {
		distmat = abs(results)
	}

	if (measure == "Original Relationality") {
		distmat[distmat < 0.05] = 0
	} else if (measure == "Cosine"){
		distmat = ifelse(distmat > quantile(distmat, .75, na.rm = T), distmat, 0)
	} else {
		distmat = distmat
	}

  	net <- graph.adjacency(distmat, mode = "undirected", weighted = TRUE)
  
	cluster_out <- cluster_louvain(net,  weights = E(net)$weight)

	mod_out <- modularity(cluster_out)

	data <- as.data.frame(data, stringsAsFactors = FALSE)

	data$group <- as.vector(membership(cluster_out))

	out <- list(Data = data, 
				Distance_Measure = measure,
			  Distance_Matrix = distmat, 
				Modularity_Score = mod_out, 
				membership = as.numeric(membership(cluster_out)))

	return(out)

}

grab_measurements <- function(x, measures = c("eJaccard", "Podani", "Euclidean", 
                                              "Cosine", "eDice", "Correlation", 
                                              "Recursive Relationality", 
                                              "Original Relationality"), 
                                  ensemble = c("Correlation", "Recursive Relationality", "eJaccard", "Podani", "ACE")){

  require(fpc)
  require("lavaan")
  require(parallel)

  x <- na.omit(x)
  zeros <- which(apply(x, 1, var) <= 0.000000001)

  if(length(zeros) > 0) x <- x[-zeros,]

  varnames = colnames(x)
  
  outs <- mclapply(measures, FUN = function(y) metaRCA(x, measure = y), mc.cores = 9)
  names(outs) = measures

  if(length(ensemble) > 0){
    ens_outs = outs[ensemble]
    vec_list = lapply(ens_outs, FUN = function(y) y$membership)
    ensemble_out = dyadic_vote_ensemble(vec_list)
  }

  all_clusterings <- lapply(outs, FUN = function(y) y$membership)

  all_clusterings[[length(all_clusterings) + 1]] = ensemble_out
  names(all_clusterings)[length(all_clusterings)] = "Ensemble"
  measures = names(all_clusterings)

  all_results <- lapply(all_clusterings, FUN = function(y) cluster.stats(abs(cor(t(x))), clustering = y))

  temp_df <- do.call("cbind", all_clusterings)
  colnames(temp_df) = measures

  temp_df = cbind(x, temp_df)

  var_combs <- t(combn(varnames, 2))
  var_combs <- subset(var_combs, var_combs[,1] != var_combs[,2])

  covariance_model <- paste(var_combs[,1], ' ~~ ', var_combs[,2], ';', sep='' )

  overall_model <- sem(model=covariance_model,
                       data = temp_df,
                       check.gradient = FALSE)


  group_models <- vector("list", length(measures))
  for(i in 1:length(measures)){
    group_models[[i]] <- try(sem(model= covariance_model,
                       data = temp_df,
                       group = measures[i],
                       check.gradient = FALSE))
  }

  return(list(ClusterStats = all_results, SEM = group_models, OverallSEM = overall_model, DF = temp_df))
}

dyadic_vote_ensemble = function(vec_list){

  # Ensemble method for RCA
  # For each pair of respondents, counts the number of different methods for which they share an assignment to the same group
  # Results in a matrix, where cell i, j is the number of methods for which people i and j share a group assignment
  # Count matrix is inputted as a weighted network and clustered
  require(igraph)
  require(matrixStats)
  
  if(length(unique(unlist(lapply(vec_list, length)))) != 1){
    print("All vectors must be of the same length")
    return()
  }
  
  mat_compare = function(v1){
    v1_tab = table(1:length(v1), v1)
    v1_mat = v1_tab %*% t(v1_tab)
    return(v1_mat)
  }
  
  vote_comps = Reduce("+", lapply(vec_list, mat_compare))/length(vec_list)
  
  dyad_graph = graph.adjacency(vote_comps, mode = "undirected", weighted = T)
  result = membership(cluster_louvain(dyad_graph))
  
  return(as.numeric(result))

}

rescale.for.RCA <- function(df) {

  # Shift variable range to that expected by RCA (may no longer be necessary).
  if (any(df <= 0)) {
    df <- df - min(df) + 1
  }
  
  return(df)

}

finalized_measures <- function(x, measures = c("corca", "jacca", "dice", "cca", "rca", "pca", "eca", "cosine", "ace", "ensemble")){

  cluster_stats <- lapply(x$ClusterStats, function(y) data.frame(avg.silwidth = abs(y$avg.silwidth),
                                                                 pearsongamma = abs(y$pearsongamma),
                                                                 within.ss = y$within.cluster.ss,
                                                                 entropy = y$entropy,
                                                                 wb.ratio = y$wb.ratio,
                                                                 ch = abs(y$ch),
                                                                 widestgap = y$widestgap))
  cluster_stats <- do.call("rbind", cluster_stats)

  original_aic <- AIC(x$OverallSEM)
  method_aics <- unlist(lapply(x$SEM, function(x) ifelse(class(x) != "try-error", AIC(x), NA)))
  aic_imp <- original_aic-method_aics

  cluster_stats$AIC_IMP <- aic_imp

  cluster_stats$Measure <- measures

  return(cluster_stats)
}

models = readRDS("metafeature_models_cleaned.RDS")
models_w_noise = readRDS("models_w_noise.RDS")

# example_data = lapply(1:10, FUN = function(x) sample(1:4, 100, replace = T))
# example_data = do.call("cbind", example_data)

# predictions = predict.accuracies(evaluate.metafeatures(example_data), models)
# top_5 = select.method(example_data, models, n = 5)

# rca_result = metaRCA(example_data, measure = names(top_5[1]))
