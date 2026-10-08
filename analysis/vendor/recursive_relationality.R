sourceCpp("f_outer.cpp") # compile C++ code and make it available in R

recursive_gen_change_list <- function(df){
  # Helper function which recursively calculates change matrix by subtracting every change matrix value from every other
  # Size of change matrix grows exponentially, generally only one additional recursion improves accuracy
  changeX_list <- vector("list", nrow(df))
  for ( i in 1:nrow(df)){
    changeX_list[[i]] <- f_outer(f_outer(as.numeric(df[i,])))
  }
  return(changeX_list)
}

corca.dist.sotoudeh <- function(data){
  # Helper function to calculate similarity between all pairs of respondents using recursive relationality
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

recursive_RCA <- function(data){
  # Main function: similar to original RCA, with the important difference that the change matrix for each individual is recursively calculated, generally twice before diminishing returns
  require(igraph)
  require(boot)
  require(lsa)
  require(scales)

  distmat <- corca.dist.sotoudeh(data)

  diag(distmat) <- 0

  distmat = distmat * distmat

  net <- graph.adjacency(distmat, mode = "undirected", weighted = TRUE)

  cluster_out <- cluster_louvain(net,  weights = E(net)$weight)

  mod_out <- modularity(cluster_out)

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$group <- as.vector(membership(cluster_out))

  results <- list(Data = data, Distance_Matrix = distmat, Modularity_Score = mod_out, membership = as.numeric(membership(cluster_out)))

  return(results)
}
