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

relationality <- function(Xi, Xj){
  # Helper function which calculates relationality
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
  change_x_list = lapply(change_x_list, FUN = function(x) ifelse(x[] > 0, 1, x[]))
  change_x_list = lapply(change_x_list, FUN = function(x) ifelse(x[] < 0, -1, x[]))
  
  distmat <- matrix(nrow = nrow(data), ncol = nrow(data))
  for (i in 1:(nrow(distmat)-1)){
    for ( j in (i+1):nrow(distmat)){
      distmat[i,j] <- relationality(change_x_list[[i]], change_x_list[[j]])
    }
  }
  
  distmat <- f(distmat)
  
  return(distmat)
}

rca.original.square <- function(data){
  # Main function, changes with previous algorithm include: no bootstrapping, subtracting median, rather than mean, relationality value
  # and squaring of relationality matrix rather than absolute value.
  # We found these changes improve accuracy.
  require(igraph)
  require(boot)
  require(lsa)
  require(scales)

  data <- sapply(as.data.frame(data), rescale)

  distmat <- rca.dist(data)

  distmat = distmat - median(distmat, na.rm = T)

  diag(distmat) <- 0

  distmat = distmat * distmat

  net <- graph.adjacency(distmat, mode = "undirected", weighted = TRUE)

  cluster_out <- cluster_louvain(net,  weights = E(net)$weight)

  mod_out <- modularity(cluster_out)

  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$group <- as.vector(membership(cluster_out))

  results <- list(Data = data, Distance_Matrix = distmat, Modularity_Score = mod_out, membership = data$group)

  return(results)
}
