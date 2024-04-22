library(igraph)
library(MASS)
library(Matrix)
library(foreach)
library(doParallel)
library(parallel)

no_cores <- 7
cl <- makeCluster(no_cores)
load("/home/tianxili/qim/Mingyu_Network_KS_simulation/function.rda")
registerDoParallel(cl)

set.seed(301518)
my_graphon <- graphons$my_graphon3
pscale <- pscale_vec[1]



K <- nsub
setrun <- 1:length(bset)



results <- foreach(l = FL, .combine = 'c', .packages = c("MASS", "Matrix", "igraph")) %dopar% {
 
  subsampledataIter <- list()
  max_min_SIter <- list()
  subsamplestIter <- list()
  max_s_temp <- list()
  min_s_temp <- list()
  
  for (i in setrun) { 
    
    b <- bset[i]
    n <- nset[i]
    
    adjacency_matrix <- generate_random_graph(cscale, pscale, my_graphon, n)
    colnames(adjacency_matrix) <- 1:n
    rownames(adjacency_matrix) <- 1:n
    
    rho_hat_G <- sum(adjacency_matrix)/(n*(n-1))
    networkmomentG <- graph_to_stats(adjacency_matrix, rho_hat_G, n)$UR

    subsampleperIter <- matrix(0, K, 4)
    
    for (k in 1:K) {
      nodesamplel <- sample(colnames(adjacency_matrix), b, replace = FALSE)
      nodesamplel <- as.character(nodesamplel)
      A <- adjacency_matrix[nodesamplel, nodesamplel]
      subsampleperIter[k, ] <- graph_to_stats(A, rho_hat_G, b)$UR
      
    }
    
    colnames(subsampleperIter) <- c("v", "triangle", "threestar","square")
    subsampledataIter[[i]] <- subsampleperIter
    
    mysubsampledata <- sweep(subsampleperIter, 2, networkmomentG)
    mysubsampledata <- sweep(mysubsampledata, 2, c(rho_hat_G^(-2), rho_hat_G^(-3), rho_hat_G^(-3),rho_hat_G^(-4)), `*`)
    mysubsampleda <- sqrt(b)*mysubsampledata
    subsamplestIter[[i]] <- mysubsampleda
    max_s_temp[[i]] <- apply(mysubsampleda, 2, max)
    min_s_temp[[i]] <- apply(mysubsampleda, 2, min)
  }
  
  maxs_temp <- do.call(rbind, max_s_temp)
  mins_temp <- do.call(rbind, min_s_temp)
  resmaxmin <- list(max = apply(maxs_temp, 2, max), min = apply(mins_temp, 2, min))
   # Instead of returning the list, save it directly
  save(subsampledataIter, file = paste0("suppledata/subsampledataIter_l", l, ".RData"))
  save(resmaxmin, file = paste0("suppledata/max_min_SIter_l", l, ".RData"))
  save(subsamplestIter, file = paste0("suppledata/subsamplestIter_l", l, ".RData"))
  
  # Return something simple to ensure that the loop completes
  NULL
}
