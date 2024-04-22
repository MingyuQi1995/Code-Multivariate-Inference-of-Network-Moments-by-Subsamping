library(MASS)
library(Matrix)
library(igraph)
library(foreach)
library(doParallel)
library(parallel)

no_cores <- 100
cl <- makeCluster(no_cores)
registerDoParallel(cl)

load("/home/tianxili/qim/Mingyu_Network_KS_simulation/function.rda")


set.seed(3025516)
my_graphon <- graphons$my_graphon3
pscale <- pscale_vec[2]




graphondata <- list()
GRDat <- list()
graphonst <- list()
max_min_X <- list()

K <- nmc

setrun <- 1:length(bset)


for (i in setrun) {
  
  print(i)
  
  b <- bset[i]
  n <- nset[i]
  constant <- 1 - (b/n)
  rm(n)
  my_c <- sqrt(b*constant)
  
  # Inner loop - run in parallel
  inner_results <- foreach(k = 1:K, .combine = 'rbind', .packages = c("MASS", "Matrix", "igraph")) %dopar% {
    
    A <- generate_random_graph(s = cscale, pscale = pscale, my_graphon = my_graphon, graphsize = b)
    
    rho_hat_G <- sum(A)/(b*(b - 1))
    #rho_hat_G <- cscale*b^{pscale}
    
    res <- graph_to_stats(A, rho_hat_G, b)
    
    return(list(scaleUR = res$scaleUR, UR = res$UR))
  }
  
  
  # Process the results of the inner loop
  graphondata_tmp <- do.call(rbind,inner_results[,1])
  GRDat_tmp <- do.call(rbind,inner_results[,2])
  
  graphondata[[i]] <- graphondata_tmp
  GRDat[[i]] <- GRDat_tmp
  
  colnames(graphondata_tmp) <- c("v","triangle","threestar","square")
  colnames(GRDat_tmp) <- c("v","triangle","threestar","square")
  
  netmom <- colMeans(GRDat_tmp)
  rho_b <- cscale*b^{pscale}
  rho_bEhat <- netmom*c(rho_b^(-2),rho_b^(-3),rho_b^(-3),rho_b^(-4))
  mygraphondat <- sweep(graphondata_tmp, 2, rho_bEhat)
  mygraphonda <- mygraphondat*my_c 
  
  graphonst[[i]] <- mygraphonda
  max_min_X_tmp <- list(max = apply(mygraphonda,2,max), min = apply(mygraphonda,2,min))
  max_min_X[[i]] <- max_min_X_tmp 
  
  save(graphondata, file = "suppledata/GraphonData.Rdata")
  save(max_min_X,   file = "bvary/Xmaxmin.Rdata")
  save(graphonst,   file = "bvary/GraphonData.Rdata")
 
}


stopCluster(cl)
