library(igraph)
library(MASS)
library(Matrix)

# Set seed for reproducibility
set.seed(6669)

# Load data
load("data/collablc.rda")



# for large network


adj_matrix <- collab_adjmat_combine$adjcondense99

adj_matrix <- as.matrix(adj_matrix)
adj_matrix[adj_matrix != 0] = 1
diag(adj_matrix) = 0


N <- dim(adj_matrix)[1]

colnames(adj_matrix) <- c(1:N)
rownames(adj_matrix) <- c(1:N)

rho_hat_G <- sum(adj_matrix)/(N*(N-1))

res_subsample_super <- list()

res_subsample_exact <- list()

b = 635
B = 2000


for(i in 1:B){
   
  print(i)
  
  nodesamplel <- sample(colnames(adj_matrix), b, replace = F)
  
  nodesamplel <- as.character(nodesamplel)
  
  A <- adj_matrix[nodesamplel, nodesamplel]
  
  res_tmp <- G_to_XR_UR(A,rho_hat_G)
  
  res_subsample_super[[i]] <- res_tmp$superUR
  
  res_subsample_exact[[i]] <- res_tmp$exactUR

}

res_stats_super <- do.call(rbind, res_subsample_super)

colnames(res_stats_super) <- c("twoStar","triangle","threeStar","square")

res_stats_exact <- do.call(rbind, res_subsample_exact)

colnames(res_stats_exact) <- c("twoStar","triangle","threeStar","square")


res_stats <- list(res_stats_super = res_stats_super, res_stats_exact = res_stats_exact )


b = 379


for(i in 1:B){
   
  print(i)
  
  nodesamplel <- sample(colnames(adj_matrix), b, replace = F)
  
  nodesamplel <- as.character(nodesamplel)
  
  A <- adj_matrix[nodesamplel, nodesamplel]
  
  res_tmp <- G_to_XR_UR(A,rho_hat_G)
  
  res_subsample_super[[i]] <- res_tmp$superUR
  
  res_subsample_exact[[i]] <- res_tmp$exactUR

}

res_netSci_super <- do.call(rbind, res_subsample_super)

colnames(res_netSci_super) <- c("twoStar","triangle","threeStar","square")

res_netSci_exact <- do.call(rbind, res_subsample_exact)

colnames(res_netSci_exact) <- c("twoStar","triangle","threeStar","square")


res_netSci <- list(res_netSci_super = res_netSci_super, res_netSci_exact = res_netSci_exact )




res_con_99 <- list()
res_con_99[[1]] <- res_stats
res_con_99[[2]] <- res_netSci


# Save results
save(res_con_99, file = "res/cond_mat_99.RData")