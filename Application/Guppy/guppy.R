library(igraph)
library(MASS)
library(Matrix)

# Set seed for reproducibility
set.seed(11116)

# Load data
load("data/msiguppylc.rda")


# for small network


A <- adjmat[[1]]
A[A != 0] = 1
diag(A) = 0

b <- dim(A)[1] 

rho_A <- sum(A)/(b*(b-1))

ressmall <- G_to_XR_UR(A,rho_A)

super <- list()
super[[1]] <- ressmall$superUR

  
exact<- list()
exact[[1]] <- ressmall$exactUR
  

# for large network


adj_matrix <- adjmat[[2]]

adj_matrix <- as.matrix(adj_matrix)
adj_matrix[adj_matrix != 0] = 1
diag(adj_matrix) = 0


N <- dim(adj_matrix)[1]

colnames(adj_matrix) <- c(1:N)
rownames(adj_matrix) <- c(1:N)

rho_hat_G <- sum(adj_matrix)/(N*(N-1))

res_subsample_super <- list()

res_subsample_exact <- list()

B = 10000


for(i in 1:B){
   
  print(i)
  
  nodesamplel <- sample(colnames(adj_matrix), b, replace = F)
  
  nodesamplel <- as.character(nodesamplel)
  
  A <- adj_matrix[nodesamplel, nodesamplel]
  
  res_tmp <- G_to_XR_UR(A,rho_hat_G)
  
  res_subsample_super[[i]] <- res_tmp$superUR
  
  res_subsample_exact[[i]] <- res_tmp$exactUR

}

res_super <- do.call(rbind, res_subsample_super)

colnames(res_super) <- c("twoStar","triangle","threeStar","square")

res_exact <- do.call(rbind, res_subsample_exact)

colnames(res_exact) <- c("twoStar","triangle","threeStar","square")

super[[2]] <- res_super
exact[[2]] <- res_exact 

res_guppy <- list()
res_guppy[[1]] <- super
res_guppy[[2]] <- exact


# Save results
save(res_guppy, file = "res/guppylc.RData")