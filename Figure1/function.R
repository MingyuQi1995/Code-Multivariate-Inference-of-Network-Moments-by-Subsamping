# Function to calculate the super number of k-star 
cal_k_star <- function(adj, k){
  
  # Create an adjacency matrix for a k-star structure
  staradj <- matrix(0, (k+1), (k+1)) # Initialize a (k+1)x(k+1) matrix with zeros
  staradj[1, 2:(k+1)] = 1            # Set first row (except for the first column) to 1
  staradj[2:(k+1), 1] = 1            # Set first column (except for the first row) to 1
  
  A0 <- as.matrix(adj)               # Convert the input adjacency matrix to a matrix object
  
  num_kstar <- c()                   # Initialize a vector to store the count of k-stars for each node
  
  # Loop through each row (node) of the adjacency matrix
  for(i in 1:nrow(A0)){
    
    # Find indices of non-zero entries, which are the connected nodes
    idx_edge <- which(A0[i,] != 0)
    k_tmp <- 0                        # Temporary counter for k-stars
    
    # Check if the number of connected nodes is at least k
    if(length(idx_edge) >= k){
      
      # Generate all combinations of k nodes from the connected nodes
      comb_tmp <- combn(idx_edge, k)
      
      # Check each combination to see if it forms a k-star with node i
      for(j in 1:ncol(comb_tmp)){
        tmp <- c(i, comb_tmp[,j])
        adj_tmp <- A0[tmp, tmp]       # Get the subgraph of node i and nodes in the combination
        dimnames(adj_tmp) <- NULL     # Remove dimension names for comparison
        dimnames(staradj) <- NULL     # Remove dimension names for comparison
        
        # Check if the subgraph is a k-star by comparing with the k-star adjacency matrix
        if(sum(abs(adj_tmp - staradj)) < 1e-10){
          k_tmp <- k_tmp + 1
        }
        
      }
      
      num_kstar[i] <- k_tmp
    } else {
      num_kstar[i] = 0
    }
  }
  
  return(num_kstar)
}

# Function to calculate the exact number of k-star subgraphs a graph
cal_star_exact <- function(adj, k) {
  # Initialize the adjacency matrix for a k-star
  staradj <- matrix(0, nrow = k + 1, ncol = k + 1)
  staradj[1, 2:(k + 1)] <- 1
  staradj[2:(k + 1), 1] <- 1
  
  A0 <- as.matrix(adj)  # Ensure the adjacency data is in matrix form
  num_kstar <- numeric(nrow(A0))  # Initialize a vector to store the count of k-stars for each node
  
  # Iterate over each node in the adjacency matrix
  for (i in 1:nrow(A0)) {
    # Identify all nodes connected to the current node
    idx_edge <- which(A0[i,] != 0)
    
    if (length(idx_edge) >= k) {
      # Generate all k-combinations of connected nodes
      comb_tmp <- combn(idx_edge, k)
      
      # Initialize a counter for k-stars centered at node i
      k_tmp <- 0
      
      # Examine each combination to see if it forms a k-star with the central node
      for (j in 1:ncol(comb_tmp)) {
        tmp <- c(i, comb_tmp[, j])
        adj_tmp <- A0[tmp, tmp]
        
        # Remove dimension names for the matrix comparison
        dimnames(adj_tmp) <- NULL
        dimnames(staradj) <- NULL
        
        # If the subgraph matches the k-star structure, increment the counter
        if (sum(abs(adj_tmp - staradj)) < 1e-10) {
          k_tmp <- k_tmp + 1
        }
      }
      
      num_kstar[i] <- k_tmp
    } else {
      num_kstar[i] <- 0
    }
  }
  
  return(num_kstar)
}


# Function to calculate the number of exact  squares in a graph
cal_square_exact <- function(adj) {
  A0 <- as.matrix(adj) # Convert the adjacency list to a matrix
  num_sq <- c() # Vector to store the number of squares involving each vertex
  n <- dim(A0)[[1]] # Number of vertices in the graph
  
  # Loop over each vertex in the graph
  for (v in 1:n) {
    num_sq_v <- c() # Temporary vector to store counts of squares per combination
    tmp <- which(A0[v,] != 0) # Find all vertices directly connected to vertex v
    
    # Proceed only if there are at least two connected vertices
    if (length(tmp) < 2) {
      num_sq[v] <- 0
    } else {
      broteforcein <- combn(tmp, 2) # Get all pairs of connected vertices
      bflength <- dim(broteforcein)[[2]] # Number of such pairs
      
      # Iterate over each pair of connected vertices
      for (k in 1:bflength) {
        temtwonodes <- broteforcein[, k]
        node1 <- temtwonodes[1]
        node2 <- temtwonodes[2]
        
        node1connect <- which(A0[node1, ] != 0) # Neighbors of node1
        node2connect <- which(A0[node2, ] != 0) # Neighbors of node2
        
        nodemutual <- intersect(node1connect, node2connect) # Common neighbors of node1 and node2
        nodemutual <- nodemutual[nodemutual != v] # Exclude vertex v
        
        num_sq_v_temp_twonodes <- 0 # Counter for squares with the current two nodes
        
        # Check mutual connections for potential squares
        for (last in nodemutual) {
          currentindex <- c(v, node1, node2, last)
          B <- A0[currentindex, currentindex] # Submatrix for the potential square
          
          # Check for missing edges that would invalidate the square
          if (sum(B[1, 4]) == 0 && sum(B[2, 3]) == 0) {
            num_sq_v_temp_twonodes <- num_sq_v_temp_twonodes + 1
          }
        }
        
        num_sq_v[k] <- num_sq_v_temp_twonodes
      }
      
      num_sq[v] <- sum(num_sq_v) # Sum of all squares involving vertex v
    }
  }
  
  squreX <- sum(num_sq) / 4 # Each square is counted four times, once for each vertex
  return(squreX) # Return the total number of squares in the graph
}

# Function to calculate the number of super squares in a graph
cal_square_super <- function(adj) {
  A0 <- as.matrix(adj)  # Convert the adjacency list to a matrix
  num_sq <- vector("numeric", length = nrow(A0))  # Preallocate vector for efficiency
  
  for (v in 1:nrow(A0)) {
    connected_nodes <- which(A0[v,] != 0)  # Nodes directly connected to vertex v
    
    # Only proceed if there are at least two connected nodes
    if (length(connected_nodes) >= 2) {
      combinations <- combn(connected_nodes, 2)  # All pairs of connected vertices
      num_sq_v <- numeric(ncol(combinations))  # Temporary vector for squares per combination
      
      for (k in seq_len(ncol(combinations))) {
        nodes_pair <- combinations[, k]
        mutual_nodes <- intersect(which(A0[nodes_pair[1], ] != 0), which(A0[nodes_pair[2], ] != 0))
        mutual_nodes <- mutual_nodes[mutual_nodes != v]  # Exclude the central node v
        
        # Calculate potential squares excluding self-loops or duplicate edges
        num_sq_v[k] <- sum(sapply(mutual_nodes, function(last) {
          subgraph <- A0[c(v, nodes_pair, last), c(v, nodes_pair, last)]
          # A square must not have any diagonal connections between the pairs
          all(subgraph[1, 4] == 0 && subgraph[2, 3] == 0)
        }))
      }
      
      num_sq[v] <- sum(num_sq_v)  # Sum up all valid squares for this vertex
    } else {
      num_sq[v] <- 0
    }
  }
  
  # Each square is counted four times (once per each vertex), so divide by 4
  total_squares <- sum(num_sq) / 4
  return(total_squares)
}


# Calculate empirical cumulative distribution function (ECDF) differences
empirical_dcdf <- function(X, Y, x) {
  
  
  # Initialize variables
  num_eval_points <- nrow(x)
  num_features <- ncol(x)
  num_rows_X <- nrow(X)
  num_rows_Y <- nrow(Y)
  ecdf_X <- numeric(num_eval_points)
  ecdf_Y <- numeric(num_eval_points)
  diff_ecdf <- numeric(num_eval_points)
  
  # Sort and order data
  sorted_X <- apply(X, 2, sort)
  sorted_Y <- apply(Y, 2, sort)
  order_X <- apply(X, 2, order)
  order_Y <- apply(Y, 2, order)
  
  # Compute ECDFs and their differences
  for (i in 1:num_eval_points) {
    idx_tmp1 <- matrix(FALSE, nrow = num_rows_X, ncol = num_features)
    idx_tmp2 <- matrix(FALSE, nrow = num_rows_Y, ncol = num_features)
    
    for (j in 1:num_features) {
      tmp1 <- ( sorted_X[,j] < x[i,j])
      tmp2 <- ( sorted_Y[,j] < x[i,j])
      idx_tmp1[order_X[which(tmp1),j],j] <- TRUE
      idx_tmp2[order_Y[which(tmp2),j],j] <- TRUE
    }
    
    com1 <- length(which(rowSums(idx_tmp1) == num_features))
    com2 <- length(which(rowSums(idx_tmp2) == num_features))
    
    ecdf_X[i] <- com1  / num_rows_X
    ecdf_Y[i] <- com2  / num_rows_Y
    diff_ecdf[i] <- ecdf_X[i] - ecdf_Y[i]
  }
  
  return(list(ecdf_X, ecdf_Y, diff_ecdf))
}


# Calculate ks distance for the graphon data and subsampled data
evalks <- function(X,Y,seq_inf,num_values){
   
   
   seqinf <- seq_inf
   
   seqv <- seq(from =  seqinf[2,1], to = seqinf[1,1], length.out = num_values)
   
   seqtri <-  seq(from =  seqinf[2,2], to = seqinf[1,2], length.out = num_values)
   
   seqstar <-  seq(from =  seqinf[2,3], to = seqinf[1,3], length.out = num_values)
   
   seqsquare <- seq(from =  seqinf[2,4], to = seqinf[1,4], length.out = num_values)
   
   
   x_v <- as.matrix(seqv)
   x_tri <- as.matrix(seqtri)
   x_star <- as.matrix(seqstar)
   x_sq <- as.matrix(seqsquare)
   
   
   X_v <- as.matrix(X[,1])
   X_tri <- as.matrix(X[,2])
   X_star <- as.matrix(X[,3])
   X_sq <- as.matrix(X[,4])
   Y_v <- as.matrix(Y[,1])
   Y_tri <- as.matrix(Y[,2])
   Y_star <- as.matrix(Y[,3])
   Y_sq <- as.matrix(Y[,4])
   
   
   temp <- empirical_dcdf(X_v,Y_v,x_v)
   tempv <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_tri,Y_tri,x_tri)
   temptri <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_star,Y_star,x_star)
   tempstar <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_sq,Y_sq,x_sq)
   tempsq <- max(abs(temp[[3]]))
   
   
   uni_res <- c(tempv,temptri,tempstar,tempsq)
   
   
   
   x_vt <- expand.grid(seqv,seqtri)
   x_v3 <- expand.grid(seqv,seqstar) 
   x_vs <- expand.grid(seqv,seqsquare) 
   
   x_t3 <- expand.grid(seqtri,seqstar)
   x_ts <- expand.grid(seqtri,seqsquare)
   
   
   x_3s <-expand.grid(seqstar,seqsquare)
   
   X_vt <- X[,c(1,2)]
   X_v3 <- X[,c(1,3)]
   X_vs <- X[,c(1,4)]
   X_t3 <- X[,c(2,3)]
   X_ts <- X[,c(2,4)]
   X_3s <- X[,c(3,4)]
   Y_vt <- Y[,c(1,2)]
   Y_v3 <- Y[,c(1,3)]
   Y_vs <- Y[,c(1,4)]
   Y_t3 <- Y[,c(2,3)]
   Y_ts <- Y[,c(2,4)]
   Y_3s <- Y[,c(3,4)]
   
   temp <- empirical_dcdf( X_vt,Y_vt,x_vt)
   tempvt <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_v3,Y_v3,x_v3)
   tempv3 <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_vs,Y_vs,x_vs)
   tempvs <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_t3,Y_t3,x_t3)
   tempt3 <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_ts,Y_ts,x_ts)
   tempts <- max(abs(temp[[3]]))
   
   temp <- empirical_dcdf(X_3s,Y_3s,x_3s)
   temp3s <- max(abs(temp[[3]]))
   
   multi_res <- c(tempvt,tempv3,tempvs,tempt3,tempts,temp3s)
   
   return(list(unires = uni_res,multisres = multi_res))
   
}

# Define a function to calculate the global maximum and minimum from a list of maxima and minima
findXmaxmin  <- function(max_min_X) {
  # Extract max values from each element in the list and combine them into a dataframe
  max_vectors <- lapply(max_min_X, function(x) x$max)
  df_max <- do.call(rbind, max_vectors)
  resmax <- apply(df_max, 2, max)  # Calculate the maximum for each column
  
  # Extract min values from each element in the list and combine them into a dataframe
  min_vectors <- lapply(max_min_X, function(x) x$min)
  df_min <- do.call(rbind, min_vectors)
  resmin <- apply(df_min, 2, min)  # Calculate the minimum for each column
  
  # Return a list containing the results
  return(list(resmax = resmax, resmin = resmin))
}

#Geneate random graphon from graphon model
generate_random_graph <- function(s = 0.4, pscale = -0.001, my_graphon, graphsize) {
  # Define the number of nodes
  n <- graphsize
  
  # Calculate the scaling factor for the graphon values
  rho_n <- s * n^pscale
  
  # Generate random values for each node, which are inputs to the graphon function
  graphon_values <- runif(n)
  
  # Create row and column indices for the upper triangle of the adjacency matrix
  rows <- unlist(lapply(1:(n-1), function(i) rep(i, n-i)))
  cols <- unlist(lapply(1:(n-1), function(i) (i+1):n))
  
  # Function to calculate the adjacency matrix value for a pair of nodes
  calculate_value <- function(i, j) {
    # Obtain graphon values for the pair of nodes
    a <- graphon_values[i]
    b <- graphon_values[j]
    # Evaluate the graphon function and scale it
    temp <- my_graphon(a, b) * rho_n
    # Ensure probabilities are within [0,1] bounds
    temp <- ifelse(temp > 1 | temp < 0, 0, temp)
    # Perform a Bernoulli trial to determine edge existence
    U <- runif(1) 
    bernoulli_trial <- ifelse(U <= temp, 1, 0)
    return(bernoulli_trial) 
  }
  
  # Calculate adjacency values for all pairs (i, j)
  adj_values <- mapply(calculate_value, rows, cols)
  
  # Create a sparse matrix and convert it to a regular matrix
  B <- sparseMatrix(i = c(rows, cols), j = c(cols, rows), x = c(adj_values, adj_values), dims = c(n, n))
  
  return(as.matrix(B))
}

#Computa super network moments of a graph
graph_to_stats <- function(A, rho, k){
  
  A2 <- A %*% A
  diag(A2) <- 0
  
  Uv <- sum(A2)/(k*(k-1)*(k-2)/3)
  scaleUv <-  (rho^(-2))*Uv
  
  
  
  Utri <- sum(diag(A %*% A %*% A)) / (k*(k-1)*(k-2))
  scaleUtri <-  (rho^(-3))*Utri 
  
  
  vv <- colSums(A)
  vv2 <- colSums(A^2)
  vv3 <- colSums(A^3)
  
  Utstar <- sum(vv^3 - 3*vv2*vv + 2*vv3) /6 / (k*(k-1)*(k-2)*(k-3)/24)
  scaleUtstar <- (rho^(-3))*Utstar 
  
  #Xsquare <- cal_square_super(A)
  
  #Usquare <- Xsquare/ (k*(k-1)*(k-2)*(k-3)/24)
  
  #scaleUsquare <- (rho^(-4))*Usquare
  
  Usquare <- 1
  scaleUsquare <- 1
  return(list(UR = c(Uv,Utri,Utstar,Usquare), scaleUR = c(scaleUv,  scaleUtri, scaleUtstar,scaleUsquare)))
}
