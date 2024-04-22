library(MASS)
library(Matrix)
library(igraph)
library(foreach)
library(doParallel)
library(parallel)

no_cores <- 50
cl <- makeCluster(no_cores)
registerDoParallel(cl)

load("/home/tianxili/qim/Mingyu_Network_KS_simulation/function.rda")


aggregated_max_min_S <- list()
aggregated_subsamplest <- list()

# Loop over L to load each file and aggregate
for (l in 1:L) {
  load(paste0("suppledata/max_min_SIter_l", l, ".RData"))
  aggregated_max_min_S[[l]] <- resmaxmin

  load(paste0("suppledata/subsamplestIter_l", l, ".RData"))
  aggregated_subsamplest[[l]] <- subsamplestIter
}
  save(aggregated_subsamplest, file = "bvary/Subsample.Rdata")
  save(aggregated_max_min_S , file = "bvary/Smaxmin.Rdata")



load("bvary/Xmaxmin.Rdata")
load("bvary/Smaxmin.Rdata")
resg <- findXmaxmin(max_min_X)
ress <- findXmaxmin(aggregated_max_min_S)
seq_inf <- seqtablegen(resg,ress)
rm(list=setdiff(ls(), "seq_inf"))




load("/home/tianxili/qim/Mingyu_Network_KS_simulation/function.rda")
load("bvary/GraphonData.Rdata")
load("bvary/Subsample.Rdata")



eval <- list()


for (i in seq_along(bset) ) {
  
  print(i)
    

  b <- bset[i]
  
  eval_uni <-list()
  
  eval_multi <-list()
  
    
  mygraphondata <-  graphonst[[i]]
  
  X <- as.data.frame(mygraphondata)
  
  
# Parallel loop
results <- foreach(l = 1:L, .combine = 'c', .packages = c("MASS")) %dopar% {
  ssdata <- aggregated_subsamplest[[l]]
  Y <- as.data.frame(ssdata[[i]])  # Make sure 'i' is correctly defined/supplied

  # Assuming evalKS is a function that returns a list with 'unires' and 'multisres'
  res <- evalks(X, Y, seq_inf, num_values)

  list(unires = res$unires, multisres = res$multisres)
}

   odd_indexed <- lapply(seq(1, length(results), by = 2), function(i) results [[i]])
   eval_uni <- do.call(rbind, odd_indexed)
   even_indexed <- lapply(seq(2, length(results), by = 2), function(i) results [[i]])
   eval_multi <- do.call(rbind, even_indexed)
   eval[[i]] <- setNames(list(eval_uni, eval_multi), c(paste0("unieval", b), paste0("multieval", b)))
   save(eval, file = "res/eval.Rdata")
  
}