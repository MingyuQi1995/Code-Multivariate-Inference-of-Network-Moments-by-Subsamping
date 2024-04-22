library(irlba)
library(Matrix)
load("data/guppylc.RData")


res <- res_guppy

exact <- res[[2]]

n <- dim(guppylarge)[[1]]
UR_G <- graph_stats_guppy(guppylarge,1,n)

save(UR_G,file = "res/URG.RData")