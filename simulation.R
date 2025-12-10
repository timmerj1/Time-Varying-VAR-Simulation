
library(animation)
source("simulation_functions.R")

N <- c(200, 500, 1000, 2000)
G <- list()

for (sample in N) {
  G[[sample]] <- GenerateGraph(n_parameters = 10,
                     sparsity = 0.29,
                     theta = 0.35,
                     N = sample,
                     seed = 67,
                     verbose=FALSE,
                     max_check=10000)
  G_check <- apply(G[[sample]]$G, 3, function(x) {
    eig_x <- eigen(x)
    sum(abs(eig_x$values) >= 1) > 0 # TRUE if violated
  })
  print(sum(G_check))
}



# G_animate <- saveGIF({for (i in 1:N) {
#   qgraph(G[[200]]$G[,,i], layout = animation_graph$layout, palette = "colorblind")
#   }}, movie.name = "TimeVaryingGraph.gif")

Data <- list()
for (sample in N) {
  Data[[sample]] <- Data1000 <- VARData(G[[sample]]$G, matrix(0, sample, 10), 1, seed = 67)
}

save(Data, N, file = "Data.Rdata")
