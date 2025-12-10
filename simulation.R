
library(animation)
source("simulation_functions.R")

N <- 1000

G <- GenerateGraph(n_parameters = 10,
                   sparsity = 0.29,
                   theta = 0.35,
                   N = N,
                   seed = 67,
                   verbose=FALSE,
                   max_check=10000)

# G_animate <- saveGIF({for (i in 1:N) {
#   qgraph(G$G[,,i], layout = animation_graph$layout, palette = "colorblind")
#   }}, movie.name = "TimeVaryingGraph.gif")

Data1000 <- VARData(G$G, matrix(0, 1000, 10), 1, seed = 67)

save(Data1000, file = "Data1000.Rdata")


