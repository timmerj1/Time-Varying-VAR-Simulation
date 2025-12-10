
# Adopted from Haslbeck, J. M. B., Bringmann, L. F., & Waldorp, L. J. (2021). A Tutorial on Estimating Time-Varying Vector Autoregressive Models. Multivariate Behavioral Research, 56(1), 120-149. https://doi.org/10.1080/00273171.2020.1743630

GenerateGraph <- function(n_parameters,
                          sparsity,
                          theta,
                          N,
                          seed,
                          verbose=FALSE,
                          max_check=10000) {

  set.seed(seed)

  # ---------- Define Graph Types ----------

  # Define Graph Types
  edgetypes <- list()
  x <- seq(0, theta, length = N)
  k <- 15

  edgetypes[[1]] <- rep(theta, N) # Edge 1: Constant
  edgetypes[[2]] <- seq(0, theta, length = N) # Edge 2: Linear Increase
  edgetypes[[3]] <- seq(theta, 0, length = N) # Edge 3: Linear Decrease
  # Edge 4: Fast Sine
  edgetypes[[4]] <- theta * (sin((2/5) * base::pi * 1:N) / 2 + 0.5)
  # Edge 5: Slow Sine
  edgetypes[[5]] <- theta * (sin((2/1825) * base::pi * 1:N) / 2 + 0.5)
  # Edge 6: Slow + Fast Sine
  edgetypes[[6]] <- theta *
    ((sin((2/5) * base::pi * x) + sin((2/1825) * base::pi * 1:N)) / 4 + 0.5)



  # ---------- Generate and Assign Edges ----------

  check <- 0
  counter <- 1

  while(check == 0) {

    # ---- 1) Generate Graph Structure -----

    # What are we doing here?
    # a) Set all autocorrelations in the diagonal to be present
    # b) sample size off-diagonal elements, reflecting the specified P(e)

    # Generate Empty Graph
    n_edges <- n_parameters^2 - n_parameters # minus diagonal
    n_upper_diag <- n_parameters*(n_parameters-1)/2 # same for lower diag, of course
    size <- round(n_edges*sparsity)
    v_e <- sample(2:n_edges, size = size, replace = FALSE)

    # Fill in 2d Dummy matrix
    G_aux <- matrix(NA, n_parameters, n_parameters)
    diag(G_aux) <- 1
    G_aux[upper.tri(G_aux)] <- 2:(n_upper_diag+1) # not using 1, so I can subset using unequal 3 lines down
    G_aux[lower.tri(G_aux)] <- (n_upper_diag+2):(n_upper_diag*2+1)
    G_aux[which(G_aux %in% v_e, arr.ind = TRUE)] <- 1
    G_aux[G_aux != 1] <- 0

    # get indicator for which entries are 1
    ind_present <- which(G_aux == 1, arr.ind = TRUE)

    # ---- 2) Assign the Type of Edges -----

    Gind <- matrix(0, n_parameters, n_parameters) # Storage: summary information
    G <- array(0, dim = c(n_parameters, n_parameters, N)) # the 3D time-varying VAR array

    for(i in 1:nrow(ind_present)) {
      type_i <- sample(1:6, size = 1)
      G[ind_present[i, 1], ind_present[i, 2], ] <- edgetypes[[type_i]]
      Gind[ind_present[i, 1], ind_present[i, 2]] <- type_i
    }

    # ---------- Check Eigenvalue Condition ----------

    # Check condition for each time step
    G_check <- apply(G, 3, function(x) {
      eig_x <- eigen(x)
      sum(abs(eig_x$values) >= 1) > 0 # TRUE if violated
    })

    if(sum(G_check) == 0) {
      check <- 1 # break the loop
    } else {
      counter <- counter + 1
    }

    # To avoid infinite loop: break after 1000 loops:

    if(counter > max_check) stop('No graph within the constrained region found in 1000 iterations.')
    if(verbose) print(counter)

  }




  # ---------- Export ----------

  outlist <- list('G' = G,
                  'Gind' = Gind,
                  'counter' = counter)

  return(outlist)

} # eoF: GenerateGraph



VARData <- function(graph,
                    th,
                    sd,
                    seed) {

  set.seed(seed)

  N <- dim(graph)[3]
  p <- dim(graph)[1]

  data <- matrix(NA, N, p)
  data[1,] <- rnorm(p, 0, sd)


  for(t in 2:N) {
    for(node in 1:p) {
      data[t, node] <- th[t, node] + sum(data[t-1,] * graph[node,,t]) + rnorm(1, mean = 0, sd = sd)
    }
  }

  return(data)

}
