
load("Data.Rdata")

head(Data[[N[1]]])
head(Data[[N[2]]])
head(Data[[N[3]]])
head(Data[[N[4]]])


# load in data
data200 <- Data[[N[1]]]
data500 <- Data[[N[2]]]
data1000 <- Data[[N[3]]]
data2000 <- Data[[N[4]]]


# load in packages
library(tvvarGAM)
library(qgraph)


# estimate tvGAM
tvgam_obj200 <- tvvarGAM(data = data200, nb = 20, scale = TRUE)
tvgam_obj500 <- tvvarGAM(data = data500, nb = 20, scale = TRUE)
tvgam_obj1000 <- tvvarGAM(data = data1000, nb = 20, scale = TRUE)
tvgam_obj2000 <- tvvarGAM(data = data2000, nb = 20, scale = TRUE)


# check edf to see if increase nb
# If the edf is close to the maximum possible edf (or number of basis functions),
# the model should be refitted with more basis functions.
get_all_edf <- function(tvobj) {
  unlist(lapply(tvobj$model, function(m) summary(m)$s.table[, "edf"]))
}
all_edf200 <- get_all_edf(tvgam_obj200)
all_edf500 <- get_all_edf(tvgam_obj500)
all_edf1000 <- get_all_edf(tvgam_obj1000)
all_edf2000 <- get_all_edf(tvgam_obj2000)
max(all_edf200) # max edf is 16.19624, does it too close to the 20(nb)?
max(all_edf500)
max(all_edf1000)
max(all_edf2000)



#### ----------- GLM ------------
# estimation function
Estimate_var_adj <- function(y)


{ #x is the index, y is the y variable

  N <- nrow(y)
  np <- ncol(y)

  tt=1:N

  data1=matrix(0,N,(np*2),byrow=T)
  for (h in (np+1):(np*2)){
    data1[,(h-np)]=y[,(h-np)]# data wordt hier gelagged

    data1[,h]=c(NA,y[1:(N-1),(h-np)])# data wordt hier gelagged
  }

  data1=as.data.frame(data1)
  colnames(data1)=c(paste("y",1:np,sep=""),paste("y",1:np,"L",sep=""))

  coln=colnames(data1)[1:np]
  colnL=colnames(data1)[(np+1):(np*2)]

  allcol3=paste(colnL,collapse="+")


  model=list()
  for (j in 1:np){
    ff <- as.formula(paste(coln[j]," ~ ",allcol3))
    model[[j]]<-lm(ff,data=data1)

  }

  # Collapse coefficients into matrix
  beta_mat <- do.call(rbind, lapply(model, function(x) x$coefficients))
  p_mat <- do.call(rbind, lapply(model, function(m) summary(m)$coefficients[,4]))
  se_mat <- do.call(rbind, lapply(model, function(m) summary(m)$coefficients[,2]))
  t_mat <- do.call(rbind, lapply(model, function(m) summary(m)$coefficients[,3]))


  return(list(
    beta = beta_mat,
    p = p_mat,
    se = se_mat,
    t = t_mat
  ))
}


# estimate network using GLM
GLM200 <- Estimate_var_adj(data200)
GLM500 <- Estimate_var_adj(data500)
GLM1000 <- Estimate_var_adj(data1000)
GLM2000 <- Estimate_var_adj(data2000)


# function to plot GLM network
plot_GLM_network <- function(res,
                             sig = TRUE,
                             alpha = 0.05,
                             layout = "spring",
                             return_layout = FALSE,
                             title = "GLM Network") {

  # extract beta and p
  B <- res$beta
  P <- res$p

  # remove intercept
  B <- B[, -1, drop = FALSE]
  P <- P[, -1, drop = FALSE]

  # select significant edge
  if (sig) {
    B_sig <- B
    B_sig[P > alpha] <- 0
  } else {
    B_sig <- B
  }

  # set row, column names
  n <- nrow(B_sig)
  rownames(B_sig) <- paste0("V", 1:n)
  colnames(B_sig) <- paste0("V", 1:n)

  # plot
  g <- qgraph(
    B_sig,
    layout = layout,
    directed = TRUE,
    title = title
  )

  # return layout
  if (return_layout) {
    return(g$layout)
  }

  invisible(B_sig)
}


# plot network with only siginificant edges
plot_GLM_network(res = GLM200, title = "GLM Network (200)")
# save layout
layout <- plot_GLM_network(res = GLM200, return_layout = TRUE)
# plot network with same layout
plot_GLM_network(res = GLM500, layout = layout, title = "GLM Network (500)")
plot_GLM_network(res = GLM1000, layout = layout, title = "GLM Network (1000)")
plot_GLM_network(res = GLM2000, layout = layout, title = "GLM Network (2000)")


