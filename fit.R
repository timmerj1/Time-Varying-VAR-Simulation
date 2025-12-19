
load("Data.Rdata")

head(Data[[N[1]]])
head(Data[[N[2]]])
head(Data[[N[3]]])
head(Data[[N[4]]])


# create foulders to store results
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("figures/gvar", recursive = TRUE, showWarnings = FALSE)


# load in data
data200 <- Data[[N[1]]]
data500 <- Data[[N[2]]]
data1000 <- Data[[N[3]]]
data2000 <- Data[[N[4]]]


# load in packages
library(tvvarGAM)
library(qgraph)
library(graphicalVAR)
library(RColorBrewer)
library(scales)
set.seed(67)





#### ----------- graphicalVAR ------------

mod200 <- graphicalVAR(data200, gamma = 0.5)
mod500 <- graphicalVAR(data500, gamma = 0.5)
mod1000 <- graphicalVAR(data1000, gamma = 0.5)
mod2000 <- graphicalVAR(data2000, gamma = 0.5)
PDC200 <- mod200$PDC
PCC200 <- mod200$PCC
avg_layout <- averageLayout(PDC200, PCC200)  # get layout


# plot
# Save GLM plots to github
models_graphicalVAR <- list(
  "200"  = mod200,
  "500"  = mod500,
  "1000" = mod1000,
  "2000" = mod2000
)

for (n in names(models_graphicalVAR)) {

  PDC <- models_graphicalVAR[[n]]$PDC

  png(
    filename = file.path("figures/gvar",
                         paste0("graphicalVAR_PDC_", n, ".png")),
    width = 2000,
    height = 2000,
    res = 300
  )

  qgraph(
    PDC,
    layout = avg_layout,
    title = paste0("Temporal Network (lag-1), N = ", n)
  )

  dev.off()
}





#### ----------- tvGAM ---------------
# estimate tvGAM
tvgam_obj200 <- tvvarGAM(data = data200, nb = 60, scale = TRUE)
tvgam_obj500 <- tvvarGAM(data = data500, nb = 60, scale = TRUE)
tvgam_obj1000 <- tvvarGAM(data = data1000, nb = 60, scale = TRUE)
tvgam_obj2000 <- tvvarGAM(data = data2000, nb = 60, scale = TRUE)


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




# ---------- plot GAM2000 ------------
timeL <- dim(tvgam_obj2000$Results_GAM$Estimate)[3]

# scale to 20 time points
wadj_point <- tvgam_obj2000$Results_GAM$Estimate[-1, , ]
wadj_CIlow <- tvgam_obj2000$Results_GAM$CI_low[-1, , ]
wadj_CIup <- tvgam_obj2000$Results_GAM$CI_high[-1, , ]
wadj_point_unthresh <- wadj_point
dim(wadj_point_unthresh)[3]

# select significant parameters
unlist_this <- function(x) { matrix(unlist(x$s.table[-1,4]), 10,1) }
sign <- matrix(unlist(lapply(lapply(tvgam_obj2000$model,summary), unlist_this)),10,10)

for(ii in 1:10) {
  for(j in 1:10) {
    if(sign[j, ii] < 0.05) {
      wadj_point[j, ii,] <- wadj_point[j, ii,]
    } else {
      wadj_point[j, ii,] <- 0
    }
  }
}
wadj_point_thresh<-wadj_point


# if signs are the same, there is no overlap with zero
ind_overlap <- sign(round(wadj_CIlow,2)) == sign(round(wadj_CIup,2))
wadj_point_thresh[!ind_overlap] <- 0


# plot
# save to github
png("figures/tvvarGAM_network_and_timecourse.png",
    width = 3600, height = 2400, res = 300)

lmat <- matrix(c(
  1, 2, 3,
  4, 4, 4
), ncol = 3, byrow = TRUE)
lo <- layout(lmat,
             heights = c(.7, .6),
             widths = c(1, 1, 1))


# select 3 time points
tpSelect <- c(500, 1000, 1500)


# plot network
for(tp in tpSelect) {
  qgraph(wadj_point_thresh[, , tp],
         layout = avg_layout,
         vsize = 13,
         esize = 10,
         asize = 10,
         mar = c(6, 6, 6, 6),
         minimum = 0,
         maximum = .5,
         title = paste("Time point =", tp))
}


# plot some parameter variance over time
plot.new()
par(mar = c(4,4,0,1))
plot.window(xlim=c(1, 2000), ylim=c(-.25, .85))
axis(1, c(1, 500, 1000, 1500, 2000), labels=T)
axis(2, c(-.25, 0, .25, 0.5, .75), las=2)
abline(h = 0, col = "grey", lty=2)
title(xlab = "Estimation points", cex.lab = 1.2)
title(ylab = "Parameter estimate", cex.lab = 1.2)


# Show same parameters
m_par_display <- matrix(c(1, 1,
                          3, 2,
                          7, 7), ncol = 2, byrow = T)


# Select colors
cols <- brewer.pal(5, "Set1")[c(2,4,5)]

# plot parameter variance over time
for(i in 1:nrow(m_par_display)) {

  # Plot point estimates
  par_row <- m_par_display[i, ]
  P1_pointest <- wadj_point_unthresh[par_row[1], par_row[2], ]
  # lines((1:20)+v_jitter[i], P1_pointest, col = cols[i], lwd = 2, lty=i)
  lines((1:timeL), P1_pointest, col = cols[i], lwd = 2, lty=i)

  # Center CI
  CI_low_par <- wadj_CIlow[par_row[1], par_row[2], ]
  CI_up_par <- wadj_CIup[par_row[1], par_row[2], ]

  polygon(x = c(1:timeL, timeL:1),
          y = c(CI_low_par, rev(CI_up_par)),
          col=alpha(colour = cols[i], alpha = .3),
          border=FALSE)

}


# define Legend
legend_labels <- c(
  expression("V1"["t-1"] %->% "V1"["t"]),
  expression("V3"["t-1"] %->% "V2"["t"]),
  expression("V7"["t-1"] %->% "V7"["t"])
)


# add legend
legend(
  1, .69,
  legend = legend_labels,
  col = cols,
  lwd = 2,
  bty = "n",
  cex = 1.3,
  horiz=T
)

dev.off()





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
                             layout = avg_layout,
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
    layout = avg_layout,
    directed = TRUE,
    title = title
  )

  # return layout
  if (return_layout) {
    return(g$layout)
  }

  invisible(B_sig)
}


# Save GLM plots to github
models <- list(
  "200"  = GLM200,
  "500"  = GLM500,
  "1000" = GLM1000,
  "2000" = GLM2000
)

for (n in names(models)) {

  png(
    filename = file.path("figures", paste0("GLM_network_", n, ".png")),
    width = 2000,
    height = 2000,
    res = 300
  )

  plot_GLM_network(
    res = models[[n]],
    layout = avg_layout,
    title = paste0("GLM Network (", n, ")")
  )

  dev.off()
}





#### ----------- save all the estimation results (Saved to OSF instead of GitHub) ------------
# save tvvar network estimation results
saveRDS(tvgam_obj200,  "results/tvvarGAM_200.rds")
saveRDS(tvgam_obj500,  "results/tvvarGAM_500.rds")
saveRDS(tvgam_obj1000, "results/tvvarGAM_1000.rds")
saveRDS(tvgam_obj2000, "results/tvvarGAM_2000.rds")

# save all tvvar parameters(for 2000 sample size)
saveRDS(
  list(
    wadj_point_unthresh = wadj_point_unthresh,
    wadj_point_thresh   = wadj_point_thresh,
    wadj_CIlow          = wadj_CIlow,
    wadj_CIup           = wadj_CIup,
    sign                = sign,
    ind_overlap         = ind_overlap
  ),
  file = "results/tvvarGAM_2000_processed.rds"
)


# save GLM network estimation results
saveRDS(
  list(
    GLM200  = GLM200,
    GLM500  = GLM500,
    GLM1000 = GLM1000,
    GLM2000 = GLM2000
  ),
  file = "results/GLM_models.rds"
)


# save graphicalVAR network estimation results
saveRDS(
  list(
    graphicalVAR200  = mod200,
    graphicalVAR500  = mod500,
    graphicalVAR1000 = mod1000,
    graphicalVAR2000 = mod2000
  ),
  file = "results/graphicalVAR_models.rds"
)


# save the plot layput
saveRDS(
  list(
    qgraph_layout = avg_layout
  ),
  file = "results/layouts_graphicalVAR200.rds"
)



