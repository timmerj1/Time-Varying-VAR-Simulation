#Data Extraction & Cleaning

load("Data.Rdata")

# Define the sample sizes
N_vec <- c(200, 500, 1000, 2000)

# Load the graphicalVAR results
graphicalVAR_models <- readRDS("results/graphicalVAR_models.rds")

# Initialize the result list
Cleaned_Results <- list()

# Load GAM results from OSF
tvvarGAM_200 <- readRDS("results/tvvarGAM_200.rds")
tvvarGAM_500 <- readRDS("results/tvvarGAM_500.rds")
tvvarGAM_1000 <- readRDS("results/tvvarGAM_1000.rds")
tvvarGAM_2000 <- readRDS("results/tvvarGAM_2000.rds")
layout_avg <- readRDS("results/layouts_graphicalVAR200.rds")

# Put all GAM results to a list
GAM_Results <- list(
  "200"  = tvvarGAM_200,
  "500"  = tvvarGAM_500,
  "1000" = tvvarGAM_1000,
  "2000" = tvvarGAM_2000
)

for (sample_size in N_vec) {

  ss <- as.character(sample_size)

  # True Parameters
  True_G <- G[[sample_size]]$G

  # tv VAR GAM
  current_gam <- GAM_Results[[ss]]

  Est_GAM_Raw <- current_gam$Results_GAM$Estimate

  # Delete Intercept
  Est_GAM_NoIntercept <- Est_GAM_Raw[-1, , ]

  # Transpose to adjust the dimension order
  # Change to [Target, Predictor, Time]
  Est_GAM_Clean <- aperm(Est_GAM_NoIntercept, c(2, 1, 3))

  # Extract and clean Stationary VAR (graphical)
  graphicalVAR_name <- paste0("graphicalVAR", sample_size)

  if(!is.null(graphicalVAR_models[[graphicalVAR_name]])) {
    current_graphicalVAR <- graphicalVAR_models[[graphicalVAR_name]]

    # Extract Beta Matrix
    Beta_graphicalVAR<- current_graphicalVAR$beta

    # Delete Intercept
    Beta_graphicalVAR_NoIntercept <- Beta_graphicalVAR[, -1]

    # Extend dimension
    Est_graphicalVAR_Clean <- array(rep(Beta_graphicalVAR_NoIntercept, sample_size),
                                    dim = c(10, 10, sample_size))
  }

  # Store
  if(!any(is.na(Est_GAM_Clean)) && !any(is.na(Est_graphicalVAR_Clean))) {
    Cleaned_Results[[ss]] <- list(
      True_G = True_G,
      Est_GAM = Est_GAM_Clean,
      Est_graphicalVAR = Est_graphicalVAR_Clean
    )
  }
}
