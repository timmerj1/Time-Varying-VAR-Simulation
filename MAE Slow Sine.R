# Targeted Evaluation on Slow Sine Edges (Type 5)

library(dplyr)
library(ggplot2)

# Calculate the MAE under the condition of Slow Sine
df_MAE_SlowSine <- data.frame()

# Calculate the MAE under the condition of Slow Sine
for (sample_size_str in names(Cleaned_Results)) {

  N_val <- as.numeric(sample_size_str)

  # Get cleaned results
  dat <- Cleaned_Results[[sample_size_str]]

  # Get Gind
  Edge_Type_Matrix <- G[[N_val]]$Gind

  # Create mask: Select only the edges of Slow Sine (Type 5)
  Mask_SlowSine <- (Edge_Type_Matrix == 5)

  # Check if there are any Slow Sine edges
  num_slow_sine <- sum(Mask_SlowSine)

  if (num_slow_sine > 0) {
    # Extend mask to time dimension [10, 10, N]
    Mask_3D <- array(rep(Mask_SlowSine, N_val), dim = c(10, 10, N_val))

    # Calculate the absolute error matrix
    Abs_Err_GAM  <- abs(dat$Est_GAM - dat$True_G)
    Abs_Err_GVAR <- abs(dat$Est_graphicalVAR - dat$True_G)

    # Calculate the Slow Sine MAE
    mae_gam  <- mean(Abs_Err_GAM[Mask_3D], na.rm = TRUE)
    mae_gvar <- mean(Abs_Err_GVAR[Mask_3D], na.rm = TRUE) # --- 修改点 2: 变量名 ---

    # Store data in data frame
    df_MAE_SlowSine <- rbind(df_MAE_SlowSine, data.frame(
      N = N_val, Model = "TV-VAR (GAM)", MAE = mae_gam
    ))
    df_MAE_SlowSine <- rbind(df_MAE_SlowSine, data.frame(
      N = N_val, Model = "Stationary VAR (graphicalVAR)", MAE = mae_gvar
    ))

    print(paste("N =", N_val, ": Found", num_slow_sine, "Slow Sine edges. MAE calculated."))

  } else {
    warning(paste("N =", N_val, ": No Type 5 (Slow Sine) edges"))
  }
}


# Graph the line chart

# Make sure that N is numeric and arranged in order
df_MAE_SlowSine$N <- as.numeric(df_MAE_SlowSine$N)

p1_target <- ggplot(df_MAE_SlowSine, aes(x = N, y = MAE, color = Model, shape = Model)) +
  # Draw lines and dots
  geom_line(size = 1.2) +
  geom_point(size = 3) +

  # Set color schemes
  scale_color_manual(values = c("Stationary VAR (graphicalVAR)" = "#E69F00",
                                "TV-VAR (GAM)" = "#56B4E9")) +

  # X-axis setting
  scale_x_log10(breaks = c(200, 500, 1000, 2000)) +

  # Y-axis setting
  scale_y_continuous(limits = c(0, max(df_MAE_SlowSine$MAE) * 1.1)) +

  # Titles and tabs
  labs(
    title = "MAE plot (Slow Sine edges only)",
    subtitle = "Comparing TV-VAR vs. graphicalVAR",
    x = "Sample Size (N) [Log Scale]",
    y = "Mean Absolute Error (MAE)"
  ) +

  # Theme settings
  theme_classic() +
  theme(
    legend.position = "bottom",
    text = element_text(size = 12),
    panel.grid.major.y = element_line(color = "grey90")
  )

print(p1_target)

# Save the plot
ggsave("Figure1_MAE_SlowSine.png", plot = p1_target, width = 12, height = 5, dpi = 300)
