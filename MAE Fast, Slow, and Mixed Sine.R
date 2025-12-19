# Multi-Wave: Targeted Analysis for Fast, Slow, and Mixed Sine

library(dplyr)
library(ggplot2)

df_MAE_MultiWave <- data.frame()

# 4 = Fast Sine
# 5 = Slow Sine
# 6 = Mixed Sine
target_types <- c(4, 5, 6)
type_labels  <- c("4" = "Fast Sine (High Frequency)",
                  "5" = "Slow Sine (Seasonal)",
                  "6" = "Mixed Sine (Fast + Slow)")

# Iterate through each sample size N
for (sample_size_str in names(Cleaned_Results)) {

  N_val <- as.numeric(sample_size_str)
  dat <- Cleaned_Results[[sample_size_str]]

  # Get the edge type matrix
  Edge_Type_Matrix <- G[[N_val]]$Gind

  # Internal loop: Iterate through the three types of sine waves
  for (type_code in target_types) {

    # Create masks
    Mask_Current <- (Edge_Type_Matrix == type_code)

    # Calculate only when the type exist
    if (sum(Mask_Current) > 0) {

      # Extend the mask to time dimension
      Mask_3D <- array(rep(Mask_Current, N_val), dim = c(10, 10, N_val))

      # Calculate errors
      Abs_Err_GAM  <- abs(dat$Est_GAM - dat$True_G)
      Abs_Err_GVAR <- abs(dat$Est_graphicalVAR - dat$True_G)

      # Calculate the average error of specific types of edges
      mae_gam  <- mean(Abs_Err_GAM[Mask_3D], na.rm = TRUE)
      mae_gvar <- mean(Abs_Err_GVAR[Mask_3D], na.rm = TRUE)

      # Get the name label of the current waveform
      current_label <- type_labels[as.character(type_code)]

      # Store data in data frame
      df_MAE_MultiWave <- rbind(df_MAE_MultiWave, data.frame(
        N = N_val,
        Model = "TV-VAR (GAM)",
        MAE = mae_gam,
        Wave_Type = current_label
      ))

      df_MAE_MultiWave <- rbind(df_MAE_MultiWave, data.frame(
        N = N_val,
        Model = "Stationary VAR (graphicalVAR)",
        MAE = mae_gvar,
        Wave_Type = current_label
      ))
    }
  }
}

# Plot the graph

# Make sure N is numeric
df_MAE_MultiWave$N <- as.numeric(df_MAE_MultiWave$N)

# Set the factor order of Wave_Type (Slow first and Fast last)
df_MAE_MultiWave$Wave_Type <- factor(df_MAE_MultiWave$Wave_Type,
                                     levels = c("Slow Sine (Seasonal)",
                                                "Fast Sine (High Frequency)",
                                                "Mixed Sine (Fast + Slow)"))

p_multi <- ggplot(df_MAE_MultiWave, aes(x = N, y = MAE, color = Model, group = Model)) +
  # Draw lines and dots
  geom_line(size = 1) +
  geom_point(size = 2.5) +

  # Unify y axis
  facet_wrap(~ Wave_Type, scales = "fixed") +

  # Color schemes
  scale_color_manual(values = c("Stationary VAR (graphicalVAR)" = "#E69F00",
                                "TV-VAR (GAM)" = "#1F78B4")) +

  # X-axis settings (Log Scale)
  scale_x_log10(breaks = c(200, 500, 1000, 2000)) +

  # Titles and tabs
  labs(
    title = "MAE Plot (Slow Sine, Fast Sine, and Mixed Sine)",
    subtitle = "Comparing TV-VAR vs. graphicalVAR",
    x = "Sample Size (N) [Log Scale]",
    y = "Mean Absolute Error (MAE)"
  ) +

  # Theme settings
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 11, face = "bold"),
    axis.text = element_text(size = 10)
  )

print(p_multi)

# Save the plot
ggsave("Figure2_MAE_SlowFastMixedSine.png", plot = p_multi, width = 12, height = 5, dpi = 300)
