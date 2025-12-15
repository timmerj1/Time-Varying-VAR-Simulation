# ==============================================================================
# Step 4 (Multi-Wave): Targeted Analysis for Fast, Slow, and Mixed Sine
# ==============================================================================

library(dplyr)
library(ggplot2)

df_MAE_MultiWave <- data.frame()

# 定义我们要分析的类型及其名称
# 根据您的 GenerateGraph 代码:
# 4 = Fast Sine
# 5 = Slow Sine
# 6 = Slow + Fast Sine
target_types <- c(4, 5, 6)
type_labels  <- c("4" = "Fast Sine (High Freq)",
                  "5" = "Slow Sine (Seasonal)",
                  "6" = "Mixed Sine (Fast + Slow)")

# 遍历每一个样本量
for (sample_size_str in names(Cleaned_Results)) {

  N_val <- as.numeric(sample_size_str)
  dat <- Cleaned_Results[[sample_size_str]]

  # 获取边类型矩阵
  Edge_Type_Matrix <- G[[N_val]]$Gind

  # --- 内部循环：遍历三种正弦波类型 ---
  for (type_code in target_types) {

    # 1. 创建该类型的掩码
    Mask_Current <- (Edge_Type_Matrix == type_code)
    num_edges <- sum(Mask_Current)

    if (num_edges > 0) {

      # 2. 扩展掩码到时间维度
      Mask_3D <- array(rep(Mask_Current, N_val), dim = c(10, 10, N_val))

      # 3. 计算误差
      Abs_Err_GAM <- abs(dat$Est_GAM - dat$True_G)
      Abs_Err_GLM <- abs(dat$Est_GLM - dat$True_G)

      # 4. 提取特定类型边的平均误差
      mae_gam <- mean(Abs_Err_GAM[Mask_3D], na.rm = TRUE)
      mae_glm <- mean(Abs_Err_GLM[Mask_3D], na.rm = TRUE)

      # 5. 存入数据框 (增加一列 Wave_Type)
      current_label <- type_labels[as.character(type_code)]

      df_MAE_MultiWave <- rbind(df_MAE_MultiWave, data.frame(
        N = N_val,
        Model = "TV-VAR (GAM)",
        MAE = mae_gam,
        Wave_Type = current_label
      ))

      df_MAE_MultiWave <- rbind(df_MAE_MultiWave, data.frame(
        N = N_val,
        Model = "Stationary VAR (GLM)",
        MAE = mae_glm,
        Wave_Type = current_label
      ))

    } else {
      # 仅在调试时打印，避免刷屏
      # print(paste("N =", N_val, ": No edges found for Type", type_code))
    }
  }
}

print("多类型正弦波误差计算完成！")
head(df_MAE_MultiWave)

# ==============================================================================
# Step 5 (Multi-Wave): Faceted Plotting
# ==============================================================================

# 确保 N 是数值型
df_MAE_MultiWave$N <- as.numeric(df_MAE_MultiWave$N)

# 设置 Wave_Type 的因子顺序 (让 Slow 在前面，Fast 在后面，符合逻辑递进)
df_MAE_MultiWave$Wave_Type <- factor(df_MAE_MultiWave$Wave_Type,
                                     levels = c("Slow Sine (Seasonal)",
                                                "Fast Sine (High Freq)",
                                                "Mixed Sine (Fast + Slow)"))

p_multi <- ggplot(df_MAE_MultiWave, aes(x = N, y = MAE, color = Model, group = Model)) +
  # 画线和点
  geom_line(size = 1) +
  geom_point(size = 2.5) +

  # 关键：使用分面 (Facet Wrap) 来区分三种波形
  facet_wrap(~ Wave_Type, scales = "free_y") +

  # 配色
  scale_color_manual(values = c("Stationary VAR (GLM)" = "#FDBF6F",
                                "TV-VAR (GAM)" = "#1F78B4")) +

  # 坐标轴 (Log Scale)
  scale_x_log10(breaks = c(200, 500, 1000, 2000)) +

  # 标题
  labs(
    title = "Model Performance across Different Temporal Frequencies",
    subtitle = "Comparing TV-VAR vs. Stationary VAR on Slow, Fast, and Mixed Sine Waves",
    x = "Sample Size (N) [Log Scale]",
    y = "Mean Absolute Error (MAE)"
  ) +

  # 主题优化
  theme_bw() + # 使用黑白主题框线更清晰
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold"), # 分面标题加粗
    axis.text = element_text(size = 10)
  )

print(p_multi)
