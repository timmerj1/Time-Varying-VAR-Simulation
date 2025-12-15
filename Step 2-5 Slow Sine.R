# ==============================================================================
# Final Analysis: Targeted Evaluation on Slow Sine Edges (Type 5)
# ==============================================================================


library(ggplot2)
library(dplyr)

# -------------------------------------------------------
# 1. 准备数据：只针对 Type 5 (Slow Sine) 计算平均误差
# -------------------------------------------------------
df_MAE_SlowSine <- data.frame()

# 遍历每一个样本量 N
for (sample_size_str in names(Cleaned_Results)) {

  N_val <- as.numeric(sample_size_str)
  dat <- Cleaned_Results[[sample_size_str]]

  # 获取边类型矩阵 (Gind)
  # 注意：这里需要访问原始的 G 列表来获取 Gind
  # 假设 G 列表还在环境中
  Edge_Type_Matrix <- G[[N_val]]$Gind

  # 创建掩码：只选择 Type 5 (Slow Sine) 的边
  Mask_SlowSine <- (Edge_Type_Matrix == 5)

  # 检查是否存在 Type 5 的边
  if (sum(Mask_SlowSine) > 0) {

    # 扩展掩码到时间维度 (10 x 10 x N)
    Mask_3D <- array(rep(Mask_SlowSine, N_val), dim = c(10, 10, N_val))

    # 计算误差矩阵
    Err_GAM <- abs(dat$Est_GAM - dat$True_G)
    Err_GLM <- abs(dat$Est_GLM - dat$True_G)

    # 只提取 Type 5 位置的误差求平均
    mae_gam <- mean(Err_GAM[Mask_3D], na.rm = TRUE)
    mae_glm <- mean(Err_GLM[Mask_3D], na.rm = TRUE)

    # 存入数据框
    df_MAE_SlowSine <- rbind(df_MAE_SlowSine, data.frame(
      N = N_val, Model = "TV-VAR (GAM)", MAE = mae_gam
    ))
    df_MAE_SlowSine <- rbind(df_MAE_SlowSine, data.frame(
      N = N_val, Model = "Stationary VAR (GLM)", MAE = mae_glm
    ))

  } else {
    warning(paste("N =", N_val, ": 未找到 Slow Sine (Type 5) 的边！"))
  }
}

# -------------------------------------------------------
# 2. 画图：复刻 Figure 5 风格 (Line Chart)
# -------------------------------------------------------

# 确保 N 是数值型，并按顺序排列
df_MAE_SlowSine$N <- as.numeric(df_MAE_SlowSine$N)

p1_target <- ggplot(df_MAE_SlowSine, aes(x = N, y = MAE, color = Model, group = Model)) +
  # 画折线和点
  geom_line(size = 1.2) +
  geom_point(size = 3) +

  # 配色方案 (参考 RColorBrewer 'Paired' 或自定义)
  scale_color_manual(values = c("Stationary VAR (GLM)" = "#FDBF6F", # 浅橙色 (参考论文)
                                "TV-VAR (GAM)" = "#1F78B4")) +      # 深蓝色 (参考论文)

  # X轴设置：对数刻度，显示原始标签
  scale_x_log10(breaks = c(200, 500, 1000, 2000)) +

  # Y轴设置：从 0 开始
  scale_y_continuous(limits = c(0, max(df_MAE_SlowSine$MAE) * 1.1)) +

  # 标题和标签
  labs(
    title = "Estimation Error on Seasonal Parameters (Slow Sine)",
    subtitle = "Mean Absolute Error (MAE) vs. Sample Size (Log Scale)",
    x = "Number of Time Points (N)",
    y = "Mean Absolute Error"
  ) +

  # 主题设置 (简约风)
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.y = element_line(color = "grey90") # 加一点横向网格线方便读数
  )

print(p1_target)
