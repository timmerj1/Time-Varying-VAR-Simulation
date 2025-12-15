# ==============================================================================
# Plot 2: Error Stability Over Time (Targeted: Slow Sine Only)
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. 选择一个具有代表性的样本量 (建议选 N=2000，这是 TV-VAR 表现最好的时候)
target_N_h2 <- 2000
sample_str <- as.character(target_N_h2)

# 2. 获取数据
if (!sample_str %in% names(Cleaned_Results)) {
  stop("请检查 target_N_h2 是否在你的 N 列表中 (200, 500, 1000, 2000)")
}
dat <- Cleaned_Results[[sample_str]]

# 3. 获取边类型矩阵 & 筛选 Type 5 (Slow Sine)
Edge_Type_Matrix <- G[[target_N_h2]]$Gind
Mask_SlowSine <- (Edge_Type_Matrix == 5)

# 检查是否有 Slow Sine 边
if (sum(Mask_SlowSine) == 0) stop("未找到 Slow Sine 边，无法画图！")

# 4. 计算每一个时间点的平均误差 (只针对 Slow Sine 边)
# 我们需要一个循环或者 apply 来遍历时间 t
err_gam_t <- numeric(target_N_h2)
err_glm_t <- numeric(target_N_h2)

Abs_Err_GAM <- abs(dat$Est_GAM - dat$True_G)
Abs_Err_GLM <- abs(dat$Est_GLM - dat$True_G)

for (t in 1:target_N_h2) {
  # 取出 t 时刻的误差矩阵
  mat_gam <- Abs_Err_GAM[, , t]
  mat_glm <- Abs_Err_GLM[, , t]

  # 只计算 Mask 为 TRUE 的位置的平均值
  err_gam_t[t] <- mean(mat_gam[Mask_SlowSine])
  err_glm_t[t] <- mean(mat_glm[Mask_SlowSine])
}

# 5. 整理画图数据
plot_data_h2 <- rbind(
  data.frame(Time = 1:target_N_h2, Model = "TV-VAR (GAM)", Error = err_gam_t),
  data.frame(Time = 1:target_N_h2, Model = "Stationary VAR (GLM)", Error = err_glm_t)
)

# 6. 画图
p2_target <- ggplot(plot_data_h2, aes(x = Time, y = Error, color = Model)) +
  geom_line(size = 0.8, alpha = 0.9) +

  # 复刻论文配色
  scale_color_manual(values = c("Stationary VAR (GLM)" = "#FDBF6F", "TV-VAR (GAM)" = "#1F78B4")) +

  labs(
    title = paste("Hypothesis 2: Error Stability Over Time (N =", target_N_h2, ")"),
    subtitle = "Analysis limited to Seasonal Parameters (Slow Sine) only",
    x = "Time Points",
    y = "Mean Absolute Error (at Time t)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(p2_target)


# ==============================================================================
# Plot 3: Visual Parameter Tracking (Targeted: Slow Sine Only)
# ==============================================================================

# 1. 依然使用 N=2000 (效果最震撼)
target_N_visual <- 2000
dat_vis <- Cleaned_Results[[as.character(target_N_visual)]]

# 2. 找到一条是 Slow Sine (Type 5) 的边
# Gind 矩阵中值为 5 的位置
Edge_Type_Matrix <- G[[target_N_visual]]$Gind
sine_indices <- which(Edge_Type_Matrix == 5, arr.ind = TRUE)

if (nrow(sine_indices) == 0) stop("未找到 Slow Sine 边！")

# 选取第一条符合条件的边
target_row <- sine_indices[1, 1]
target_col <- sine_indices[1, 2]

print(paste("Plotting Edge: From Node", target_col, "To Node", target_row))

# 3. 提取这条边的轨迹
track_true <- dat_vis$True_G[target_row, target_col, ]
track_gam  <- dat_vis$Est_GAM[target_row, target_col, ]
track_glm  <- dat_vis$Est_GLM[target_row, target_col, ]

# 4. 整理数据
plot_data_p3 <- rbind(
  data.frame(Time = 1:target_N_visual, Type = "True Parameter", Value = track_true),
  data.frame(Time = 1:target_N_visual, Type = "TV-VAR (GAM)",   Value = track_gam),
  data.frame(Time = 1:target_N_visual, Type = "Stationary (GLM)", Value = track_glm)
)

# 5. 画图
p3_target <- ggplot(plot_data_p3, aes(x = Time, y = Value, color = Type, linetype = Type, size = Type)) +
  geom_line() +

  # 自定义配色和线型以突出对比
  scale_color_manual(values = c("True Parameter" = "black",
                                "Stationary (GLM)" = "#FDBF6F",
                                "TV-VAR (GAM)" = "#1F78B4")) +
  scale_linetype_manual(values = c("True Parameter" = "solid",
                                   "Stationary (GLM)" = "dashed", # 虚线表示它很"虚"
                                   "TV-VAR (GAM)" = "solid")) +
  scale_size_manual(values = c("True Parameter" = 1.2,
                               "Stationary (GLM)" = 1,
                               "TV-VAR (GAM)" = 1)) +

  labs(
    title = paste("Visual Inspection: Parameter Recovery (N =", target_N_visual, ")"),
    subtitle = paste("Tracking Edge V", target_col, "-> V", target_row, "(Type 5: Slow Sine)"),
    x = "Time Points",
    y = "Parameter Value"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

print(p3_target)

