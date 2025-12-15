# ==============================================================================
# Step 1: Data Extraction & Cleaning
# ==============================================================================

# 定义我们要处理的样本量列表
N_vec <- c(200, 500, 1000, 2000)

# 创建一个列表来存储清洗后的数据，方便后续步骤使用
Cleaned_Results <- list()

for (sample_size in N_vec) {

  # -------------------------------------------------------
  # 1. 提取真值 (True Parameters)
  # -------------------------------------------------------
  # 您的队友生成的 G 是一个稀疏列表，直接用 sample_size 索引提取
  True_G <- G[[sample_size]]$G

  # -------------------------------------------------------
  # 2. 提取并清洗 TV-VAR (GAM) 估计值
  # -------------------------------------------------------
  # 动态获取对象名 (例如 tvgam_obj200)
  gam_obj_name <- paste0("tvgam_obj", sample_size)
  current_gam <- get(gam_obj_name)

  # tvvarGAM 的输出结构通常是: [Predictor(包括截距), Target, Time]
  # 我们需要去掉第一行 (Intercept)，因为它不是网络边缘参数
  Est_GAM_Raw <- current_gam$Results_GAM$Estimate
  Est_GAM_NoIntercept <- Est_GAM_Raw[-1, , ]

  # 【关键修正】:
  # tvvarGAM 输出通常是 [From_Node, To_Node, Time]
  # 但 GenerateGraph 生成的 G 通常是 [To_Node, From_Node, Time] (或者是反过来的，取决于定义)
  # 为了保险起见，我们需要转置前两个维度，让它变成 [Target, Predictor, Time]
  # 如果后续发现误差特别大（接近随机），可能需要把下面这就 aperm 注释掉
  Est_GAM_Clean <- aperm(Est_GAM_NoIntercept, c(2, 1, 3))

  # -------------------------------------------------------
  # 3. 提取并清洗 Stationary VAR (GLM) 估计值
  # -------------------------------------------------------
  # 动态获取对象名 (例如 GLM200)
  glm_obj_name <- paste0("GLM", sample_size)
  current_glm <- get(glm_obj_name)

  # 提取 Beta 矩阵
  # GLM 函数输出的结构: 行是 Target (y1, y2...), 列是 Predictor (Intercept, y1_lag, y2_lag...)
  Beta_GLM <- current_glm$beta

  # 去掉第一列 (Intercept)
  Beta_GLM_NoIntercept <- Beta_GLM[, -1]

  # 扩展维度 (Expand):
  # Stationary 模型的参数是一个静态矩阵 [10, 10]
  # 为了能和 True_G 做减法，我们必须把它复制 N 次，变成 [10, 10, N]
  Est_GLM_Clean <- array(rep(Beta_GLM_NoIntercept, sample_size),
                         dim = c(10, 10, sample_size))

  # -------------------------------------------------------
  # 4. 存储清洗好的数据
  # -------------------------------------------------------
  Cleaned_Results[[as.character(sample_size)]] <- list(
    True_G = True_G,
    Est_GAM = Est_GAM_Clean,
    Est_GLM = Est_GLM_Clean
  )
}



# ==============================================================================
# Step 2: Calculate Metrics & Prepare Plotting Data
# ==============================================================================

# 初始化三个空的数据框，用来存放我们要画的三张图的数据
df_MAE_Summary <- data.frame()      # 图 1: 总体 MAE 对比 (H1)
df_Error_Time  <- data.frame()      # 图 2: 误差随时间变化 (H2)
df_Param_Track <- data.frame()      # 图 3: 参数追踪展示 (Visual Check)

# 遍历每一个样本量 (200, 500, 1000, 2000)
for (sample_size_str in names(Cleaned_Results)) {

  # 1. 获取当前样本量的数据
  N <- as.numeric(sample_size_str)
  dat <- Cleaned_Results[[sample_size_str]]

  True_G  <- dat$True_G
  Est_GAM <- dat$Est_GAM
  Est_GLM <- dat$Est_GLM

  # -------------------------------------------------------
  # 计算绝对误差矩阵 (Absolute Error Matrix)
  # -------------------------------------------------------
  # 维度: [10, 10, N]
  Abs_Err_GAM <- abs(Est_GAM - True_G)
  Abs_Err_GLM <- abs(Est_GLM - True_G)

  # -------------------------------------------------------
  # A. 准备数据：总体 MAE (对应 H1)
  # -------------------------------------------------------
  # 计算所有时间点、所有边的平均误差
  mae_gam_val <- mean(Abs_Err_GAM, na.rm = TRUE)
  mae_glm_val <- mean(Abs_Err_GLM, na.rm = TRUE)

  # 存入数据框
  df_MAE_Summary <- rbind(df_MAE_Summary, data.frame(
    Sample_Size = N,
    Model = "TV-VAR (GAM)",
    MAE = mae_gam_val
  ))

  df_MAE_Summary <- rbind(df_MAE_Summary, data.frame(
    Sample_Size = N,
    Model = "Stationary VAR (GLM)",
    MAE = mae_glm_val
  ))

  # -------------------------------------------------------
  # B. 准备数据：误差随时间变化 (对应 H2)
  # -------------------------------------------------------
  # 对每一个时间点 t，计算 10x10 个参数的平均误差
  # apply(..., 3, mean) 表示在第3维度(时间)上做平均
  err_time_gam <- apply(Abs_Err_GAM, 3, mean, na.rm = TRUE)
  err_time_glm <- apply(Abs_Err_GLM, 3, mean, na.rm = TRUE)

  # 存入数据框 (长格式，方便 ggplot)
  tmp_time_df <- data.frame(
    Time = 1:N,
    Sample_Size = N,
    Error_GAM = err_time_gam,
    Error_GLM = err_time_glm
  )

  # 转换成长格式 (Long Format)
  tmp_time_long <- rbind(
    data.frame(Time = 1:N, Sample_Size = N, Model = "TV-VAR (GAM)", Error = err_time_gam),
    data.frame(Time = 1:N, Sample_Size = N, Model = "Stationary VAR (GLM)", Error = err_time_glm)
  )

  df_Error_Time <- rbind(df_Error_Time, tmp_time_long)

  # -------------------------------------------------------
  # C. 准备数据：单条参数追踪 (Visual Check)
  # -------------------------------------------------------
  # 为了画图好看，我们需要自动找到一条"波动最明显"的边
  # 策略：计算 True_G 中每条边随时间的方差，找方差最大的那条

  # 计算每个位置 [i, j] 的方差
  var_matrix <- apply(True_G, c(1, 2), var)
  # 找到方差最大的位置索引
  max_idx <- which(var_matrix == max(var_matrix), arr.ind = TRUE)
  target_row <- max_idx[1, 1]
  target_col <- max_idx[1, 2]

  # 提取这条边在三个模型中的轨迹
  track_true <- True_G[target_row, target_col, ]
  track_gam  <- Est_GAM[target_row, target_col, ]
  track_glm  <- Est_GLM[target_row, target_col, ]

  # 存入数据框
  edge_label <- paste0("Edge: V", target_col, " -> V", target_row)

  tmp_track_df <- rbind(
    data.frame(Time = 1:N, Sample_Size = N, Type = "True Parameter", Value = track_true),
    data.frame(Time = 1:N, Sample_Size = N, Type = "TV-VAR (GAM)",   Value = track_gam),
    data.frame(Time = 1:N, Sample_Size = N, Type = "Stationary (GLM)", Value = track_glm)
  )
  tmp_track_df$Edge_Name <- edge_label

  df_Param_Track <- rbind(df_Param_Track, tmp_track_df)
}

# 简单检查一下数据是否生成成功
print(head(df_MAE_Summary))


# ==============================================================================
# Step 3: Graphing & Visualization
# ==============================================================================

library(ggplot2)
library(dplyr)

# -------------------------------------------------------
# Plot 1: 总体准确度对比 (验证 H1)
# -------------------------------------------------------
# 即使 H1 目前看起来被证伪了，这张图依然要放，然后去解释原因
p1_his <- ggplot(df_MAE_Summary, aes(x = factor(Sample_Size), y = MAE, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  scale_fill_manual(values = c("Stationary VAR (GLM)" = "#E69F00", "TV-VAR (GAM)" = "#56B4E9")) +
  labs(
    title = "Hypothesis 1 Check: Overall Parameter Estimation Accuracy",
    subtitle = "Comparison of Mean Absolute Error (MAE) across Sample Sizes",
    x = "Sample Size (N)",
    y = "Mean Absolute Error (MAE)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1_his)

p1_line <- ggplot(df_MAE_Summary,
             aes(x = factor(Sample_Size),
                 y = MAE,
                 color = Model,
                 group = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(
    values = c(
      "Stationary VAR (GLM)" = "#E69F00",
      "TV-VAR (GAM)" = "#56B4E9"
    )
  ) +
  labs(
    title = "Hypothesis 1 Check: Overall Parameter Estimation Accuracy",
    subtitle = "Comparison of Mean Absolute Error (MAE) across Sample Sizes",
    x = "Sample Size (N)",
    y = "Mean Absolute Error (MAE)",
    color = "Model"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p1_line)

# -------------------------------------------------------
# Plot 2: 误差随时间的变化 (验证 H2)
# -------------------------------------------------------
# 我们选取 N=1000 的情况来展示，因为样本量大时趋势更清晰
plot_data_h2 <- df_Error_Time %>% filter(Sample_Size == 1000)

p2 <- ggplot(plot_data_h2, aes(x = Time, y = Error, color = Model)) +
  geom_line(alpha = 0.8, size = 0.8) +
  scale_color_manual(values = c("Stationary VAR (GLM)" = "#E69F00", "TV-VAR (GAM)" = "#56B4E9")) +
  labs(
    title = "Hypothesis 2 Check: Error Stability Over Time (N = 1000)",
    subtitle = "Stationary model error should fluctuate; TV-VAR error should be stable",
    x = "Time Points",
    y = "Mean Absolute Error at Time t"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)


# -------------------------------------------------------
# Plot 3: 单条参数追踪 (Visual Check)
# -------------------------------------------------------
# 这张图最关键！我们要看对于一条"真正变化"的边，谁拟合得更好。
# 选取 N=1000 的那条波动最大的边
plot_data_visual <- df_Param_Track %>% filter(Sample_Size == 1000)

p3 <- ggplot(plot_data_visual, aes(x = Time, y = Value, color = Type, linetype = Type)) +
  geom_line(size = 1) +
  scale_color_manual(values = c(
    "True Parameter" = "black",
    "Stationary (GLM)" = "#E69F00",
    "TV-VAR (GAM)" = "#56B4E9"
  )) +
  scale_linetype_manual(values = c(
    "True Parameter" = "solid",
    "Stationary (GLM)" = "dashed",
    "TV-VAR (GAM)" = "solid"
  )) +
  labs(
    title = "Visual Inspection: Parameter Recovery (N = 1000)",
    subtitle = paste("Tracking a single dynamic edge:", unique(plot_data_visual$Edge_Name)),
    x = "Time Points",
    y = "Parameter Value"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p3)
