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
