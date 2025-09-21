library(survival)
library(dplyr)
set.seed(42) # 设置随机种子以保证每次运行结果都相同
n_patients <- 1500 # 模拟1500名患者
# 1.1 创建基线数据集 (wide format) ---------------------------------------------
baseline_data <- tibble(
  id = 1:n_patients,
  # 随机分配治疗组: 0 = 标准治疗, 1 = 强化治疗
  treatment = sample(0:1, n_patients, replace = TRUE),
  age = round(rnorm(n_patients, mean = 68, sd = 6)),
  sex = sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.6, 0.4)),
  # 基线时的LVH指数 (Peguero-Lo Presti index, 单位 μV)
  lvh_baseline = rnorm(n_patients, mean = 2600, sd = 450)
)
# 1.2 创建包含随访信息的长格式数据集 (long format) -----------------------------
# 假设在基线(time=0), 第1, 2, 3年有LVH测量值
long_data <- baseline_data %>%
  slice(rep(1:n(), each = 4)) %>%
  group_by(id) %>%
  mutate(visit_time = c(0, 1, 2, 3)) %>%
  ungroup() %>%
  mutate(
    # 模拟LVH指数的动态变化：
    # 强化治疗组(treatment=1)的LVH指数每年下降得更多
    # 这就人为地创造了“路径 a” (Treatment -> LVH)
    lvh_index = case_when(
      visit_time == 0 ~ lvh_baseline,
      # 强化组每年平均多降100, 再加上一些随机波动
      TRUE ~ lvh_baseline - visit_time * (50 + 100 * treatment) + rnorm(n(), sd = 150)
    )
  )

# 1.3 模拟生存时间和事件 (心血管事件) ------------------------------------------
# 事件风险依赖于治疗组以及当前的LVH指数
event_data <- baseline_data %>%
  mutate(
    # 模拟事件发生时间。风险依赖于治疗和基线LVH
    # 强化治疗(treatment=1)降低风险, 高LVH增加风险
    # log(hazard) = -0.6 * treatment + 0.001 * (lvh_baseline - 2600)
    true_time = rexp(n(), rate = 0.04 * exp(-0.6 * treatment + 0.001 * (lvh_baseline - 2600))),
    # 模拟一个删失时间(0.1到4年)，代表失访或研究结束
    censor_time = runif(n(), min = 0.1, max = 4),
    # 最终观察时间是真实事件时间和删失时间中的较小者
    observed_time = pmin(true_time, censor_time),
    # 状态：1 = 发生事件, 0 = 删失
    status = as.numeric(true_time <= censor_time)
  ) %>%
  dplyr::select(id, observed_time, status)

# 1.4 将数据转换为适用于时变协变量Cox模型的格式 (start, stop, event) -----------

# 这是最关键的数据准备步骤，使用survival包的tmerge函数
final_analysis_data <- tmerge(
  data1 = baseline_data,
  data2 = event_data,
  id = id,
  event = event(observed_time, status)
)

# 将时变的LVH指数作为时变协变量(tdc)合并进来
final_analysis_data <- tmerge(
  data1 = final_analysis_data,
  data2 = long_data,
  id = id,
  lvh = tdc(visit_time, lvh_index) # tdc() 表示lvh是一个时变协变量
)

#  2.1 拟合模型 1: 估计总效应 (路径 c: Treatment -> Outcome) -------------------
cat("--- 模型 1: 治疗对心血管结局的总效应 (Total Effect) ---\n")
model_total_effect <- coxph(
  Surv(observed_time, status) ~ treatment + age + sex,
  data = left_join(event_data, baseline_data, by = "id")
)
print(summary(model_total_effect))
# 提取总效应系数
coef_c <- coef(model_total_effect)["treatment"]
coef_c

# 2.2 拟合模型 2: 估计路径 a (Treatment -> Mediator) ---------------------------
cat("\n--- 模型 2: 治疗对中介变量(LVH)的影响 (Path 'a') ---\n")
lvh_at_year_3 <- long_data %>% filter(visit_time == 3)
model_path_a <- lm(lvh_index ~ treatment + age + sex, data = lvh_at_year_3)
print(summary(model_path_a))

# 提取路径 a 的系数
coef_a <- coef(model_path_a)["treatment"]
coef_a

# 2.3 拟合模型 3: 估计路径 b 和直接效应 c' -------------------------------------
cat("\n--- 模型 3: 估计路径 'b' 和直接效应 'c_prime' ---\n")
model_paths_b_and_c_prime <- coxph(
  Surv(tstart, tstop, event) ~ treatment + lvh + age + sex,
  data = final_analysis_data
)
print(summary(model_paths_b_and_c_prime))

# 提取路径 b 的系数 (LVH 对结局的影响)
coef_b <- coef(model_paths_b_and_c_prime)["lvh"]
coef_b
# 提取直接效应 c' 的系数 (控制LVH后，治疗对结局的剩余影响)
coef_c_prime <- coef(model_paths_b_and_c_prime)["treatment"]
coef_c_prime


#  3.1 使用系数乘积法计算 ------------------------------------------------------
indirect_effect <- coef_a * coef_b
mediation_proportion <- indirect_effect / coef_c
mediation_proportion


# 第二种方法：手动bootstrap-----------------------------------------------------------------
# ==============================================================================
# 黄金标准: 使用手动Bootstrap对时变中介变量进行中介分析
# ==============================================================================

# --- 步骤 0: 准备工作 - 加载额外包 ---
# lme4 包用于拟合线性混合效应模型
# 如果尚未安装，请取消下面代码的注释并运行
# install.packages("lme4")
library(lme4)

# 之前已加载 survival 和 dplyr

# --- 步骤 1: 设置Bootstrap参数 ---
n_boot <- 500 # Bootstrap重复次数。建议至少1000次，此处为演示设为500
set.seed(42) # 保证结果可重复

# 创建一个向量来存储每次Bootstrap的间接效应结果
indirect_effects_boot <- numeric(n_boot)

# --- 步骤 2: 执行Bootstrap循环 ---
# 这个过程可能需要几分钟，取决于您的电脑性能和n_boot的大小
cat(paste0("开始执行 ", n_boot, " 次Bootstrap模拟...\n"))

for (i in 1:n_boot) {
  # --- 2.1 对研究对象(id)进行有放回的重抽样 ---
  boot_ids <- sample(unique(baseline_data$id), size = n_patients, replace = TRUE)
  
  # 创建自助样本，需要从原始数据中提取对应id的所有记录
  boot_baseline_data <- baseline_data[match(boot_ids, baseline_data$id), ]
  # 需要处理重复ID的问题，为自助样本创建新的唯一ID
  boot_baseline_data$new_id <- 1:n_patients
  
  # 对应地创建自助长格式数据和生存数据
  boot_long_data <- long_data %>% filter(id %in% boot_ids) %>%
    left_join(dplyr::select(boot_baseline_data, id, new_id), by="id")
  
  boot_event_data <- event_data %>% filter(id %in% boot_ids) %>%
    left_join(dplyr::select(boot_baseline_data, id, new_id), by="id")
  
  
  # --- 2.2 在自助样本上拟合模型 a (LMM) ---
  # 使用lmer评估治疗对LVH变化轨迹(斜率)的影响
  # (1|new_id)表示为每个患者设置随机截距
  # visit_time:treatment 表示我们关心治疗是否改变了LVH随时间变化的斜率
  model_a_boot <- lmer(lvh_index ~ visit_time * treatment + age + sex + (1 | new_id), 
                       data = boot_long_data,
                       # 忽略收敛警告，因为在某些自助样本中可能出现拟合问题
                       control = lmerControl(check.nobs.vs.nRE = "ignore"))
  
  # 路径 a 的系数: 治疗对斜率的交互影响
  coef_a_boot <- fixef(model_a_boot)["visit_time:treatment"]
  
  # --- 2.3 在自助样本上拟合模型 b (Cox with TDC) ---
  # 准备时变数据格式
  boot_final_data <- tmerge(
    data1 = dplyr::select(boot_baseline_data, new_id, treatment, age, sex),
    data2 = dplyr::select(boot_event_data, new_id, observed_time, status),
    id = new_id,
    event = event(observed_time, status)
  )
  boot_final_data <- tmerge(
    data1 = boot_final_data,
    data2 = dplyr::select(boot_long_data, new_id, visit_time, lvh_index),
    id = new_id,
    lvh = tdc(visit_time, lvh_index)
  )
  
  model_b_boot <- coxph(Surv(tstart, tstop, event) ~ treatment + lvh + age + sex, 
                        data = boot_final_data)
  
  # 路径 b 的系数
  coef_b_boot <- coef(model_b_boot)["lvh"]
  
  # --- 2.4 计算并存储本次循环的间接效应 ---
  indirect_effects_boot[i] <- coef_a_boot * coef_b_boot
  
  # 打印进度
  if (i %% 50 == 0) cat(paste0("已完成 ", i, "/", n_boot, " 次迭代...\n"))
}

cat("Bootstrap模拟完成!\n")


# --- 步骤 3: 分析Bootstrap结果 ---
cat("\n=================================================================\n")
cat("          手动Bootstrap中介分析 - 最终结果\n")
cat("=================================================================\n\n")

# 点估计 (使用中位数更稳健)
point_estimate <- median(indirect_effects_boot, na.rm = TRUE)

# 计算95%置信区间
ci_lower <- quantile(indirect_effects_boot, 0.025, na.rm = TRUE)
ci_upper <- quantile(indirect_effects_boot, 0.975, na.rm = TRUE)

cat(paste0("间接效应的点估计 (Median): ", round(point_estimate, 5), "\n"))
cat(paste0("95% Bootstrap置信区间: [", round(ci_lower, 5), ", ", round(ci_upper, 5), "]\n\n"))

# --- 结论 ---
if (ci_lower * ci_upper > 0) { # 如果CI的下限和上限同号 (都不包含0)
  cat("结论: 间接效应具有统计学显著性。\n")
  cat("我们有证据表明，治疗通过影响LVH的变化轨迹，进而影响了心血管结局。\n")
} else {
  cat("结论: 间接效应没有统计学显著性。\n")
  cat("我们没有足够的证据表明LVH在此处扮演了中介角色。\n")
}

# 第二种方法是更可靠的，第一种方法是非常不严谨的


