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

# median包来做，但是median包不可以处理包含时序变量的中介分分析------------------
library(mediation)

# 为了演示，我们简化模型，使用基线LVH作为中介
# 1. 拟合路径 a 的模型 (M ~ X)
model_m_simple <- lm(lvh_baseline ~ treatment + age + sex, data = baseline_data)

# 2. 拟合包含 X 和 M 的结局模型 (Y ~ X + M)
model_y_simple <- survreg(
  Surv(observed_time, status) ~ treatment + lvh_baseline + age + sex,
  data = left_join(event_data, baseline_data, by = "id")
)

# 3. 运行 mediation 分析，进行1000次Bootstrap模拟
# boot = TRUE 默认开启Bootstrap
# sims = 1000 指定模拟次数
set.seed(123) # 保证结果可重复
med_analysis_boot <- mediate(
  model.m = model_m_simple,
  model.y = model_y_simple,
  treat = "treatment",
  mediator = "lvh_baseline",
  sims = 1000
)

# 4. 查看详细结果
cat("\n\n=================================================================\n")
cat("          使用 'mediation' 包进行 Bootstrap 分析的结果\n")
cat("=================================================================\n\n")
print(summary(med_analysis_boot))









