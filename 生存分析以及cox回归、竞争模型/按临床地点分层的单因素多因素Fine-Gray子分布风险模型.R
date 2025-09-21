# 加载这两个核心包
library(cmprsk)
library(survival)
library(riskRegression)

# 1.第一步：创建模拟数据集------------------------------------------------------
# 设置随机数种子，以保证每次运行的结果都一样
set.seed(123)

# 定义患者数量
n_patients <- 6359 # 这是研究中基线无CSD的患者数量

# 创建模拟数据集
step_data <- data.frame(
  # 1. 生存时间和事件状态 (这是竞争风险模型的核心)
  # time: 从入组到发生事件或随访结束的时间（单位：月）
  time = runif(n_patients, 12, 48), 
  
  # status: 结局状态
  # 0 = 删失 (Censored): 随访结束时，既没有死亡，也没有发生CSD
  # 1 = 事件 (Event of Interest): 发生了新发CSD (这是我们关心的结局)
  # 2 = 竞争风险 (Competing Risk): 因其他原因死亡 (死亡阻止了CSD的发生)
  status = sample(c(0, 1, 2), n_patients, replace = TRUE, prob = c(0.85, 0.06, 0.09)),
  
  # 2. 核心自变量：治疗分组
  treatment = factor(sample(c("Standard", "Intensive"), n_patients, replace = TRUE)),
  
  # 3. 分层变量：临床中心
  clinical_site = factor(sample(paste0("Site_", 1:15), n_patients, replace = TRUE)),
  
  # 4. 需要校正的协变量 (按照方法学描述创建)
  age = rnorm(n_patients, mean = 65, sd = 5),
  sex = factor(sample(c("Male", "Female"), n_patients, replace = TRUE, prob = c(0.45, 0.55))),
  bmi = rnorm(n_patients, mean = 25.5, sd = 3),
  sbp_baseline = rnorm(n_patients, mean = 146, sd = 17),
  smoking = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.15, 0.85))),
  diabetes = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.20, 0.80))),
  hyperlipidemia = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.35, 0.65))),
  cvd_history = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.06, 0.94))),
  aspirin_use = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.10, 0.90))),
  statin_use = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.20, 0.80))),
  arb_use = factor(sample(c("Yes", "No"), n_patients, replace = TRUE, prob = c(0.44, 0.56))),
  egfr = rnorm(n_patients, mean = 109, sd = 24),
  total_chol = rnorm(n_patients, mean = 4.9, sd = 1.1),
  hdl_c = rnorm(n_patients, mean = 1.27, sd = 0.3)
)

# 查看数据前几行，确保创建成功
head(step_data)


# 2.第二步：实现Fine-Gray子分布风险模型-----------------------------------------
# 运行按中心分层的单变量Fine-Gray模型
# 语法非常清晰：
# Hist(time, status): 这是riskRegression包中定义竞争风险结局的标准方式
# ~ treatment: 这是我们关心的自变量
# + strata(clinical_site): 这就是我们想要的分层项！
# data: 指定数据集
# cause = 1: 指定我们关心的结局事件的编码是 "1"

univar_model_fgr <- FGR(
  prodlim::Hist(time, status) ~ treatment + strata(clinical_site),
  data = step_data,
  cause = 1
)

# 查看模型结果
print("--- [推荐] 使用 FGR() 的分层单变量模型结果 ---")
summary(univar_model_fgr)
## 2.2多变量调整模型 (Multivariable-adjusted Model)-----------------------------
# 运行按中心分层的多变量Fine-Gray模型
# 我们只需要在公式中加入所有需要校正的变量即可
multivar_model_fgr <- FGR(
  prodlim::Hist(time, status) ~ treatment + age + sex + bmi + sbp_baseline + 
    smoking + diabetes + hyperlipidemia + cvd_history + 
    aspirin_use + statin_use + arb_use + egfr + 
    total_chol + hdl_c + strata(clinical_site), # <- 分层项放在最后
  data = step_data,
  cause = 1
)

# 查看模型结果
print("--- [推荐] 使用 FGR() 的分层多变量模型结果 ---")
summary(multivar_model_fgr)


