# 1.多元线性回归----------------------------------------------------------------
df15_1 <- data.frame(
  cho = c(5.68,3.79,6.02,4.85,4.60,6.05,4.90,7.08,3.85,4.65,4.59,4.29,7.97,
          6.19,6.13,5.71,6.40,6.06,5.09,6.13,5.78,5.43,6.50,7.98,11.54,5.84,
          3.84),
  tg = c(1.90,1.64,3.56,1.07,2.32,0.64,8.50,3.00,2.11,0.63,1.97,1.97,1.93,
         1.18,2.06,1.78,2.40,3.67,1.03,1.71,3.36,1.13,6.21,7.92,10.89,0.92,
         1.20),
  ri = c(4.53, 7.32,6.95,5.88,4.05,1.42,12.60,6.75,16.28,6.59,3.61,6.61,7.57,
         1.42,10.35,8.53,4.53,12.79,2.53,5.28,2.96,4.31,3.47,3.37,1.20,8.61,
         6.45),
  hba = c(8.2,6.9,10.8,8.3,7.5,13.6,8.5,11.5,7.9,7.1,8.7,7.8,9.9,6.9,10.5,8.0,
          10.3,7.1,8.9,9.9,8.0,11.3,12.3,9.8,10.5,6.4,9.6),
  fpg = c(11.2,8.8,12.3,11.6,13.4,18.3,11.1,12.1,9.6,8.4,9.3,10.6,8.4,9.6,10.9,
          10.1,14.8,9.1,10.8,10.2,13.6,14.9,16.0,13.2,20.0,13.3,10.4)
)

str(df15_1)


##### 1.1回归前探索数据---------------------------------------------------------
library(GGally)
library(psych)
cor(df15_1)
corr.test(df15_1)
ggpairs(df15_1) + theme_bw()
##### 1.2建立回归方程-----------------------------------------------------------
f <- lm(fpg ~ cho + tg + ri + hba, data = df15_1)
summary(f)


# 2.回归诊断--------------------------------------------------------------------
##### 2.1看图来判断-------------------------------------------------------------
opar <- par(mfrow = c(2,2))
plot(f)
par(opar)

###### 2.1.1回归诊断图解析------------------------------------------------------

"第1幅图（左上）是残差拟合图
展示真实残差和拟合残差的关系    判读是否满足线性这个条件
如果满足，则应该为一条直线      本图明显是一条曲线
说明不是很满足线性这个条件，可能需要加二次项
"
"第2幅图（右上）是正态Q-Q图    判断是否满足正态性这个条件
"

"第3幅图（左下）是位置尺度图    判读是否满足同方差性
如果满足，水平线两侧的点应该随机分布"

"第4幅图（右下）是残差杠杆图    用于识别离群点等"

##### 2.2统计方法验证-----------------------------------------------------------
###### 2.2.1线性回归模型的综合验证----------------------------------------------
library(gvlma)
gvmodel<-gvlma(f)
summary(gvmodel)

"Call:
 gvlma(x = f) 

                     Value  p-value                   Decision
Global Stat        9.68910 0.046003 Assumptions NOT satisfied!
Skewness           0.65344 0.418886    Assumptions acceptable.
Kurtosis           0.04015 0.841193    Assumptions acceptable.
Link Function      7.68064 0.005582 Assumptions NOT satisfied!
Heteroscedasticity 1.31487 0.251515    Assumptions acceptable."
###### 2.2.2综合验证结果解读----------------------------------------------------
"(1) Global Stat (全局检验)
意义：综合评估模型是否满足所有线性回归假设。
你的结果：
p = 0.046 (<0.05) → 模型假设整体上不满足，需进一步检查具体问题。

(2) Skewness (偏度检验)
检验内容：残差是否对称分布（是否偏斜）。
你的结果：
p = 0.419 (>0.05) → 残差分布对称性可接受。

(3) Kurtosis (峰度检验)
检验内容：残差是否服从正态分布的峰度（尾部厚度）。
你的结果：
p = 0.841 (>0.05) → 残差峰度符合正态分布假设。

(4) Link Function (链接函数)
检验内容：因变量与预测变量之间是否是线性关系（或需非线性变换）。
你的结果：
p = 0.006 (<0.05) → 线性关系假设不成立，可能存在：
非线性关系（如二次项、交互项未纳入模型）。
因变量需转换（如对数变换、Box-Cox变换）。

(5) Heteroscedasticity (异方差性)
检验内容：残差的方差是否恒定（即是否随预测值变化）。
你的结果：
p = 0.252 (>0.05) → 无异方差性问题，方差稳定性可接受"

##### 2.3回归模型条件挨个验证---------------------------------------------------
###### 2.3.1验证正态性----------------------------------------------------------
library(car)
qqPlot(f,labels = row.names(df15_1), id.method = "identify",simulate = T,
       main = "Q-Q plot")   

###### 2.3.2验证因变量独立性----------------------------------------------------
durbinWatsonTest(f) 
#p=0.32 > 0.05 → 接受原假设，认为残差无显著自相关
###### 2.3.3验证线性------------------------------------------------------------
crPlots(f)

# 3.多重共线性的检验------------------------------------------------------------
vif(f)
vif(f)>4
library(AppliedPredictiveModeling)
library(caret)
findLinearCombos(df15_1)
# 变量选择----------------------------------------------------------------------
# 4.逐步选择法------------------------------------------------------------------
library(MASS)
stepAIC(f, direction = "backward")
fit.step <- step(f, direction = "backward")
summary(fit.step)#查看最佳模型
stepAIC(f, direction = "forward")
fit.step <- step(f, direction = "forward")
summary(fit.step)#查看最佳模型
broom::tidy(fit.step)#查看结果

# 5.全局择优法(全子集回归法，最优子集法)----------------------------------------
library(leaps)
leaps <- regsubsets(fpg ~ cho + tg + ri + hba, data = df15_1, nbest=1)
# nbest=1返回每个变量数量（k）下的 1个最优模型
summary(leaps)
##### 5.1 定义subsTable()函数查看全子集结果-------------------------------------
subsTable <- function(obj, scale){
  x <- summary(leaps)
  m <- cbind(round(x[[scale]],3), x$which[,-1])
  colnames(m)[1] <- scale
  m[order(m[,1]), ]
}

subsTable(leaps, scale="adjr2")
#regsubsets() 函数默认不会显示 所有可能的变量组合，
#而是返回每个变量数量（k）下的 最优模型
##### 5.2broom::tidy神包查看----------------------------------------------------
broom::tidy(leaps)
##### 5.3通过Cp图判断-----------------------------------------------------------
plot(leaps, scale = "Cp") # 通过Cp判断

##### 5.4表格化全子集结果-------------------------------------------------------
library(gt)
library(tidyverse)
library(scales)
broom::tidy(leaps) %>%
  select(-`(Intercept)`) %>%
  rownames_to_column(var = "n_vars") %>%
  gt(rowname_col = "n_vars") %>%
  gt::data_color(
    columns = cho:hba,
    fn = col_numeric(
      palette = c("#fdae61", "#abdda4"),
      domain = c(0, 1)) 
  ) %>%
  gt::fmt_number(r.squared:mallows_cp, n_sigfig = 4)

##### 5.5图像化全子集结果-------------------------------------------------------
broom::tidy(leaps) %>%
  select(r.squared:mallows_cp) %>%
  mutate(n_vars = 1:n()) %>%
  pivot_longer(cols = -n_vars, names_to = "metric") %>%
  ggplot(aes(x = n_vars, y = value)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = . %>%
      group_by(metric) %>%
      filter(value == ifelse(str_detect(metric, "r.squared"),
                             max(value), min(value))),
    aes(xintercept = n_vars), lty = 2) +
  theme_bw()+
  facet_wrap(~ metric, scales = "free_y")


