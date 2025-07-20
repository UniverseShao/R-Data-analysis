# 14.1 准备数据-----------------------------------------------------------------
library(survival)
library(rms)

rm(list = ls())
dim(lung)
str(lung)
# 14.2 建立模型和列线图---------------------------------------------------------
dd <- datadist(lung)
options(datadist = "dd")
coxfit <- cph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
              data = lung ,surv = T)

# 构建生存函数，注意你的最大生存时间
surv <- Survival(coxfit) 
surv1 <- function(x) surv(365,x) # 1年OS
surv2 <- function(x) surv(365*2,x) # 2年OS

nom <- nomogram(coxfit,
                fun = list(surv1,surv2),
                lp = T,
                funlabel = c('1-year survival Probability',
                             '2-year survival Probability'),
                maxscale = 100,
                fun.at = c(0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1))
# 画图
plot(nom, 
     lplabel="Linear Predictor",
     xfrac = 0.2, # 左侧标签距离坐标轴的距离
     #varname.label = TRUE, 
     tcl = -0.2, # 刻度长短和方向 
     lmgp = 0.1, # 坐标轴标签距离坐标轴远近
     points.label ='Points', 
     total.points.label = 'Total Points',
     cap.labels = FALSE,
     cex.var = 1, # 左侧标签字体大小
     cex.axis = 1, # 坐标轴字体大小
     col.grid = gray(c(0.8, 0.95))) # 竖线颜色

# 14.3 计算分数-----------------------------------------------------------------
#使用nomogramFormula计算每个患者的列线图得分
library(nomogramFormula)
results <- formula_lp(nomogram = nom)#提取线性预测公式
points1 <- points_cal(formula = results$formula, lp = coxfit$linear.predictors)
#formula：上一步提取的公式（results$formula）,
#lp：Cox 模型（coxfit）中每个样本的线性预测值
#或者
#results <- formula_rd(nomogram = nom)
#points1 <- points_cal(formula = results$formula, rd = lung)

length(points1)
## [1] 223
head(points1)
##         1         2         3         4         5         6 
## 129.96853  98.56938  90.51815 142.40181 102.54570 104.51291

# 14.4 分层---------------------------------------------------------------------

library(tidyr)
library(survminer)

##### 14.4.1去掉缺失值----------------------------------------------------------
tmp <- lung %>% 
  drop_na(ph.ecog,ph.karno,pat.karno)
dim(tmp)
## [1] 223  10

tmp$points <- points1

##### 14.4.2确定最佳截点，然后根据最佳截点分层----------------------------------
res.cut <- surv_cutpoint(tmp, time = "time", event = "status",
                         variables = "points"
)
summary(res.cut)
##### 14.4.3根据最佳截点分层----------------------------------------------------
res.cat <- surv_categorize(res.cut)
##### 14.4.4绘制生存曲线--------------------------------------------------------
library("survival")
fit <- survfit(Surv(time, status) ~ points, data = res.cat)
ggsurvplot(fit, data = res.cat, pval = T)

# 14.5 外部验证集---------------------------------------------------------------
# 假设这是外部验证集
valdf <- na.omit(lung[1:100,])
# 计算分数
results <- formula_rd(nomogram = nom)
points_val <- points_cal(formula = results$formula, 
                         rd = valdf) # 外部验证集
length(points_val)
head(points_val)
valdf$points <- points_val
valdf$groups <- ifelse(valdf$points>109.2188,"high","low")
#绘制外部验证集的生存曲线
library("survival")
fit <- survfit(Surv(time, status) ~ groups, data = valdf)
ggsurvplot(fit, data = valdf, pval = T)
