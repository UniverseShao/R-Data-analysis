rm(list = ls())
library(rms)
# 10.1 逻辑回归的RCS列线图-------------------------------------------------------
# 加载数据
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\titanic3.rdata")
# 使用rms前先把数据打包
dd <- datadist(titanic3); options(datadist='dd')

# 逻辑回归的立方样条
f <- lrm(survived ~ rcs(sqrt(age),5) + sex, data=titanic3)
f
#画图
nom <- nomogram(f, fun=plogis, funlabel="Risk of Death")  
plot(nom) 

# 10.2 COX回归RCS的列线图

rm(list = ls())
library(survival)

# 打包数据
dd <- datadist(lung)
options(datadist = "dd")

coxfit <- cph(Surv(time, status) ~ rcs(sqrt(age),5) + sex,
              data = lung, x=T,y=T,surv = T
)

# 构建生存函数，计算生存率，注意你的最大生存时间
surv <- Survival(coxfit) 
surv1 <- function(x) surv(365,x) # 1年OS
surv2 <- function(x) surv(365*2,x) # 2年OS

# 画图
nom <- nomogram(coxfit, fun = list(surv1,surv2),
                funlabel = c('1-year survival Probability',
                             '2-year survival Probability')
)

plot(nom)
