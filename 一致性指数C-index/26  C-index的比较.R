# 26.1 二分类资料C-index的比较--------------------------------------------------
#二分类资料的AUC和C-index是一样的，所以可以参考Chapter 23关于ROC曲线的显著性检验
rm(list = ls())
# 26.2 生存资料C-index的比较----------------------------------------------------
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\timeROC.RData")
str(df2)
#compareC包，专门用来比较生存资料的C-index
library(compareC)
#只要提供4个参数：time，status，第一个指标，第二个指标，即可
compareC(df2$futime,
         df2$event,
         df2$riskScore,
         df2$age
)

# 26.3 两个cox模型的比较--------------------------------------------------------
# 使用anova()函数即可
library(survival)
library(dplyr)

df1 <- lung %>% 
  mutate(status=ifelse(status == 1,1,0))


cox_fit1 <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
                  data = df1,x = T, y = T)

cox_fit2 <- coxph(Surv(time, status) ~ ph.ecog + ph.karno + pat.karno,
                  data = df1,x = T, y = T)

anova(cox_fit1,cox_fit2)
##### 26.3.1anova()结果解释-----------------------------------------------------
## Analysis of Deviance Table
##  Cox model: response is  Surv(time, status)
##  Model 1: ~ age + sex + ph.ecog + ph.karno + pat.karno
##  Model 2: ~ ph.ecog + ph.karno + pat.karno
##    loglik  Chisq Df Pr(>|Chi|)  
## 1 -257.97                       
## 2 -261.94 7.9486  2    0.01879 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#(1)
#Model 1 的对数似然值（-257.97）高于 Model 2（-261.94），
#表明包含 age 和 sex 后模型拟合更好
#(2)
#P 值（显著性水平）说明 age 和 sex 的联合作用对模型有显著贡献（不应被剔除）

##### 26.3.2rms包lrtest()函数---------------------------------------------------

suppressMessages(library(rms))
dd <- datadist(lung)
options(datadist="dd")

cox_fit1 <- cph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
                data = df1,x = T, y = T)

cox_fit2 <- cph(Surv(time, status) ~ ph.ecog + ph.karno + pat.karno,
                data = df1,x = T, y = T)

lrtest(cox_fit1,cox_fit2)
## Model 1: Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno
## Model 2: Surv(time, status) ~ ph.ecog + ph.karno + pat.karno
## 
## L.R. Chisq       d.f.          P 
## 7.94861288 2.00000000 0.01879233
