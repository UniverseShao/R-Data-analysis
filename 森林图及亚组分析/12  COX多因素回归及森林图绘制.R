#准备数据####
rm(list = ls())
library(survival)
library(survminer)


#因子化处理变量####
lung$sex <- factor(lung$sex, labels = c("female","male"))
lung$ph.ecog <- factor(lung$ph.ecog, 
                       labels = c("asymptomatic", "symptomatic",'in bed <50%',
                                  'in bed >50%'))

str(lung)
#拟合模型####

fit.cox <- coxph(Surv(time, status) ~ sex + age + ph.karno, data = lung)
summary(fit.cox)


#等比例风险检验####
ftest <- cox.zph(fit.cox)

#森林图绘制####

# 为了森林图好看点，多选几个变量
fit.cox <- coxph(Surv(time, status) ~ . , data = lung)

ggforest(fit.cox, data = lung,
         main = "Hazard ratio",
         cpositions = c(0.01, 0.15, 0.35), # 更改前三列的相对位置
         fontsize = 0.7,
         refLabel = "reference",
         noDigits = 2
)

























