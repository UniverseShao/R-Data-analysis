# 16.1 加载数据-----------------------------------------------------------------
rm(list = ls())
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\lnc_expr_clin.RData")
#去掉没有生存信息的样本
lnc_expr_clin1 <- lnc_expr_clin[!is.na(lnc_expr_clin$time_months),]
lnc_expr_clin1 <- lnc_expr_clin1[lnc_expr_clin1$time_months>0,]
#选择其中一部分数据
dat.cox <- lnc_expr_clin1[,c(72:73,1:59)]
dim(dat.cox)
dat.cox[1:4,1:6]
# 16.2 建立模型-----------------------------------------------------------------
library(survival)

fit.cox <- coxph(Surv(time_months,event)~.,data = dat.cox)
fit.cox

# 16.3 逐步选择-----------------------------------------------------------------
fit.step <- step(fit.cox,direction = "both")
summary(fit.step)
#最终59个变量剩下17个，筛选效果还不错
##### 16.3.1查看最终的AIC和BIC--------------------------------------------------
# 初始模型的AIC
AIC(fit.cox)
# 筛选后的AIC和BIC
AIC(fit.step)
BIC(fit.step)
##### 16.3.2查看回归系数--------------------------------------------------------
step.coef <- coef(fit.step)
step.coef
##### 16.3.3提取变量名----------------------------------------------------------
step.lnc <- names(coef(fit.step))
step.lnc
##### 16.4.4broom::tidy直接提取结果---------------------------------------------
broom::tidy(fit.step)
broom::glance(fit.step)

# 16.4 自助法stepwise-----------------------------------------------------------
#自助法重抽样进行逐步筛选变量，比如进行1000次bootstrap
#下面是一个10次bootstrap的逐步选择法
library(bootStepAIC)
# 10次bootstrap  Bootstrap 重抽样 结合 逐步回归（stepAIC）
fit.boot <- boot.stepAIC(fit.cox,data=dat.cox,direction="both",B=10,seed=123)
#提取变量名字
names(coef(fit.boot$OrigStepAIC))



