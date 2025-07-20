#surv_cutpoint()可以同时计算多个变量的最佳截断值，X-tile软件一次只能计算1个变量的最佳截断值；
#surv_cutpoint()只能找1个最佳截断值，X-tile软件还可以找2个最佳截断值，把所有数据分为3组；
#surv_cutpoint()是基于maxstat包计算出maximally selected rank statistics，X-tile软件是基于log-rank卡方值

# surv_cutpoint()----------------------------------------------------------------
#使用myeloma数据进行演示
library(survival)
library(survminer)
data(myeloma)
head(myeloma)
#先使用surv_cutpoint()函数找最佳截断值
res.cut <- surv_cutpoint(myeloma, time = "time", event = "event",
                         variables = c("CCND1", "CRIM1", "DEPDC1") # 找这3个变量的最佳切点
)

summary(res.cut)
#查看根据最佳切点进行分组后的数据分布情况
# 2. Plot cutpoint for DEPDC1
plot(res.cut, "DEPDC1", palette = "npg")
#使用surv_categorize()根据最佳截断值对数据进行分组，这样数据就根据最佳截断值变成了高表达/低表达组
# 3. Categorize variables
res.cat <- surv_categorize(res.cut)
head(res.cat)
#根据最佳切点绘制生存曲线
# 4. Fit survival curves and visualize
library("survival")
fit <- survfit(Surv(time, event) ~DEPDC1, data = res.cat)
ggsurvplot(fit, data = res.cat, pval = T)
