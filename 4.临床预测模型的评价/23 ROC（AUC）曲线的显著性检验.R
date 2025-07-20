# 1.二分类资料的ROC比较---------------------------------------------------------
library(pROC)

data(aSAH)
dim(aSAH)

str(aSAH)
##### 1.1比较-----------------------------------------------------------------------
roc1 <- roc(aSAH$outcome,aSAH$s100b)
roc2 <- roc(aSAH$outcome,aSAH$ndka)
res <- roc.test(roc1,roc2)
res
##### 1.2图中显示
rocobj1 <- plot.roc(aSAH$outcome, aSAH$s100,percent=TRUE, col="#1c61b6")
rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="#008600")
legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)
testobj <- roc.test(rocobj1, rocobj2)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

# 2.生存资料ROC的比较-----------------------------------------------------------
rm(list = ls())
library(timeROC)
library(survival)
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\timeROC.RData")
str(df2)

# riskScore的ROC曲线
ROC.risk <- timeROC(T=df2$futime,
                    delta=df2$event,   
                    marker=df2$riskScore,   
                    cause=1,                
                    weighting="marginal",   
                    times=3,  # c(1,2) 
                    iid=TRUE)


# age的ROC曲线
ROC.age <- timeROC(T=df2$futime,   
                   delta=df2$event,   
                   marker=df2$age,   
                   cause=1,   
                   weighting="marginal",   
                   times=3,   # c(1,2)
                   iid=TRUE)

##### 2.1compare()函数进行比较------------------------------------------------------
compare(ROC.risk, ROC.age)


# riskScore的ROC曲线
ROC.risk <- timeROC(T=df2$futime,
                    delta=df2$event,   
                    marker=df2$riskScore,   
                    cause=1,                
                    weighting="marginal",   
                    times=c(1,2),
                    iid=TRUE)


# age的ROC曲线
ROC.age <- timeROC(T=df2$futime,   
                   delta=df2$event,   
                   marker=df2$age,   
                   cause=1,   
                   weighting="marginal",   
                   times=c(1,2),
                   iid=TRUE)

compare(ROC.risk, ROC.age)
compare(ROC.risk, ROC.age, adjusted = T) # 计算调整p值
