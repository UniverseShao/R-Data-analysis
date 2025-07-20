# 1.加载R包和数据---------------------------------------------------------------
rm(list = ls())
library(timeROC)
library(survival)
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\timeROC.RData")
# 2.多个时间点ROC---------------------------------------------------------------
str(df)

# 构建timeroc

ROC <- timeROC(T=df$futime,   
               delta=df$event,   
               marker=df$riskScore,   
               cause=1,                #阳性结局指标数值
               weighting="marginal",   #计算方法，默认为marginal
               times=c(1, 2, 3),       #时间点，选取1年，3年和5年的生存率
               iid=TRUE)

ROC   #查看模型变量信息
plot(ROC, 
     time=1, col="red", lwd=2, title = "")   #time是时间点，col是线条颜色
plot(ROC,
     time=2, col="blue", add=TRUE, lwd=2)    #add指是否添加在上一张图中
plot(ROC,
     time=3, col="orange", add=TRUE, lwd=2)

#添加标签信息
legend("bottomright",
       c(paste0("AUC at 1 year: ",round(ROC[["AUC"]][1],2)), 
         paste0("AUC at 2 year: ",round(ROC[["AUC"]][2],2)), 
         paste0("AUC at 3 year: ",round(ROC[["AUC"]][3],2))),
       col=c("red", "blue", "orange"),
       lty=1, lwd=2,bty = "n") 



# 3.多指标ROC-------------------------------------------------------------------
str(df2)

# riskScore的ROC曲线
ROC.risk <- timeROC(T=df2$futime,
                    delta=df2$event,   
                    marker=df2$riskScore,   
                    cause=1,                
                    weighting="marginal",   
                    times=3,   
                    iid=TRUE)


# gender的ROC曲线
ROC.gender <- timeROC(T=df2$futime,   
                      delta=df2$event,   
                      marker=df2$gender,   
                      cause=1,   
                      weighting="marginal",   
                      times=3,   
                      iid=TRUE)


# age的ROC曲线
ROC.age <- timeROC(T=df2$futime,   
                   delta=df2$event,   
                   marker=df2$age,   
                   cause=1,   
                   weighting="marginal",   
                   times=3,   
                   iid=TRUE)


# T分期的ROC曲线
ROC.T <- timeROC(T=df2$futime,
                 delta=df2$event,  
                 marker=df2$t,   
                 cause=1, 
                 weighting="marginal", 
                 times=3, 
                 iid=TRUE)


# N分期的ROC曲线
ROC.N <- timeROC(T=df2$futime,   
                 delta=df2$event,   
                 marker=df2$n,   
                 cause=1,   
                 weighting="marginal",   
                 times=3,   
                 iid=TRUE)


# M分期的ROC曲线
ROC.M <- timeROC(T=df2$futime,   
                 delta=df2$event,   
                 marker=df2$m,   
                 cause=1,   
                 weighting="marginal",   
                 times=3,   
                 iid=TRUE)


# 画图

plot(ROC.risk, time = 3, col="#E41A1C", lwd=2, title = "")
plot(ROC.gender, time = 3, col="#A65628", lwd=2, add = T)
plot(ROC.age, time = 3, col="#4DAF4A", lwd=2, add = T)
plot(ROC.T, time = 3, col="#377EB8", lwd=2, add = T)
plot(ROC.N, time = 3, col="#984EA3", lwd=2, add = T)
plot(ROC.M, time = 3, col="#FFFF33", lwd=2, add = T)
legend("bottomright",
       c(paste0("Risk score: ",round(ROC.risk[["AUC"]][2],2)), 
         paste0("gender: ",round(ROC.gender[["AUC"]][2],2)), 
         paste0("age: ",round(ROC.age[["AUC"]][2],2)),
         paste0("T: ",round(ROC.T[["AUC"]][2],2)),
         paste0("N: ",round(ROC.N[["AUC"]][2],2)),
         paste0("M: ",round(ROC.M[["AUC"]][2],2))
       ),
       col=c("#E41A1C", "#A65628", "#4DAF4A","#377EB8","#984EA3","#FFFF33"),
       lty=1, lwd=2,bty = "n")  


