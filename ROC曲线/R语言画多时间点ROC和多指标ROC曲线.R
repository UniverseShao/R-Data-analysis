# 1.多个时间点的ROC曲线---------------------------------------------------------
##### 1.1加载R包和数据----------------------------------------------------------
rm(list = ls())
library(timeROC)
library(survival)
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\timeROC.RData")
##### 1.2画图-------------------------------------------------------------------
#下面是画图代码

ROC <- timeROC(T=df$futime,    # 生存时间（如：随访时间，单位通常为年）
               delta=df$event,   # 事件状态（1=发生事件/死亡，0=删失）
               marker=df$riskScore,   # 预测指标/风险评分（数值越大风险越高）
               cause=1,                #阳性结局指标数值
               weighting="marginal",   #计算方法，默认为marginal
               times=c(1, 2, 3),       #时间点，选取1年，3年和5年的生存率
               iid=TRUE)
plot(ROC, 
     time=1, col="red", lwd=2, title = "")   #time是时间点，col是线条颜色
plot(ROC,
     time=2, col="blue", add=TRUE, lwd=2)    #add指是否添加在上一张图中
plot(ROC,
     time=3, col="orange", add=TRUE, lwd=2)

#添加标签信息
legend("bottomright",
       c(paste0("AUC at 1 year: ",round(ROC[["AUC"]][1],2)), # 1年AUC值（保留2位小数）
         paste0("AUC at 2 year: ",round(ROC[["AUC"]][2],2)), # 2年AUC值
         paste0("AUC at 3 year: ",round(ROC[["AUC"]][3],2))),# 3年AUC值
       col=c("red", "blue", "orange"),
       lty=1, lwd=2,bty = "n")
# 线条类型（1=实线） # 线条宽度 # 取消图例边框（"n"=无边框）

# 2.多指标的ROC曲线-------------------------------------------------------------

# 2.1加载数据和R包--------------------------------------------------------------

knitr::kable(df2[1:10,])
##### 2.2timeROC()生成不同指标ROC-----------------------------------------------
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
##### 2.3画图-------------------------------------------------------------------
# 拼图
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


# 3.生存分析中的时间依赖性ROC的理论详解及作用-----------------------------------

##### 3.1传统ROC（用于二分类结局）----------------------------------------------
#输入：诊断测试结果 + 金标准分类（患病/未患病）

#输出：一条ROC曲线（所有可能截断值对应的TPR/FPR）

#计算基础：直接比较预测值和真实状态的2×2表格

##### 3.2时间依赖性ROC（用于生存数据）------------------------------------------
#输入：生存时间 + 事件状态 + 预测指标（连续值或分类）

#输出：在特定时间点评估预测准确性（如3年生存率）

#核心创新：解决生存数据的两个特性：

#时间因素：结局发生在不同时间点

#删失数据：部分患者未观察到事件

##### 3.3预测指标（连续值或分类）的含义和作用-----------------------------------
#即 timeROC(   marker=df2$n   )中的marker即是预测指标
#ROC.age的预测指标是age
#ROC.gender的预测指标是gender
#RPC.risk的预测指标是riskScore
#在 time = 3时，就是这些预测指标评分预测三年事件发生的数目/真实事件发生数目

#举例riskScore连续变量，值越大风险越高
#时间点（t）：3年
#那么ROC.risk画出来的图，横坐标就是用riskScore预测事件发生的假阳性率，即
#假阳性率（1-特异度），即错误的预测事件发生的患者数目/所有存活≥3年的患者数目

#真阳性率（灵敏度），即正确的预测事件发生的患者数目/所有存活小于3年的患者数目

##### 3.4指标曲线靠左上，指标预测效果好-----------------------------------------


