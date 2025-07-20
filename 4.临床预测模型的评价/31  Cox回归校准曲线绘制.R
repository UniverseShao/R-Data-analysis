# 31.1准备数据------------------------------------------------------------------
library(survival)
rm(list = ls())
dim(lung)
str(lung)

library(dplyr)
library(tidyr)
lung <- lung %>% 
  mutate(status=ifelse(status == 2,1,0))
str(lung)

##### 31.1.1数据划分为训练集、测试集--------------------------------------------

set.seed(123)
ind1 <- sample(1:nrow(lung),nrow(lung)*0.7)
train_df <- lung[ind1,]

set.seed(563435)
ind2 <- sample(1:nrow(lung),nrow(lung)*0.7)
test_df <- lung[ind2, ]
dim(train_df)
dim(test_df)

# 31.2 方法1：rms---------------------------------------------------------------
library(rms)

# 必须先打包数据
dd <- datadist(train_df)
options(datadist = "dd")
units(train_df$time) <- "day" # 单位设置为：天

##### 31.2.1建立cox回归模型，时间点选择100天------------------------------------
coxfit1 <- cph(Surv(time, status) ~ sex + ph.ecog + ph.karno + wt.loss,
               data = train_df, x = T, y = T, surv = T,
               time.inc = 100 # 100天，告诉模型在时间轴上的哪个点计算和存储基线生存函数的估计值
#后续使用survest()或其他相关函数预测生存概率时，如果指定了时间点，模型会使用这个存储的信息进行插值或计算
               )
summary(coxfit1)
coxfit1
##### 31.2.2绘制训练集校准曲线--------------------------------------------------
# m=40表示以40个样本为1组，一般取4-6组，我们这个数据样本量太少了
# u=100和上面的time.inc对应
cal1 <- calibrate(coxfit1, cmethod = "KM", method = "boot",
                  u = 100, m = 40, B = 500) 
#u = 100 指定评估校准的时间点（u 代表 "time"）
#m = 40 表示每个组大约包含 40 个样本
#B = 500 指定 Bootstrap 重抽样次数（B 代表 "Bootstrap samples"）
plot(cal1)

plot(cal1,
     lwd = 2, # 误差线粗细
     lty = 1, # 误差线类型，可选0-6
     errbar.col = c("#2166AC"), # 误差线颜色
     xlim = c(0.7,1),ylim= c(0.7,1), # 坐标轴范围
     xlab = "Prediced OS",ylab = "Observed OS",
     cex.lab=1.2, cex.axis=1, cex.main=1.2, cex.sub=0.6) # 字体大小
lines(cal1[,c('mean.predicted',"KM")], 
      type = 'b', # 连线的类型，可以是"p","b","o"
      lwd = 3, # 连线的粗细
      pch = 16, # 点的形状，可以是0-20
      col = "tomato") # 连线的颜色
box(lwd = 2) # 边框粗细
abline(0,1,lty = 3, # 对角线为虚线
       lwd = 2, # 对角线的粗细
       col = "grey70" # 对角线的颜色
) 

##### 31.2.3绘制测试集校准曲线--------------------------------------------------
# 获取测试集的预测的生存概率，这一步有没有都行
estimates <- survest(coxfit1,newdata=test_df,times=100)$surv

vs <- val.surv(coxfit1, newdata = test_df,
               S = Surv(test_df$time,test_df$status),
               est.surv = estimates,# 这一步有没有都行
               u = 100 # 时间点，也是选100天
)
plot(vs)

# 31.3 方法2：riskRegression----------------------------------------------------

# 删除缺失值
df2 <- na.omit(lung)
# 划分数据
set.seed(123)
ind1 <- sample(1:nrow(df2),nrow(df2)*0.9)
train_df <- df2[ind1,]
set.seed(563435)
ind2 <- sample(1:nrow(df2),nrow(df2)*0.9)
test_df <- df2[ind2, ]
dim(train_df)
dim(test_df)

##### 31.3.1构建模型------------------------------------------------------------
cox_fit1 <- coxph(Surv(time, status) ~ sex + ph.ecog + ph.karno,
                  data = train_df,x = T, y = T)
#x = T 在拟合的 Cox 模型对象中 存储原始设计矩阵（协变量矩阵）
#y = T 在拟合的 Cox 模型对象中 存储原始生存时间数据（Surv(time, status) 的响应变量）

##### 31.3.2绘制训练集校准曲线--------------------------------------------------
# 画图
library(riskRegression)
set.seed(1)
cox_fit_s <- Score(list("fit1" = cox_fit1),
                   formula = Surv(time, status) ~ 1,
                   data = train_df,
                   plots = "calibration",
                   conf.int = T,
                   B = 500,
                   M = 50, # 每组的人数
                   times=c(100) # 时间点选100天
)
plotCalibration(cox_fit_s,cens.method="local",# 减少输出日志
                xlab = "Predicted Risk",
                ylab = "Observerd RISK")


##### 31.3.3ggplot2画图---------------------------------------------------------
# 获取数据
data_all <- plotCalibration(cox_fit_s,plot = F,cens.method="local")

# 数据转换
plot_df <- data_all$plotFrames$fit1

# 画图
library(ggplot2)
ggplot(plot_df, aes(Pred,Obs))+
  geom_point()+
  geom_line(linewidth=1.2)+
  scale_x_continuous(limits = c(0,0.5),name = "Predicted Risk")+
  scale_y_continuous(limits = c(0,0.5),name = "Observerd Risk")+
  geom_abline(slope = 1,intercept = 0,lty=2)+
  theme_bw()

##### 31.3.4绘制测试集校准曲线--------------------------------------------------
set.seed(1)
cox_fit_s <- Score(list("fit1" = cox_fit1),
                   formula = Surv(time, status) ~ 1,
                   data = test_df, # 测试集
                   plots = "calibration",
                   B = 500,
                   M = 50,
                   times=c(100) # 时间点
)
plotCalibration(cox_fit_s,cens.method="local",# 减少输出日志
                xlab = "Predicted Risk",
                ylab = "Observerd Risk")


