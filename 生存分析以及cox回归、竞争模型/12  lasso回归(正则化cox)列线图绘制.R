# 33.1 安装(若没有安装)---------------------------------------------------------
install.packages("hdnom")
# 33.2 加载R包和数据------------------------------------------------------------
rm(list = ls())
library(hdnom)

data("smart")
x <- as.matrix(smart[, -c(1, 2)])
time <- smart$TEVENT
event <- smart$EVENT
y <- survival::Surv(time, event)

# 33.3 拟合正则化COX模型--------------------------------------------------------
suppressMessages(library("doParallel"))
registerDoParallel(detectCores())

fit <- fit_lasso(x, y, nfolds = 10, rule = "lambda.1se", seed = 1001)
fit
names(fit)

# 33.4 列线图-------------------------------------------------------------------
##### 33.4.1提取模型对象和超参数------------------------------------------------
model <- fit$model
alpha <- fit$alpha
lambda <- fit$lambda
adapen <- fit$pen_factor

##### 33.4.2绘制列线图----------------------------------------------------------
nom <- as_nomogram(
  fit, x, time, event,
  pred.at = 365 * 2,
  funlabel = "2-Year Overall Survival Probability"
)

plot(nom)


# 33.5 模型验证-----------------------------------------------------------------
##### 33.5.1 内部验证-----------------------------------------------------------

val_int <- validate(
  x, time, event,
  model.type = "lasso",
  alpha = 1, # lasso的alpha是1
  lambda = lambda,
  method = "bootstrap", boot.times = 10,
  tauc.type = "UNO", tauc.time = seq(1, 5, 0.5) * 365,
  seed = 42, trace = FALSE
)
# 最后一行给出了不同时间截点的time-dependent AUC
print(val_int)
# 给出time-dependent AUC的均值、最大值、最小值等信息
summary(val_int)
#把time-dependent AUC画出来
plot(val_int)

##### 33.5.2 外部验证----------------------------------------------------------- 
#也就是用一个新的数据集进行验证
#从smart中随机抽取1000个样本作为外部验证集

x_new <- as.matrix(smart[, -c(1, 2)])[1001:2000, ]
time_new <- smart$TEVENT[1001:2000]
event_new <- smart$EVENT[1001:2000]

val_ext <- validate_external(
  fit, x, time, event,
  x_new, time_new, event_new,
  tauc.type = "UNO",
  tauc.time = seq(0.25, 2, 0.25) * 365 # 时间截点和内部验证不同了
)

# 3个查看结果的方法，也是内部验证一样的
print(val_ext)

summary(val_ext)

plot(val_ext)

#内部验证：评估长期（1~5年），可能因为训练数据来自长期随访研究。
#外部验证：评估短期（0.25~2年），可能因为：
#外部数据来自术后短期随访队列；
#或外部数据中2年后失访率高，无法可靠评估


# 33.6 模型校准-----------------------------------------------------------------
#也就是绘制校准曲线，同样是支持内部校准和外部校准
##### 33.6.1 内部校准曲线-------------------------------------------------------
#内部校准通过calibrate实现

