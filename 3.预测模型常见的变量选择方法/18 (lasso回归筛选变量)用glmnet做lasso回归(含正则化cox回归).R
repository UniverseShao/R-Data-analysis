# 1.lasso回归的作用-------------------------------------------------------------


#lasso回归就可以通过算法让其中一些不重要变量的系数变成0，达到筛选变量的目的
#除此之外，减少自变量个数，还可以降低模型复杂度，防止过拟合
#让系数变小，就是大家常说的对系数进行惩罚penalty，也被称为正则化regularization

#glmnet包中，lambda是总的正则化程度，该值越大惩罚力度越大，
#最终保留的变量越少，模型复杂度越低

#alpha是L1正则化的比例，当alpha=1时，就是lasso
#alpha=0时，就是岭回归
#0<alpha<1时，就是弹性网络

# 2.建模------------------------------------------------------------------------
##### 2.1构建数据和建立回归模型-------------------------------------------------
rm(list = ls())
library(glmnet)
data(BinomialExample)
x <- BinomialExample$x#自变量矩阵
y <- BinomialExample$y#因变量
class(x)
dim(x)
x[1:4,1:4]
class(y)
head(y)
#glmnet需要的自变量格式，需要是matrix或者稀疏矩阵格式
#family用来指定不同的模型类型，对于二分类数据，应该选择binomial
#family的其他选项如下："gaussian"（默认）, "poisson", "multinomial", "cox", "mgaussian"
fit <- glmnet(x, y, family = "binomial")

##### 2.2可视化-----------------------------------------------------------------
###### 2.2.1L1作为横坐标--------------------------------------------------------
plot(fit,label = T)#横坐标是L1正则化程度
###### 2.2.2log-lambda作为横坐标------------------------------------------------
plot(fit, xvar = "lambda", label = T)#横坐标是log-lambda，可以看做是正则化程度，也就是对系数进行惩罚的力度
#正则化程度，也就是对系数进行惩罚的力度
#随着lambda值变大，系数值逐渐减小，直至为0
#上面的横坐标也显示随着lambda值变大，保留的变量数量也越来越少
###### 2.2.3dev作为横坐标-------------------------------------------------------
plot(fit, xvar = "dev", label = TRUE)
#坐标是模型解释的偏差百分比
#图形的最右侧部分，模型能够解释的偏差百分比基本变化不大
#但是模型系数基本都是往上或往下“飘”的很厉害

# 3.打印结果--------------------------------------------------------------------
print(fit) # 直接fit也可
#左侧的df是非零系数的个数
#中间的%Dev是模型解释的偏差百分比
#右侧的Lambda是总惩罚值大小

# 3.查看变量系数----------------------------------------------------------------
##### 3.1查看某个Lambda值下的变量系数-------------------------------------------
coef(fit, s = 0.065380)
#共有12个变量的系数不是0

##### 3.2同时指定多个lambda值---------------------------------------------------
coef(fit, s = c(0.065380,0.078750))

##### 3.3exact参数的使用--------------------------------------------------------
#如果exact = TRUE，那么当一个lambda不在默认的lambda值中时
#函数会重新使用这个lambda值拟合模型然后给出结果
# 可以看前面的print(fit)的结果，看看lambda的取值有哪些
any(fit$lambda == 0.08)
coef.apprx <- coef(fit, s = 0.08, exact = FALSE)
coef.exact <- coef(fit, s = 0.08, exact = TRUE, x=x, y=y)
cbind2(coef.exact[which(coef.exact != 0)], 
       coef.apprx[which(coef.apprx != 0)])
#exact = TRUE时，需要提供x和y，因为需要重新拟合模型

# 4.预测新数据------------------------------------------------------------------
nx <- head(x) #随便准备的新的测试数据

predict(fit, newx = nx, s = c(0.065380,0.078750))
##              s1         s2
## [1,] -0.7609757 -0.5755105
## [2,]  1.4563904  1.1266031
## [3,]  0.4415409  0.3981256
## [4,] -1.1676684 -0.9923334
## [5,]  0.5730604  0.5612494
## [6,]  0.3064590  0.1926588

##### 4.1线性预测结果的解释-----------------------------------------------------
#正数：表示该样本预测为 y=1 的概率 > 0.5
#负数：表示该样本预测为 y=1 的概率 < 0.5

#link：线性预测值，默认是这个
#response：预测概率
#class：预测类别

##### 4.2预测概率---------------------------------------------------------------
predict(fit, newx = nx, s = c(0.065380,0.078750), type = "response")

# 5.交叉验证--------------------------------------------------------------------
cvfit <- cv.glmnet(x, y)
#对于逻辑回归，type.measure可以是以下取值
#mse：均方误差；
#deviance：偏差；
#mae：平均绝对误差，mean absolute error；
#class：错分率；
#auc：只能用于二分类逻辑回归
cvfit
#Call:  cv.glmnet(x = x, y = y) 

#Measure: Mean-Squared Error 

#      Lambda Index   Measure  SE    Nonzero
#min 0.02830    24  0.1314 0.01446      17
#1se 0.05428    17  0.1438 0.01588      13
##### 5.1交叉验证结果解读-------------------------------------------------------
#Nonzero指的是非零变量的数目
#Index 指的是的Lambda值在Lambda表中排第几行
#λ min，意思是偏差最小时的λ ，lambda取值下，模型拟合效果最高
#λ-se，意思是最小λ右侧的1个标准误，该λ取值下，构建模型的拟合效果也很好
#同时纳入方程的个数更少，模型更简单，临床上一般会选择右侧的λ1-se作为最终方程筛选标准

# 6.plot方法验证----------------------------------------------------------------
plot(cvfit)

#查看这两个lambda值
cvfit$lambda.min
cvfit$lambda.1se

##### 6.1换一个type.measure-----------------------------------------------------
cvfit1 <- cv.glmnet(x, y, family = "binomial", type.measure = "auc")
plot(cvfit1)


# 7.coef方法--------------------------------------------------------------------
# 此时s不能同时使用多个值
coef(cvfit, s = "lambda.min")
coef(cvfit, s = "lambda.1se") # 这个是默认值
#coef()的结果都是稀疏矩阵格式，计算效率更高，不方便后续使用
as.matrix(coef(cvfit))


# 8.predict方法-----------------------------------------------------------------
predict(cvfit, newx = x[1:5,], s = "lambda.min")

##### 8.1一些参数解释-----------------------------------------------------------
#alpha：可以看做是L1正则化的比例，当alpha=1时，就是lasso，当alpha=0时，就是岭回归，当0<alpha<1时，就是弹性网络。
#weights：不同观测的权重，默认都是1。（glmnet会自动对权重进行重新标准化，使得所有观测的权重相加等于样本数量）。
#nlambda：lambda的取值个数，默认是100。
#lambda：用户可以通过这个参数自己指定lambda的取值。
#standardize：逻辑值，是否在拟合模型前对自变量进行标准化，默认是TRU
# 简单定义一下，前50个是1，后50个是2
wts <-  c(rep(1,50), rep(2,50))
fit1 <- glmnet(x, y, alpha = 0.2, weights = wts, nlambda = 20)

print(fit1)

##### 8.2在测试集评估模型-------------------------------------------------------
###### 8.2.1assess.glmnet()-----------------------------------------------------
data(BinomialExample)
x <- BinomialExample$x
y <- BinomialExample$y
itrain <- 1:70 # 前70个作为训练集
fit <- glmnet(x[itrain, ], y[itrain], family = "binomial", nlambda = 6)
# 在测试集评估模型
assess.glmnet(fit, newx = x[-itrain, ], newy = y[-itrain])

###### 8.2.2每个family对应的性能指标--------------------------------------------
glmnet.measures()
## $gaussian
## [1] "mse" "mae"
## 
## $binomial
## [1] "deviance" "class"    "auc"      "mse"      "mae"     
## 
## $poisson
## [1] "deviance" "mse"      "mae"     
## 
## $cox
## [1] "deviance" "C"       
## 
## $multinomial
## [1] "deviance" "class"    "mse"      "mae"     
## 
## $mgaussian
## [1] "mse" "mae"
## 
## $GLM
## [1] "deviance" "mse"      "mae"

###### 8.2.3交叉验证在assess.glmnet()同样适用-----------------------------------
cfit <- cv.glmnet(x[itrain, ], y[itrain], family = "binomial", nlambda = 30)
assess.glmnet(cfit, newx = x[-itrain, ], newy = y[-itrain])
#不过此时默认使用的lambda值是lambda.1se，也可以使用lambda.min
assess.glmnet(cfit, newx = x[-itrain, ],newy = y[-itrain], s = "lambda.min")
#也可以获取训练集的各种指标，只要在建模时使用keep=TRUE参数即可
cfit <- cv.glmnet(x, y, family = "binomial", keep = TRUE, nlambda = 3)
assess.glmnet(cfit$fit.preval, newy = y, family = "binomial")

###### 8.2.4roc.glmnet()--------------------------------------------------------

fit <- glmnet(x[itrain,], y[itrain], family = "binomial")

rocs <- roc.glmnet(fit, newx = x[-itrain,], newy=y[-itrain])
#rocs是一个列表，其长度就是lambda值的数量，对于每一个lambda取值，
#它都计算了可以用来画ROC曲线的数据
plot(rocs[[3]],type = "l",xlim=c(0,1),ylim=c(0,1))
invisible(sapply(rocs, lines)) # 把所有的ROC都画出来
abline(0,1,col="grey")

###### 8.2.5交叉验证在roc.glmnet()同样适用--------------------------------------

# 建立模型
cfit <- cv.glmnet(x, y, family = "binomial", type.measure = "auc", 
                  keep = TRUE)

# 计算画ROC曲线需要的数据
rocs <- roc.glmnet(cfit$fit.preval, newy = y)
#把AUC最大的ROC曲线画出来，用红色标记，并把其他ROC曲线也画在一起
best <- cfit$index["min",] # 提取AUC最大的lambda值(提取lambda min)
plot(rocs[[best]], type = "l") # 画出AUC最大的ROC曲线
invisible(sapply(rocs, lines, col="grey")) # 把所有的ROC都画出来
lines(rocs[[best]], lwd = 2,col = "red") # 把AUC最大的标红

# 9.正则化Cox回归---------------------------------------------------------------
#glmnet在生存分析中的应用

##### 9.1基础使用---------------------------------------------------------------
#x必须是由自变量组成的matrix
#y可以是一个两列的matrix，两列的列名必须是time和status
#status必须使用0和1组成，0表示删失，1表示发生终点事件
#y还可以是由Surv()函数生成的对象
library(glmnet)
library(survival)

data(CoxExample)
x <- CoxExample$x
y <- CoxExample$y

# 查看y的数据格式
y[1:5, ]
# 建立模型
fit <- glmnet(x, y, family = "cox")
summary(fit)
fit

##### 9.2交叉验证---------------------------------------------------------------
set.seed(1)
cvfit <- cv.glmnet(x, y, family = "cox", type.measure = "C")

print(cvfit)

plot(cvfit)

##### 9.3分层COX----------------------------------------------------------------
# 把1000个观测分5层
strata <- rep(1:5, length.out = 1000)
y2 <- stratifySurv(y, strata) # 对y进行分层
str(y2[1:6])

#把y2提供给glmnet()或者cv.glmnet()就可以实现正则化的分层COX了
fit <- glmnet(x, y2, family = "cox")

cv.fit <- cv.glmnet(x, y2, family = "cox", nfolds = 5)
plot(cv.fit)

##### 9.4生存曲线---------------------------------------------------------------
data(CoxExample)
x <- CoxExample$x
y <- CoxExample$y

y <- Surv(y[,1],y[,2]) # 需要用Surv转换格式

fit <- glmnet(x, y, family = "cox")
survival::survfit(fit, s = 0.05, x = x, y = y)
plot(survival::survfit(fit, s = 0.05, x = x, y = y))
#基于新的数据画生存曲线也是可以的
plot(survival::survfit(fit, s = 0.05, x = x, y = y, newx = x[1:3, ]))


