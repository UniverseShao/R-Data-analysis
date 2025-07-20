#                               方差分析的多重比较
#
#
#多种比较就是在适用单因素方差分析之后具体进一步确定哪些组别之间的均值存在显著差异
#多重比较的适用条件通常包括以下几点：
#1、独立性：各样本是相互独立的随机样本，即满足独立性（independence）
#2、正态性
#3、方差齐性：各样本的总体方差相等
#4、连续变量：观察变量为连续变量，即可以在某个范围内连续取值，如身高体重等
#5、观测值相互独立：不同观测值之间没有联系
#6、观测值可分为多组（≥2）：观测值可以分为两个或多个组别
#7、不存在显著的异常值：观察变量中不存在显著的异常值，以避免对分析结果造成影响
#代码是
TukeyHSD(fit)
#可视化代码是
par(las=2,mar=c(5,8,4,2))
plot(TukeyHSD(fit))#画出来的是森林图
#箱线图的代码是
library(multcomp)
par(mar=c(5,4,6,2))
tuk<-glht(fit,linfct=mcp(trt="Tukey"))
plot(cld(tuk,level=0.05),col="lightgrey")
#注意一旦出现
#Variable(s) ‘trt’ of class ‘character’ is/are not contained as a factor in ‘model’.
#则说明这时候trt没有被识别成因子变量，就是在这一行中
#必须要求fit是因子变量
#而之前进行单因素方差分析和TukeyHSD(fit)多重比较的时候
#则比较智能没有很严格，#转换的方法也很简单
data1$trt <- factor(data1$trt)
fit <- aov(response ~ trt, data = data1)
#注意需要重新搞一次单因素方差分析将这个因子变量植入fit中
library(multcomp)
par(mar=c(5,4,6,2))
tuk<-glht(fit,linfct=mcp(trt="Tukey"))
plot(cld(tuk,level=0.05),col="lightgrey")
再走一边箱线图的代码就ok了





