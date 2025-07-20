# 1.线性的立方样条--------------------------------------------------------------
#数据还是用上一篇的数据
##### 1.1构造数据---------------------------------------------------------------
rm(list = ls())
x <- 1:100         
k <- c(25, 50, 75) 
u <- function(x)ifelse(x > 0, x, 0)
x2 <- u(x - k[1])
x3 <- u(x - k[2])
x4 <- u(x - k[3])
set.seed(1)
y <- 0.8 + 1*x + -1.2*x2 + 1.4*x3 + -1.6*x4 + rnorm(100,sd = 2.2)
plot(x, y)

##### 1.2线性直线回归拟合效果---------------------------------------------------
f <- lm(y ~ x)
plot(x, y)
lines(x, fitted(f),col="red")

##### 1.3rms做限制性立方样条回归------------------------------------------------

# 加载R包
library(rms)
#rms包也是绘制残线图常用的包，rms使用之前对数据进行打包
# 拟合限制性立方样条，这里对变量x使用，跟多项式回归差不多
f <- lm(y ~ rcs(x,5))#限制5个节点，包括两边，中间就正好是3个拐点

# 画出原数据
plot(x,y)
lines(x, fitted(f),col="red") # 画出拟合线

#ggplot2画图

df.tmp <- data.frame(x=x,y=y)

ggplot(df.tmp, aes(x,y))+
  geom_point(size=2)+
  geom_smooth(method = "lm",
              formula = y ~ rcs(x,5),
              se = T,
              color="red"
  )+
  theme_bw()


# 2.逻辑回归的立方样条----------------------------------------------------------
# 加载数据
load(file = "C:\\Users\\Administrator\\Desktop\\R脚本(SWY精心编辑版)\\临床预测模型\\datasets\\titanic3.rdata")
# 使用rms前先把数据打包
dd <- datadist(titanic3); options(datadist='dd')

# 逻辑回归的立方样条
f <- lrm(survived ~ rcs(sqrt(age),5) + sex, data=titanic3)
f
##### 2.1如何判断逻辑回归模型是否可以适用样条回归-------------------------------
#anova()函数查看各自变量的统计显著性
###### 2.1.1anova(f)------------------------------------------------------------
anova(f)
##                 Wald Statistics          Response: survived 
## 
##  Factor     Chi-Square d.f. P     
##  age         14.97     4    0.0048
##   Nonlinear  12.65     3    0.0055
##  sex        259.17     1    <.0001
##  TOTAL      265.88     5    <.0001

#age的Nonlinear的P<0.05，可以认为是符合非线性的
#说明对于age连续变量的Nonlinear项是对模型有显著贡献的
#所以说明这个逻辑回归的样条回归是有意义的



###### 2.1.2可视化查看模型是否可以适用样条回归----------------------------------

library(ggplot2)
ggplot(Predict(f, age, sex, fun = exp))+ # 加上 fun = plogis 则返回概率
  geom_hline(yintercept = 1, color="grey20",linetype=2)+ # OR=1的横线
  theme_bw() 
#这图太狂放了
#下图使用log OR会好一点
ggplot(Predict(f, age, sex))+ # 加上 fun = plogis 则返回概率
  geom_hline(yintercept = 0, color="grey20",linetype=2)+ # OR=1的横线
  theme_bw() 

# 3.cox回归的立方样条-----------------------------------------------------------
rm(list = ls())
##### 3.1构造数据，包括性别(sex),年龄(age),生存时间(time),生存结局(death)-------
n <- 1000
set.seed(731)
age <- 50 + 12*rnorm(n)
label(age) <- "Age"
sex <- factor(sample(c('Male','Female'), n, rep=TRUE, prob=c(.6, .4)))
cens <- 15*runif(n)
h <- .02*exp(.04*(age-50)+.8*(sex=='Female'))
time<- -log(runif(n))/h
label(time) <- 'Follow-up Time'
death<- ifelse(time <= cens,1,0)
time <- pmin(time, cens)
units(time) <- "Year"
data<-data.frame(age,sex,time,death)
psych::headTail(data)

##### 3.2限制性立方样条进行Cox回归----------------------------------------------

# 打包数据
dd <- datadist(data);options(datadist='dd') 

# 拟合cox模型
fit<- cph(Surv(time,death) ~ rcs(age,4) + sex,data = data) 
fit
anova(fit)
"若非线性p值显著（如<0.05），保留4节点。
若p值不显著，尝试减少节点数（如3节点）。"
##### 3.3画图-------------------------------------------------------------------
###### 3.3.1绘制HR~连续变量图 的注意事项----------------------------------------

"以下的代码都是在适用rms包的前提下可以适用
也就是使用cph()拟合模型而不是coxph()拟合模型，且数据提前需要打包的基础上
不能直接生搬硬套"


ggplot(Predict(fit, age,fun = exp, ref.zero = T))+ # fun = exp，计算HR值，不加这句计算概率
  geom_hline(yintercept = 1, color="grey20",linetype=2)+ # HR=1的横线
  theme_bw()
#添加sex分组
ggplot(Predict(fit, age, sex, fun = exp, ref.zero = T))+ # fun = exp，计算HR值，不加这句计算概率
  geom_hline(yintercept = 1, color="grey20",linetype=2)+ # HR=1的横线
  theme_bw()

##### 3.4通过以下函数获取HR=1时的年龄是几岁-------------------------------------
Predict(fit, age,fun = exp, ref.zero = T)
#只有在ref.zero = T的时候才显示真正的HR，如果是F则显示和截距对比的HR adjusted
#对于模型拟合predict()和fitted()的区别
#fitted()输出的是模型中每个个体的结果，都是输出的log OR或者lor HR

#predict()输出的是指定协变量组合的预测风险差异，fun=exp：输出风险比（HR）而非log HR
#排序方式也不是按照模型中的个体，而是按照协变量组合的线性顺序

##### 3.5根据HR为1更新模型（为什么更新模型）------------------------------------

dd$limits$age[2] <- 48 # 重新选择HR为1的点，第二个四分位数就是中位数，中位数对应HR为1
fit <- update(fit) # 更新模型
#我们只是在原本的模型的predict中推断48岁是HR为1的点
#原本模型中48岁对应的HR可能是1.2或者1.3这个在1上下的点
#更新模型后就对使得模型中的48岁对应的HR就是1


##### 3.6重新画图---------------------------------------------------------------
ggplot(Predict(fit, age,fun = exp, ref.zero = T))+ 
  geom_hline(yintercept = 1, color="steelblue",linetype=2,size=1.2)+ # HR=1的横线
  geom_vline(xintercept = 48, color="red",linetype=2,size=1.2)+
  theme_classic()

ggplot(Predict(fit, age, sex, fun = exp, ref.zero = T))+ 
  geom_hline(yintercept = 1, color="steelblue",linetype=2,size=1.2)+ # HR=1的横线
  geom_vline(xintercept = 48, color="red",linetype=2,size=1.2)+
  theme_classic()

# 4.如何判断cox模型是否可以适用样条回归-----------------------------------------

##### (1) 方法一：anova()检验非线性项-------------------------------------------
anova(fit)

#Wald Statistics          Response: Surv(time, status) 

#Factor     Chi-Square d.f. P     
#age        28.75      4    <.0001
#Nonlinear  7.92      3    0.048  # <- 关注这一行
#sex        12.34      1    0.0004

#Nonlinear对应的P值（此处0.048 < 0.05）
#拒绝线性假设，支持使用样条

##### (2) 方法二：似然比检验（比较线性与样条模型）------------------------------

fit_linear <- cph(Surv(time, death) ~ age + sex, data = data)
fit_spline <- cph(Surv(time, death) ~ rcs(age, 4) + sex, data = data)
lrtest(fit_linear, fit_spline)  # 若P<0.05，选择样条模型

##### (3) 方法三：图形诊断------------------------------------------------------
# 绘制HR随age的变化
ggplot(Predict(fit_spline, age, fun = exp, ref.zero = TRUE)) +
  geom_hline(yintercept = 1, linetype = 2)
#若曲线明显偏离水平线（HR=1），提示非线性

##### (4)检验比例风险假设-------------------------------------------------------
library(survival)
cox.zph(fit_linear)
#结果要P＞0.05才可以说明满足等比例风险假设检验
#若违反，尝试样条
fit_spline <- cph(Surv(time, death) ~ rcs(age, 4) + sex, data = data)
cox.zph(fit_spline)
#P＞0.05说明cox模型的样条回归符合等比例风险假设检验
#采用样条回归
"常见误区澄清
误区：“PH检验通过 → 必须用线性模型”。
正解：PH检验仅说明效应不随时间变化，但效应本身可能是非线性的（样条模型更优）。
误区：“样条模型一定破坏PH假设”。
正解：样条可以建模固定的非线性效应，同时保持比例风险（如您的案例）。"

# 5.样条Cox回归和时依变量Cox回归的最大区别是------------------------------------
"样条Cox回归专攻连续变量的非线性效应，与PH假设无关。
时依变量Cox回归专攻任何变量的PH假设违反，与变量类型无关。"

