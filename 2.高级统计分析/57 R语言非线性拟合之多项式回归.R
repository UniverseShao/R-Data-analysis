#多项式拟合---------------------------------------------------------------------
rm(list = ls() )
# 加载数据
library(car)

data("USPop")
psych::headTail(USPop)
#画图看看两列数据的情况
plot(population ~ year, data = USPop)

# 拟合线性回归------------------------------------------------------------------------
f <- lm(population ~ year, data = USPop)

# 画出原来的数据
plot(population ~ year, data = USPop)

# 添加拟合线
lines(USPop$year, fitted(f), col = "blue")
#x 是USPop$year y是fitted(f)也就是y = fitted(f)
lines(USPop$year, predict(f), col = "blue")
#这两个其实是等价的
#这里的图其实可以用ggplot画，详情见R实战173页

#拟合一个二次项的多项式回归------------------------------------------------------------------------
# 2次项，注意用法
f1 <- lm(population ~ year + I(year^2), data = USPop)
# 画出拟合线
plot(population ~ year, data = USPop)
lines(USPop$year, fitted(f1))

# 3次项--------------------------------------------------------------------------------------------
# 3次项，注意用法
f2 <- lm(population ~ year + I(year^2) + I(year^3), data = USPop)

# 画出拟合线
plot(population ~ year, data = USPop)
lines(USPop$year, fitted(f2))

#增加了一个3次项，结果并没有好很多。所以我们可以就选2次项即可


#模型比较----------------------------------------------------------------------------
#anova()比较嵌套模型----------------------------------------------------------------
# 线性回归和2次项比较
anova(f, f1)
#检查嵌套模型，若P<0.05则说明多出来的项是对模型有显著贡献的
anova(f1, f2)
#这时P>0.05说明多出来的I(year^3)是对模型没有显著贡献的

#再次举例---------------------------------------------------------------------------------------------
#再次举例（构造数据）--------------------------------------------------------------------------------
x <- 1:100         
k <- c(25, 50, 75) 
u <- function(x)ifelse(x > 0, x, 0)
x2 <- u(x - k[1])
x3 <- u(x - k[2])
x4 <- u(x - k[3])
set.seed(1)
y <- 0.8 + 1*x + -1.2*x2 + 1.4*x3 + -1.6*x4 + rnorm(100,sd = 2.2)
plot(x, y)

#例2.拟合6次项----------------------------------------------------------------------------
f.6 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
#这是在数学上在25 50 75这几个点引入斜率变化点，构造分段线性关系
#rnorm(100, sd=2.2)添加正态分布的随机噪声（标准差为2.2），使数据更接近真实场景（无完美拟合）
# 画出拟合线
plot(x,y)
lines(x, fitted(f.6))
#拟合线的开头和末尾可以发现有点上翘的趋势
#解决方法也很简单，就是我们下次要介绍的样条回归

# 多项式拟合的简便写法，拟合6次项，和上面结果完全一样
f.6 <- lm(y ~ poly(x, 6))
# 画出拟合线
plot(x,y)
lines(x, fitted(f.6))
#拟合线的ggplot画法（R实战173页老笔记）--------------------------------------------------------------------------------------------
#没有数据框的情况下ggplot()中不需要写多余的东西
library(ggplot2)

ggplot()+
  geom_point(aes(x,y),size=2)+
  geom_line(aes(x, fitted(f.6)), color="red",size=2)+
  theme_bw()
#先构造数据框
df.tmp <- data.frame(x = x, y= y)

ggplot(df.tmp, aes(x,y))+
  geom_point(aes(x,y),size=2)+
  geom_line(aes(x, fitted(f.6)), color="red",size=2)+
  theme_bw()

#或者是利用ggplot自带的geom_smooth拟合线性回归线
#相比geom_line的优势是可以添加置信区间

ggplot(df.tmp, aes(x,y))+
  geom_point(size=2)+
  geom_smooth(method = "lm",
              formula = y ~ poly(x,6),
              color="red",
              size=2,
              se = T, # 可信区间
  )+
  theme_bw()

#多项式能用于逻辑回归吗？Cox回归呢？----------------------------------------------------------------
#当然可以，只是把自变量变成多次项而已，和lm用法一模一样，函数使用glm()/coxph()等即可



