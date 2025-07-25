#双因素ANOVA——y~A*B
#单因素ANOVA——y~A
#含单个协变量的单因素ANCOVA——y~x+A（A是主变量，x是协变量）

#双因素方差分析研究两个分类自变量（因素）对一个连续因变量的独立影响
#以及它们之间是否存在交互作用
#第六行就是双因素方差分析和单因素协方差分析的区别
#单因素协方差分析不会分析两个变量之间是否存在交互作用且会分析两个变量的独立因素效效应
#双因素方差分析的意义
#1、独立因素的效应：评估两个分类自变量（因素）对因变量的独立影响
#2、交互作用：评估两个因素之间的交互作用，即一个因素的效应是否依赖于另一个因素的水平
#3、实验设计：适用于完全随机设计、随机区组设计等实验设计

#如果您关注的是两个分类变量及其交互作用对因变量的影响，选择双因素方差分析
#如果您有一个主要的分类变量，并且想控制一个或多个连续变量的影响，选择单因素协方差分析
#举例，体重是因变量，有两个自变量一个是用药时间，一个是用药剂量，这用单因素协方差分
#举例，预后是因变量，有两个自变量一个是缝合方式，一个是缝合后时间，是用双因素分析
#即我们想主要研究一个因素的时候，这时候控制住别的因素，来研究主因素就是单因素协方差分析
#即我们不仅想要研究一个因素，我们还想要研究另外一个，并还想研究他们连个相互作用对结果的
#影响，这时候选择双因素方差分析
#在16行的例子中，如果从药动学的角度入手还想研究用药时间×剂量对结果的影响就可以使用双因
#素方差分析
#
#双因素方差分析公式为
f1 <- aov(y ~ x1 * x2, data = df11_1)
summary(f1)
#
#第一种画图方式
interaction.plot(df11_1$x2, df11_1$x1, df11_1$y, type = "b", col = c("red","blue"), pch = c(12,15), xlab = "缝合时间", ylab = "轴突通过率")

#第二种画图方式
#library(gplots)
plotmeans(y ~ interaction(x1,x2),
          connect = list(c(1,3), c(2,4)),
          col = c("red","darkgreen"),
          main = "两因素析因设计",
          xlab = "时间和方法的交互")

#第三种画图方式
interaction2wt(y ~ x1 * x2)
#

#I x J x K 三因素析因设计资料的方差分析
f3 <- aov(x ~ b * c * a, data = df11_3)
#正交设计资料的方差分析
f4 <- aov(x ~ a + b + c + d + a*b, data = df11_4)

