# 1. 2 x 2 两因素析因设计资料的方差分析-----------------------------------------
# 其实就是双因素方差分析的一种形式
df11_1 <- data.frame(
  x1 = rep(c("外膜缝合","束膜缝合"), each = 10),
  x2 = rep(c("缝合1个月","缝合2个月"), each = 5),
  y = c(10,10,40,50,10,30,30,70,60,30,10,20,30,50,30,50,50,70,60,30)
)
df11_1$x1 <- factor(df11_1$x1)
df11_1$x2 <- factor(df11_1$x2)
str(df11_1)
f1 <- aov(y ~ x1 * x2, data = df11_1)
summary(f1)
"
summary(f1)
            Df Sum Sq Mean Sq F value Pr(>F)  
x1           1    180     180   0.600 0.4499  
x2           1   2420    2420   8.067 0.0118 *
x1:x2        1     20      20   0.067 0.7995  
Residuals   16   4800     300                 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
x1：对因变量 y 无显著影响
x2：控制x1后 x2对 y 有显著影响（需结合均值进一步解释方向，如x2高水平是否比低水平更优）
控制x1 x2 主效应后x1:x2交互作用不显著：说明 x1 和 x2 对 y 的影响是独立的，
无需分析简单效应（Simple Effects）
交互效应指的是一个因子的效应是否依赖于另一个因子的水平
"
##### 1.1双因素方差分析结果可视化-----------------------------------------------
###### 1.1.1第一种画法----------------------------------------------------------
library(tidyverse)
library(ggplot2)
stats <- df11_1 %>%
  group_by(x1, x2) %>%
  summarise(n=n(), mean=mean(y), sd=sd(y),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
stats
pd <- position_dodge(0.2)
ggplot(stats, 
       aes(x = x2, y = mean, 
           group=x1, 
           color=x1, 
           linetype=x1)) +
  geom_point(size = 2, 
             position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = .1, 
                position=pd) +
  theme_bw() + 
  scale_color_manual(values=c("blue", "red")) +
  labs(x="x2",
       y="Mean",
       title="Mean Plot with 95% Confidence Interval") 
###### 1.1.2第二种画法----------------------------------------------------------
interaction.plot(df11_1$x2, df11_1$x1, df11_1$y, type = "b", col = c("red","blue"), pch = c(12,15), xlab = "缝合时间", ylab = "轴突通过率")

###### 1.1.3第三种画法----------------------------------------------------------
library(gplots)
attach(df11_1)
plotmeans(y ~ interaction(x1,x2),
          connect = list(c(1,3), c(2,4)),
          col = c("red","darkgreen"),
          main = "两因素析因设计",
          xlab = "时间和方法的交互")
###### 1.1.4第四种画法----------------------------------------------------------
library(HH)
interaction2wt(y ~ x1 * x2)

# 2.I x J 两因素析因设计资料的方差分析------------------------------------------
df11_2 <- data.frame(
  druga = rep(c("1mg","2.5mg","5mg"), each = 3),
  drugb = rep(c("5微克","15微克","30微克"),each = 9),
  y = c(105,80,65,75,115,80,85,120,125,115,105,80,125,130,90,65,120,100,75,95,85,135,120,150,180,190,160)
)
df11_2$druga <- factor(df11_2$druga)
df11_2$drugb <- factor(df11_2$drugb)
str(df11_2)

f2 <- aov(y ~ druga * drugb, data = df11_2)
summary(f2)
##### 2.1结果可视化-------------------------------------------------------------
###### 2.1.1第一种画法----------------------------------------------------------
library(ggplot2)
stats <- df11_2 %>%
  group_by(druga, drugb) %>%
  summarise(n=n(), mean=mean(y), sd=sd(y),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
stats
pd <- position_dodge(0.2)
ggplot(stats, 
       aes(x = drugb, y = mean, 
           group=druga, 
           color=druga, 
           linetype=druga)) +
  geom_point(size = 2, 
             position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = .1, 
                position=pd) +
  theme_bw() + 
  scale_color_manual(values=c("blue", "red", "brown")) +
  labs(x="drugb",
       y="Mean",
       title="Mean Plot with 95% Confidence Interval") 

###### 2.1.3第二种画法----------------------------------------------------------
interaction.plot(df11_2$drugb, df11_2$druga, df11_2$y, type = "b", col = c("red","blue", "brown"), pch = c(12,15), xlab = "药物剂量", ylab = "镇痛时间")

###### 2.1.4第三种画法----------------------------------------------------------
library(gplots)
attach(df11_2)
plotmeans(y ~ interaction(druga,drugb),
          connect = list(c(1,4,7), c(2,5,8), c(3,6,9)),
          col = c("red","darkgreen","brown"),
          main = "I x J 两因素析因设计",
          xlab = "药物种类和剂量的交互")

###### 2.1.5第四种画法----------------------------------------------------------
library(HH)
interaction2wt(y ~ druga * drugb)

# 3.正交设计资料的方差分析------------------------------------------------------
df11_4 <- data.frame(a = rep(c("5度","25度"),each = 4),
                     b = rep(c(0.5, 5.0), each = 2),
                     c = c(10, 30),
                     d = c(6.0, 8.0,8.0,6.0,8.0,6.0,6.0,8.0),
                     x = c(86,95,91,94,91,96,83,88)
)

df11_4$a <- factor(df11_4$a)
df11_4$b <- factor(df11_4$b)
df11_4$c <- factor(df11_4$c)
df11_4$d <- factor(df11_4$d)
str(df11_4)
f4 <- aov(x ~ a + b + c + d + a*b, data = df11_4)
summary(f4)

# 4.嵌套设计资料的方差分析------------------------------------------------------
df11_6 <- data.frame(
  factor1 = factor(rep(c("A","B","C"),each=6)),
  factor2 = factor(rep(c(70,80,90,55,65,75,90,95,100),each=2)),
  y = c(82,84,91,88,85,83,65,61,62,59,56,60,71,67,75,78,85,89)
)
str(df11_6)
head(df11_6)

# “/”表示factor2嵌套在factor1里
f <- aov(y ~ factor1 / factor2, data = df11_6)

# 等价于以下写法，所以“/”在R中的公式中也是有特殊含义的！
#f <- aov(y ~ factor1 + factor1:factor2, data = df11_6)
summary(f)
##### 4.1嵌套设计资料交互项和双因素交互项的区别---------------------------------
"
(1) factor1 主效应
P值：5.83e-08（远小于0.001）
结论：factor1 的不同水平对 y 有极显著影响（例如，不同工厂之间的产量差异显著）。
(2) factor1:factor2 嵌套效应
P值：0.000716（小于0.001）
结论：在 factor1 的同一水平内，factor2 的不同水平对 y 也有显著影响
（例如，同一工厂内不同机器的产量差异显著）
应用条件和双因素方差分析还是不同的
y ~ factor1 * factor2
中的交互项如果显著，那么含义就是同时控制 factor1 factor2效应后
交互项对y有显著影响，也就是factor1 factor2两者对y的影响不是相互独立的
factor1对y的效应受到factor2的影响
factor2对y的效应也受到factor1的影响
"

# 5.裂区设计资料的方差分析------------------------------------------------------

df11_7 <- data.frame(
  factorA = factor(rep(c("a1","a2"),each=10)),
  factorB = factor(rep(c("b1","b2"),10)),
  id = factor(rep(c(1:10),each=2)),
  y = c(15.75,19.00,15.50,20.75,15.50,18.50,17.00,20.50,16.50,20.00,
        18.25,22.25,18.50,21.50,19.75,23.50,21.50,24.75,20.75,23.75)
)
str(df11_7)

head(df11_7)
# 裂区设计的A因素只作用于一级实验单位，B因素只作用于二级实验单位
f <- aov(y ~ factorA * factorB + Error(id/factorB), data = df11_7)
summary(f)

"
裂区设计的A因素只作用于一级实验单位，
B因素只作用于二级实验单位，所以其方差分析也是由两部分组成（课本P183）。

该例题中每个家兔对应着B因素（毒素浓度）的两个水平
（每只家兔会注射两种浓度的毒素），
但每只家兔只对应A因素的1个水平
（每只家兔只会注射一种药物，不会同时注射两种药物），所以需要为B因素指定误差项

"
# 其实这个本质还是属于是重复测量方差分析差不多的形式
# 其实本质还是重复测量方差分析
# 6.重复测量方差分析的本质------------------------------------------------------
# 重复测量方差分析并不在于重复
# 而是因变量对应两队分类变量自变量的时候

# 其中一个自变量是组间变量，一个个体只能对应一种组间变量
# 另外一个自变量是组内变量，一个个体可以对应多个组内变量
# 上一个例子中个体只能属于一种A，但是可以一个个体有两个B


