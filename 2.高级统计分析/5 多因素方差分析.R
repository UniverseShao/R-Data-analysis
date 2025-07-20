# 1. 2 x 2 两因素析因设计资料的方差分析-----------------------------------------
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
##### 1.1双因素方差分析结果可视化-----------------------------------------------
###### 1.1.1第一种画法----------------------------------------------------------
library(ggplot2)
library(tidyverse)
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


