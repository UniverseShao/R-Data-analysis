#重复测量方差分析

#所谓重复测量方差分析，即受试者被测量不止一次
w1b1<-subset(CO2,Treatment=="chilled")
w1b1
fit<-aov(uptake~conc*Type+Error(Plant/(conc)),w1b1)
#这个Error(Plant/(conc))意思就是考虑了Plant在不同conc测量下的误差
summary(fit)
par(las=2)
par(mar=c(10,4,4,2))
with(w1b1,interaction.plot(conc,Type,uptake,type="b",col=c("red","blue"),pch=c(16,18),
                           main="Interaction Plot for Plant Type and Concentration"))
#第一种分析以及作图方式


boxplot(uptake~Type*conc,data=w1b1,col=c("gold","green"),
        main="Chilled Quebec and Mississippi Plants",
        ylab="Carbon dioxide uptake rate umol/m^2 sec")
#第二种分析以及作图方式


df12_1 <- foreign::read.spss("C:\Users\Administrator\Desktop\lesson1\例12-01.sav")

library(tidyverse)
df12_11 <- 
  df12_1[,1:4] %>% 
  pivot_longer(cols = 2:3,names_to = "time",values_to = "hp") %>% 
  #这行代码使用tidyr包的pivot_longer函数将数据从宽格式转换为长格式
  #cols = 2:3指定了要转换的列，即第2列和第3列
  #names_to = "time"和values_to = "hp"分别指定了新的列名
  #time列将包含原始列的名称，hp列将包含原始列的值
  mutate_if(is.character, as.factor)
df12_11$n <- factor(df12_11$n)


f1 <- aov(hp ~ time * group + Error(n/(time)), data = df12_11)
#Error(n/(time)) 是一个特殊项，用于指定误差结构
#表示在重复测量设计中，每个受试者（由 n 表示）在不同 time 点的测量是相关的



#图形方式画图
with(df12_11,
     interaction.plot(time, group, hp, type = "b", col = c("red","blue"), 
                      pch = c(12,16), main = "两因素两水平重复测量方差分析"))


#箱线图展示结果
boxplot(hp ~ group*time, data = df12_11, col = c("gold","green"),
        main = "两因素两水平重复测量方差分析")


#对于两因素多水平（即两种因变量，每个因变量里面可能有3个或3个以上的种类）


