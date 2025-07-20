# 7.1 准备数据------------------------------------------------------------------dd
rm(list = ls())
lowbirth <- read.csv("C:\\Users\\Administrator\\Desktop\\R脚本(SWY精心编辑版)\\临床预测模型\\datasets\\lowbirth.csv")
dim(lowbirth)
str(lowbirth) 
table(lowbirth$race)#使用table()函数看各个类别的数量
#分类变量（包括因变量）因子化
#把oriental和native American这两个类别合并成一个类别，就叫other
library(dplyr)

tmp <- lowbirth %>% 
  mutate(across(where(is.character),as.factor),
         vent = factor(vent),
         dead = factor(dead),
         race = case_when(race %in% 
                            c("native American","oriental") ~ "other",
                          .default = race),
         race = factor(race))
str(tmp)
#上述代码写的很简练可以稍微记住学习一下

# 7.2 方法1：rms-----------------------------------------------------------------
library(rms)
#首先是打包数据，这一步对于rms包来说是必须的：
dd <- datadist(tmp)
options(datadist="dd")
#构建逻辑回归模型，我们只使用其中的部分变量进行演示
fit1 <- lrm(dead==1 ~ birth + lowph + pltct + bwt + vent + race,
            data = tmp)
summary(fit1)
fit1


#summary(fit1)和fit1输出结果的差异------------------------------------------------------------------
#summary(fit1)中的系数是 Diff.  Effect 其反映的是特定对比区间（Low vs High） 的效应量
#的系数变化，其OR值也是整体的Low vs High的OR值，这和大多数论文中的OR有差异
#fit1输出的结果的 Coef 是原始回归系数，其指数计算得到的OR才是我们常说的OR
#即自变量每增加1单位对 log-odds 的影响，直接指数化后得到的 OR 是 单位变化的影响


#lrm()做逻辑回归应该是默认计算排序靠后的类别的概率。
#构建列线图
nom1 <- nomogram(fit1, fun=plogis,
                 fun.at=c(0.001,0.1,0.25,0.5,0.75,0.9,0.99),
                 lp=T, # 是否显示线性预测值
                 maxscale = 100, # 最大得分数
                 conf.int = F, # 添加置信区间，很难看，可以不要
                 funlabel="Risk of Death")  
plot(nom1) 
plot(nom1,
     col.grid=c("tomato","grey")
     #conf.space = c(0.3,0.5) # 置信区间位置
) 

# 7.3 方法2：regplot--------------------------------------------------------------------------------------------------------
library(regplot)

# 建立模型，这里使用glm也可以
fit2 <- lrm(dead==1 ~ birth + lowph + pltct + bwt + vent + race,
            data = tmp)

# 绘图
aa <- regplot(fit2,
              #连续性变量形状，"no plot""density""boxes""ecdf"
              #"bars""boxplot""violin""bean" "spikes"；
              #分类变量的形状，可选"no plot" "boxes" "bars" "spikes"
              plots = c("violin", "boxes"),   
              observation = tmp[1,], #用哪行观测，或者T F
              center = T, # 对齐变量
              subticks = T,
              droplines = T,#是否画竖线
              title = "nomogram",
              points = T, # 截距项显示为0-100
              odds = T, # 是否显示OR值
              showP = T, # 是否显示变量的显著性标记
              rank = "sd", # 根据sd给变量排序
              interval="confidence", # 展示可信区间
              clickable = F # 是否可以交互
)










































