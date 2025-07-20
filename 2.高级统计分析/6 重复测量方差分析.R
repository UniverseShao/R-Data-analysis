rm(list = ls())
# 1.重复测量数据两因素两水平的方差分析------------------------------------------
data(CO2)
CO2$conc <- factor(CO2$conc)
CO2$Type <- factor(CO2$Type)
w1b1 <- subset(CO2, Treatment=='chilled')
fit <- aov(uptake ~ (conc*Type) + Error(Plant/(conc)), w1b1)
summary(fit)
##### 1.1获取绘图数据-----------------------------------------------------------
library(dplyr)
plotdata <- CO2 %>%
  group_by(conc, Type) %>%
  summarise(mean_conc = mean(uptake))
plotdata
##### 1.2绘制折线图-------------------------------------------------------------
library(ggplot2)
ggplot(data=plotdata, aes(x=conc, y=mean_conc, group=Type, color=Type,
                          linetype=Type)) +
  geom_point(size=2) +
  geom_line(size=1) +
  theme_bw() + theme(legend.position="top") +
  labs(x="Concentration", y="Mean Uptake", 
       title="Interaction Plot for Plant Type and Concentration")
##### 1.3箱线图-----------------------------------------------------------------
library(ggplot2)
ggplot(data=CO2, aes(x=conc, y=uptake, fill=Type)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position="top") +
  scale_fill_manual(values=c("aliceblue", "deepskyblue"))+
  labs(x="Concentration", y="Uptake", 
       title="Chilled Quebec and Mississippi Plants")

##### 1.4重复测量方差分析结果解读-----------------------------------------------

"Error: Plant
          Df Sum Sq Mean Sq F value  Pr(>F)   
Type       1 2667.2  2667.2   60.41 0.00148 **
Residuals  4  176.6    44.1                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Error: Plant:conc
          Df Sum Sq Mean Sq F value   Pr(>F)    
conc       6 1472.4  245.40   52.52 1.26e-12 ***
conc:Type  6  428.8   71.47   15.30 3.75e-07 ***
Residuals 24  112.1    4.67                     
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

"输出分为两部分，对应不同的变异来源：
Error: Plant → 组间变异（不同植物个体间的差异）
Error: Plant:conc → 组内变异（同一植物不同浓度下的差异）
(1)
组间效应(Error: Plant)
          Df Sum Sq Mean Sq F value  Pr(>F)   
Type       1 2667.2  2667.2   60.41 0.00148 **
控制不同植物个体间的差异后，
不同植物类型（Type）对uptake的影响高度显著，说明Type是重要影响因素
(2)
组内效应（Error: Plant:conc）
conc       6 1472.4  245.40   52.52 1.26e-12 ***
conc:Type  6  428.8   71.47   15.30 3.75e-07 ***
控制同一植物不同浓度下的差异后
1、不同浓度（conc）对uptake的影响极显著，表明浓度变化显著改变吸收率
2、浓度与植物类型存在显著交互作用，意味着：
不同植物类型对浓度变化的响应模式不同
（如Type A可能在低浓度吸收更好，Type B在高浓度更优）
"

# 2.重复测量数据两因素多水平的分析----------------------------------------------
library(haven)
df12_3 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例12-03.sav")
str(df12_3)
View(df12_3)
df12_3$group <- factor(df12_3$group)
df12_3$No <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
library(dplyr)
df12_3 <- df12_3 %>% dplyr::select(No, everything())#将No变成数据框最左侧
df12_3$row_id <- rownames(df12_3)
df12_3$No <- factor(df12_3$No)

##### 2.1转换数据格式成长表格---------------------------------------------------
library(tidyverse)

df12_31 <- df12_3 %>% 
  pivot_longer(cols = 3:7, names_to = "times", values_to = "hp")

df12_31$No <- factor(df12_31$No)
df12_31$times <- factor(df12_31$times)
str(df12_31)
##### 2.2进行方差分析-----------------------------------------------------------
f2 <- aov(hp ~ (times*group) + Error(No/(times)), df12_31)
summary(f2)

##### 2.1获取绘图数据-----------------------------------------------------------
library(dplyr)
plotdata2 <- df12_31 %>%
  group_by(times, group) %>%
  summarise(mean_times = mean(hp))
plotdata2
##### 2.2绘制折线图-------------------------------------------------------------
###### 2.2.1折线图(1)-----------------------------------------------------------
library(ggplot2)
ggplot(data=plotdata2, aes(x=times, y=mean_times, group=group, color=group,
                          linetype=group)) +
  geom_point(size=2) +
  geom_line(size=1) +
  theme_bw() + theme(legend.position="top") +
  labs(x="times", y="hp", 
       title="Interaction Plot for Plant times and group")
###### 2.2.2折线图(2)-----------------------------------------------------------
with(df12_31,
     interaction.plot(times, group, hp, type = "b", 
                      col = c("red","blue","green"), 
                      pch = c(12,16,20), 
                      main = "两因素多水平重复测量方差分析"))
##### 2.3绘制箱线图-------------------------------------------------------------
###### 2.3.1箱线图(1)-----------------------------------------------------------
library(ggplot2)
ggplot(data=df12_31, aes(x=times, y=hp, fill=group)) +
  geom_boxplot() +
  theme_bw() + theme(legend.position="top") +
  scale_fill_manual(values=c("aliceblue", "deepskyblue", "red"))+
  labs(x="times", y="hp", 
       title="两因素多水平重复测量方差分析")
###### 2.3.2箱线图(2)-----------------------------------------------------------
boxplot(hp ~ group*times, data = df12_31, col = c("gold","green","black"),
        main = "两因素多水平重复测量方差分析")

# 3.：重复测量方差分析注意事项--------------------------------------------------
#注意代码的组间因子和组内因子放置的位置
