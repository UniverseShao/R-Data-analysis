rm(list = ls())
suppressMessages(library(tidyverse))
load(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\临床预测模型\\datasets\\df1.rdata")
psych::headTail(df1)
# 四格表------------------------------------------------------------------------
xtabs(~outcome+pred,data = df1)
# 混淆矩阵形式------------------------------------------------------------------
df1 %>% count(pred, outcome)#计算每个组合的频数

# 20.1 2d混淆矩阵可视化---------------------------------------------------------
ggplot(df1, aes(pred, outcome))+
  geom_tile(aes(fill=ca125))+
  geom_label(data = df1 %>% count(pred, outcome), aes(pred,outcome,label=n),size=12)+
  theme_minimal()+
  theme(legend.position = "none")
