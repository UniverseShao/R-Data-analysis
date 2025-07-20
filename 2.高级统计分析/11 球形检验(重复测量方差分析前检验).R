# 1.球形检验--------------------------------------------------------------------
# 球形检验是非常重要的内容，在进行重复测量方差分析之前，都应该先进行球形假设检验
rm(list = ls())
library(haven)
df1 <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例12-03.sav")
str(df1)
df1$group <- factor(df1$group)
df1$No <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
library(dplyr)
df1 <- df1 %>% select(No, everything())#将No变成数据框最左侧
df1$No <- factor(df1$No)
View(df1)
str(df1)

# 1.1将数据变为矩阵，转换数据格式-----------------------------------------------
df2 <- as.matrix(cbind(df1[1:5,3:7], df1[6:10,3:7], df1[11:15,3:7]))

# 1.2测量点和分组单独建立，注意要和上面的顺序一致-------------------------------
times = ordered(rep(1:5,3))
group = factor(rep(c("A","B","C"),each = 5))
# 1.3进行球对称检验（球形检验）-------------------------------------------------
mauchly.test(lm(df2 ~ 1), M = ~ group + times, X = ~ times)
# 1.4检验结果的解释-------------------------------------------------------------
"data:  SSD matrix from lm(formula = df2 ~ 1)
W = 0.427, p-value = 0.279
原假设（H₀）：数据满足球形假设（即不同时间点间差异的方差齐性）。
p > 0.05 → 不拒绝H₀，认为数据满足球形假设。"


