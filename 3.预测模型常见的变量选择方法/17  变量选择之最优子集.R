# 前言--------------------------------------------------------------------------
#最优子集法，又叫全子集回归法，全局择优法
#目前看来最优子集筛选变量只适用于线性回归

# 17.1 准备数据-----------------------------------------------------------------
rm(list = ls())
df <- data.frame(
  cho = c(5.68,3.79,6.02,4.85,4.60,6.05,4.90,7.08,3.85,4.65,4.59,4.29,7.97,
          6.19,6.13,5.71,6.40,6.06,5.09,6.13,5.78,5.43,6.50,7.98,11.54,5.84,
          3.84),
  tg = c(1.90,1.64,3.56,1.07,2.32,0.64,8.50,3.00,2.11,0.63,1.97,1.97,1.93,
         1.18,2.06,1.78,2.40,3.67,1.03,1.71,3.36,1.13,6.21,7.92,10.89,0.92,
         1.20),
  ri = c(4.53, 7.32,6.95,5.88,4.05,1.42,12.60,6.75,16.28,6.59,3.61,6.61,7.57,
         1.42,10.35,8.53,4.53,12.79,2.53,5.28,2.96,4.31,3.47,3.37,1.20,8.61,
         6.45),
  hba = c(8.2,6.9,10.8,8.3,7.5,13.6,8.5,11.5,7.9,7.1,8.7,7.8,9.9,6.9,10.5,8.0,
          10.3,7.1,8.9,9.9,8.0,11.3,12.3,9.8,10.5,6.4,9.6),
  fpg = c(11.2,8.8,12.3,11.6,13.4,18.3,11.1,12.1,9.6,8.4,9.3,10.6,8.4,9.6,10.9,
          10.1,14.8,9.1,10.8,10.2,13.6,14.9,16.0,13.2,20.0,13.3,10.4)
)
# 17.2 建立模型-----------------------------------------------------------------
#使用全部的4个变量建立回归方程
f <- lm(fpg ~ cho + tg + ri + hba, data = df)

summary(f)
# 17.3 最优子集法---------------------------------------------------------------
library(leaps)
leaps <- regsubsets(fpg ~ cho + tg + ri + hba, data = df)
summary(leaps)
##### 17.3.1 定义subsTable()函数查看全子集结果----------------------------------
subsTable <- function(obj, scale){
  x <- summary(leaps)
  m <- cbind(round(x[[scale]],3), x$which[,-1])
  colnames(m)[1] <- scale
  m[order(m[,1]), ]
}

subsTable(leaps, scale="adjr2")
#regsubsets() 函数默认不会显示 所有可能的变量组合，
#而是返回每个变量数量（k）下的 最优模型
##### 17.3.2神包broom显示结果---------------------------------------------------
broom::tidy(leaps)
##### 17.3.3表格化全子集结果----------------------------------------------------------
library(gt)
library(tidyverse)
library(scales)

broom::tidy(leaps) %>%
  select(-`(Intercept)`) %>%
  rownames_to_column(var = "n_vars") %>%
  gt(rowname_col = "n_vars") %>%
  gt::data_color(
    columns = cho:hba,
    fn = col_numeric(
      palette = c("#fdae61", "#abdda4"),
      domain = c(0, 1)) 
  ) %>%
  gt::fmt_number(r.squared:mallows_cp, n_sigfig = 4)

##### 17.3.4图像化全子集结果----------------------------------------------------
broom::tidy(leaps) %>%
  select(r.squared:mallows_cp) %>%
  mutate(n_vars = 1:n()) %>%
  pivot_longer(cols = -n_vars, names_to = "metric") %>%
  ggplot(aes(x = n_vars, y = value)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = . %>%
      group_by(metric) %>%
      filter(value == ifelse(str_detect(metric, "r.squared"),
                             max(value), min(value))),
    aes(xintercept = n_vars), lty = 2) +
  theme_bw()+
  facet_wrap(~ metric, scales = "free_y")


