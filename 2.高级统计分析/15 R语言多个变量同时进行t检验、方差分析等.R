df <- read.csv("../000files/20210801.csv",header = T)

str(df)

# 1.数据格式要求是长数据，或者说整洁数据----------------------------------------
suppressMessages(library(tidyverse))

df_l <- df %>% 
  pivot_longer(cols = 2:5, names_to = "变量", values_to = "积分") %>% 
  dplyr::mutate_if(is.character, as.factor)

str(df_l)


# 2.正态性检验------------------------------------------------------------------
library(rstatix)
df_l %>% group_by(变量,组别) %>% shapiro_test(积分)

# 3.方差齐性检验----------------------------------------------------------------
df_l %>% group_by(变量) %>% levene_test(积分 ~ 组别)
# 4.t检验-----------------------------------------------------------------------
df_l %>% group_by(变量) %>% t_test(积分 ~ 组别)