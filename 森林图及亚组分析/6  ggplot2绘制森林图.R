#没有亚组的森林图####

rm(list = ls())
tmp <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\R_forestplot\\datasets\\ggplot_forest.csv")

library(tidyverse)
str(tmp)
## 'data.frame':    16 obs. of  7 variables:
##  $ Study : chr  "Lotfollahzadeh" "Recio" "Cagir" "Bindea" ...
##  $ Number: int  658 38 23 34 56 32 78 68 23 45 ...
##  $ Pvalue: num  0.002 0.003 0.71 0.03 0.002 0.002 0.87 0.001 0.003 0.02 ...
##  $ OR    : chr  "0.75(0.61-0.93)" "0.71(0.55-0.92)" "0.86(0.58-1.28)" "0.71(0.54-0.94)" ...
##  $ mean  : num  0.75 0.71 0.86 0.71 1.13 0.68 0.81 0.6 1.24 0.71 ...
##  $ lower : num  0.61 0.55 0.58 0.54 1.02 0.48 0.61 0.42 1.07 0.51 ...
##  $ upper : num  0.93 0.92 1.28 0.94 1.29 0.96 1.06 0.87 1.45 0.9 ...

#画图####
tmp <- tmp |> mutate(id = row_number())
p1 <- tmp |> mutate(type = ifelse(Pvalue < 0.05,"#4575b4","grey")
) |> 
  ggplot()+
  geom_errorbarh(aes(y=id,xmin=lower, xmax=upper,color=type),
                 height=0.5, # 控制左右端点两条小竖线的长短
                 size=.8)+
  geom_point(aes(y = id, x = mean,color=type),size=3)+
  scale_color_identity()+
  geom_vline(aes(xintercept=1),linetype=2,size=0.8,color="grey")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()
  )
p1
# |> ####
# |> 管道操作符
# |> 是 R 4.1.0 引入的 原生管道操作符（类似 magrittr 包的 %>%），它的作用是把 左侧的结果传递给右侧的函数，作为其第一个参数

#mutate()####
#新增或修改数据框的列

#row_number()####
#生成行号（1, 2, 3, ...）

#ifelse(condition, true_value, false_value)####
#条件判断，类似 Excel 的 IF

#画变量名字####
p2 <- tmp |> mutate(x="col1") |> 
  ggplot(aes(x=x,y=id))+
  geom_text(aes(label=Study))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )+
  labs(x="Study")+
  scale_x_discrete(position = "top")
p2

#x = "col1"的作用####
#mutate(x = "col1") 新增一列 x，所有值均为 "col1"
#用于后续绘图时固定 x 轴位置

#geom_text() 添加文本标签####
#label = Study：显示 Study 列的文本内容（通常是研究名称或分组标签）

#id的作用####
#y = id：y 轴位置由 id 列决定
#（通常是 row_number() 生成的行号
#确保文本垂直对齐 p1 森林图中的点）

#scale_x_discrete(position = "top")####
#将原本显示在底部的study标题转到顶部

#画Number这个变量####
p3 <- tmp |> mutate(x="col1") |> 
  ggplot(aes(x=x,y=id))+
  geom_text(aes(label=Number))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )+
  labs(x="Number")+
  scale_x_discrete(position = "top")
p3

#画P值####

p4 <- tmp |>  
  mutate(x = "col3") |> 
  ggplot(aes(x=x,y=id))+
  geom_text(aes(label=Pvalue))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )+
  labs(x="Pvalue")+
  scale_x_discrete(position = "top")

#画OR值和可信区间####

p5 <- tmp |>  
  mutate(x = "col4") |> 
  ggplot(aes(x=x,y=id))+
  geom_text(aes(label=OR))+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank()
  )+
  labs(x="OR(95%)")+
  scale_x_discrete(position = "top")

#全都拼到一起####
library(patchwork)
p2+p3+p4+p1+p5+plot_layout(widths = c(0.4,0.2,0.3,1,0.5))

#有亚组的森林图####

df <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\R_forestplot\\datasets\\ggplot2_forest.csv")

# 改个列名
colnames(df)[4:7] <- c("HR","mean","low","high")

df
##            Subgroup   AP   PA              HR mean  low high
## 1      All patients   NR 27.2 0.75(0.61-0.93) 0.75 0.61 0.93
## 2     Baseling ECOG                           0.00 0.00 0.00
## 3                 0   NR 27.2 0.71(0.55-0.92) 0.71 0.55 0.92
## 4                 1   NR 26.4 0.86(0.58-1.28) 0.86 0.58 1.28
## 5   Baseling BPI-SF                           0.00 0.00 0.00
## 6               0~1   NR 27.2 0.71(0.54-0.94) 0.71 0.54 0.94
## 7               2~3 25.5   NR 0.87(0.59-1.29) 0.87 0.59 1.29
## 8   Bone metastases                           0.00 0.00 0.00
## 9               Yes   NR 27.2 0.68(0.48-0.96) 0.68 0.48 0.96
## 10               No   NR 27.5 0.81(0.61-1.06) 0.81 0.61 1.06
## 11              Age                           0.00 0.00 0.00
## 12              <65   NR   NR  0.8(0.51-1.24) 0.80 0.51 1.24
## 13             <=65   NR 26.4  0.73(0.57-0.94 0.73 0.57 0.94
## 14             >=75   NR 23.8    0.71(0.51-1) 0.71 0.51 1.00
## 15     Baseline PSA                           0.00 0.00 0.00
## 16              Yes 26.9 23.8 0.72(0.43-0.94) 0.72 0.43 0.94
## 17               No   NR   NR  0.77(0.38-1.09 0.77 0.38 1.09
## 18     Baseline LDH                           0.00 0.00 0.00
## 19              Yes   NR 23.6 0.69(0.53-0.91) 0.69 0.53 0.91
## 20               No   NR 27.5 0.79(0.55-1.12) 0.79 0.55 1.12
## 21   Baseline ALK-P                           0.00 0.00 0.00
## 22              Yes   NR 23.6  0.79(0.6-1.04) 0.79 0.60 1.04
## 23               No   NR 27.5 0.66(0.46-0.94) 0.66 0.46 0.94
## 24           Region                           0.00 0.00 0.00
## 25    North America   NR 27.2 0.66(0.49-0.88) 0.66 0.49 0.88
## 26            Other   NR   NR 0.89(0.65-1.22) 0.89 0.65 1.22

df <- df |> mutate(id = factor(letters[1:26]))
p1 <- df |> mutate(type = ifelse(high<1 | low >1,"#4575b4","grey")) |> 
  ggplot()+
  geom_errorbarh(aes(y=fct_rev(id),xmin=low, xmax=high,color=type),
                 height=0.5, 
                 size=.8)+
  geom_point(aes(y = fct_rev(id), x = mean,color=type),size=3)+
  scale_color_identity()+
  geom_vline(aes(xintercept=1),linetype=2,size=0.8,color="grey")+
  scale_x_continuous(limits = c(0.3,1.3))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank()
  )
p1

#y=fct_rev(id)的作用####
#森林图（Forest Plot）中，反转 Y 轴顺序 是常见需求
#通常我们希望 第一行显示在图形顶部
#默认情况下，ggplot2 的因子顺序是 从下到上
#使用 fct_rev(id) 可以强制让 id = 1 或 a 显示在顶部，更符合直觉
#上一张图中的study就是从原本的study的顺序从下向上画图


#画亚组####
library(GGally)
align <- ifelse(df$mean==0,"righr","inward")
p2 <- df |> mutate(x="col1") |> 
  ggplot(aes(x=x,y=fct_rev(id)))+
  geom_text(aes(label=Subgroup),hjust=align,size=3)+
  geom_stripped_rows()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(rep(0,4),"cm")
  )+
  labs(x="Subgroup")+
  scale_x_discrete(position = "top")
p2

#geom_stripped_rows() 的作用####
#为图表添加 交替行条纹背景
#align <- ifelse(df$mean==0,"righr","inward")作用####
#我们可以发现这里之所以将"right"写成"righr"是为了让所有的行都居中对齐
#但是这样的代码太丑了

p2 <- df |> 
  mutate(x = "col1") |> 
  ggplot(aes(x = x, y = fct_rev(id))) +
  geom_text(aes(label = Subgroup), hjust = 0.5, size = 3) +  # 0.5 表示居中对齐
  geom_stripped_rows() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(rep(0, 4), "cm")
  ) +
  labs(x = "Subgroup") +
  scale_x_discrete(position = "top")

p2
#  hjust =0.5 表示居中对齐


#画HR和可信区间####
p3 <- df |> mutate(x="col1") |> 
  ggplot(aes(x=x,y=fct_rev(id)))+
  geom_text(aes(label= HR),size=3)+
  geom_stripped_rows()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(rep(0,4),"cm")
  )+
  labs(x="HR(95%CI)")+
  scale_x_discrete(position = "top")
p3

#画AP这个变量####
p4 <- df |> mutate(x="col1") |> 
  ggplot(aes(x=x,y=fct_rev(id)))+
  geom_text(aes(label= AP),size=3)+
  geom_stripped_rows()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(rep(0,4),"cm")
  )+
  labs(x="AP")+
  scale_x_discrete(position = "top")
p4

#画PA这个变量####

p5 <- df |> mutate(x="col1") |> 
  ggplot(aes(x=x,y=fct_rev(id)))+
  geom_text(aes(label= PA),size=3)+
  geom_stripped_rows()+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(rep(0,4),"cm")
  )+
  labs(x="PA")+
  scale_x_discrete(position = "top")
p5

#拼到一起####
library(patchwork)

p2+p4+p5+p1+p3+plot_layout(widths = c(0.1,0.05,0.05,0.1,0.1))



















