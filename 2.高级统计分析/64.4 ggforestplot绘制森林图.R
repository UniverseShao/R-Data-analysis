#ggplot2####
#编造数据####

library(tibble)
options(digits = 2)
df <- tibble(
  label = LETTERS[1:22],
  mean = rnorm(22,mean = 1, sd=0.2),
  lower = mean - 0.1,
  upper = mean + 0.2,
  group = c(rep("Group-1",7),rep("Group-2",7),rep("Group-3",8))
)
df
## # A tibble: 22 × 5
##    label  mean lower upper group  
##    <chr> <dbl> <dbl> <dbl> <chr>  
##  1 A     0.889 0.789 1.09  Group-1
##  2 B     1.18  1.08  1.38  Group-1
##  3 C     1.32  1.22  1.52  Group-1
##  4 D     0.967 0.867 1.17  Group-1
##  5 E     0.943 0.843 1.14  Group-1
##  6 F     0.791 0.691 0.991 Group-1
##  7 G     1.12  1.02  1.32  Group-1
##  8 H     1.20  1.10  1.40  Group-2
##  9 I     0.715 0.615 0.915 Group-2
## 10 J     0.767 0.667 0.967 Group-2
## # ℹ 12 more rows


#tibble()函数用于创建数据集

library(ggplot2)

p <- ggplot(data = df)+
  geom_point(aes(x=mean,y=label),size = 2)+
  geom_errorbar(aes(x = mean,y=label,xmin=lower,xmax=upper))+
  geom_vline(xintercept = 1, color = "black",linetype="dashed",alpha=0.6)+
  labs(x=NULL,y=NULL)+
  facet_grid(group ~.,scales = "free",space = "free")+
  theme_minimal()+
  theme(text=element_text(size=18, color="black"))+
  theme(panel.spacing = unit(1, "lines"))
p


#ggforestplot####

# 目前只能通过github安装
devtools::install_github("NightingaleHealth/ggforestplot")

library(ggforestplot)
library(tidyverse)

# 筛选部分数据
df <-
  ggforestplot::df_linear_associations %>%
  filter(
    trait == "BMI",
    dplyr::row_number() <= 30
  )

df
## # A tibble: 30 × 5
##    name          trait    beta      se    pvalue
##    <chr>         <chr>   <dbl>   <dbl>     <dbl>
##  1 Isoleucine    BMI    0.339  0.00945 1.11e-281
##  2 Leucine       BMI    0.343  0.00951 1.25e-285
##  3 Valine        BMI    0.287  0.00951 7.94e-200
##  4 Phenylalanine BMI    0.343  0.00862 0        
##  5 Tyrosine      BMI    0.261  0.00900 6.65e-185
##  6 Alanine       BMI    0.179  0.00890 8.62e- 90
##  7 Glutamine     BMI   -0.134  0.00945 7.68e- 46
##  8 Glycine       BMI   -0.0296 0.00937 1.56e-  3
##  9 Histidine     BMI    0.0364 0.00917 7.25e-  5
## 10 Lactate       BMI    0.131  0.00911 9.20e- 47
## # ℹ 20 more rows

ggforestplot::forestplot(
  df = df,
  name = name,
  estimate = beta,
  se = se,
  pvalue = pvalue,
  psignif = 0.002, # 显著性阈值
  xlab = "1-SD increment in BMI\nper 1-SD increment in biomarker concentration",
  title = "Associations of blood biomarkers to BMI"
)

#多组####
# 数据准备
selected_bmrs <- df %>% pull(name)

df_compare_traits <-
  ggforestplot::df_linear_associations %>%
  filter(name %in% selected_bmrs) %>%
  # Set class to factor to set order of display.
  mutate(
    trait = factor(
      trait,
      levels = c("BMI", "HOMA-IR", "Fasting glucose")
    )
  )

#画图####

# 画图
ggforestplot::forestplot(
  df = df_compare_traits,
  estimate = beta,
  pvalue = pvalue,
  psignif = 0.002,
  xlab = "1-SD increment in cardiometabolic trait\nper 1-SD increment in biomarker concentration",
  title = "Biomarker associations to metabolic traits",
  colour = trait
)


#filter() 函数####
#根据条件筛选行，保留满足条件的行

#pull() 函数####
#提取数据框中的某一列，返回一个向量（而非数据框）
#pull(.data, 列名或列位置)


#mutate() 函数####
#添加新列或修改现有列，基于现有列的值进行计算或转换
#不改变原始数据框，而是返回一个新的数据框


#为什么必须用 mutate()
#(1) 管道操作符 %>% 的工作机制
#**%>% 的作用**：将左侧的数据（或表达式结果）传递给右侧函数的第一个参数
#右侧必须是一个函数：管道右侧需要是一个函数调用（如 filter()、mutate()），而不是直接的列赋值操作
#直接写 trait = factor(...) 会报错
#mutate() 的本质
#转换或者重新编码行

