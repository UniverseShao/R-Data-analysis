#准备数据####
rm(list = ls())
library(survival)

str(colon)
## 'data.frame':    1858 obs. of  16 variables:
##  $ id      : num  1 1 2 2 3 3 4 4 5 5 ...
##  $ study   : num  1 1 1 1 1 1 1 1 1 1 ...
##  $ rx      : Factor w/ 3 levels "Obs","Lev","Lev+5FU": 3 3 3 3 1 1 3 3 1 1 ...
##  $ sex     : num  1 1 1 1 0 0 0 0 1 1 ...
##  $ age     : num  43 43 63 63 71 71 66 66 69 69 ...
##  $ obstruct: num  0 0 0 0 0 0 1 1 0 0 ...
##  $ perfor  : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ adhere  : num  0 0 0 0 1 1 0 0 0 0 ...
##  $ nodes   : num  5 5 1 1 7 7 6 6 22 22 ...
##  $ status  : num  1 1 0 0 1 1 1 1 1 1 ...
##  $ differ  : num  2 2 2 2 2 2 2 2 2 2 ...
##  $ extent  : num  3 3 3 3 2 2 3 3 3 3 ...
##  $ surg    : num  0 0 0 0 0 0 1 1 1 1 ...
##  $ node4   : num  1 1 0 0 1 1 1 1 1 1 ...
##  $ time    : num  1521 968 3087 3087 963 ...
##  $ etype   : num  2 1 2 1 2 1 2 1 2 1 ...

suppressMessages(library(tidyverse))
#执行 library(tidyverse) 时，控制台不会显示任何附加信息
df <- colon %>% 
  mutate(rx=as.numeric(rx)) %>% 
  filter(etype == 1, !rx == 2) %>%  #rx %in% c("Obs","Lev+5FU"), 
  select(time, status,rx, sex, age,obstruct,perfor,adhere,differ,extent,surg,node4) %>% 
  mutate(sex=factor(sex, levels=c(0,1),labels=c("female","male")),
         age=ifelse(age >65,">65","<=65"),
         age=factor(age, levels=c(">65","<=65")),
         obstruct=factor(obstruct, levels=c(0,1),labels=c("No","Yes")),
         perfor=factor(perfor, levels=c(0,1),labels=c("No","Yes")),
         adhere=factor(adhere, levels=c(0,1),labels=c("No","Yes")),
         differ=factor(differ, levels=c(1,2,3),labels=c("well","moderate","poor")),
         extent=factor(extent, levels=c(1,2,3,4),
                       labels=c("submucosa","muscle","serosa","contiguous")),
         surg=factor(surg, levels=c(0,1),labels=c("short","long")),
         node4=factor(node4, levels=c(0,1),labels=c("No","Yes")),
         rx=ifelse(rx==3,0,1),
         rx=factor(rx,levels=c(0,1))
  )

str(df)
## 'data.frame':    619 obs. of  12 variables:
##  $ time    : num  968 3087 542 245 523 ...
##  $ status  : num  1 0 1 1 1 1 0 0 0 1 ...
##  $ rx      : Factor w/ 2 levels "0","1": 1 1 2 1 2 1 2 1 1 2 ...
##  $ sex     : Factor w/ 2 levels "female","male": 2 2 1 1 2 1 2 1 2 2 ...
##  $ age     : Factor w/ 2 levels ">65","<=65": 2 2 1 1 1 2 2 1 2 2 ...
##  $ obstruct: Factor w/ 2 levels "No","Yes": 1 1 1 2 1 1 1 1 1 1 ...
##  $ perfor  : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ adhere  : Factor w/ 2 levels "No","Yes": 1 1 2 1 1 1 1 1 1 1 ...
##  $ differ  : Factor w/ 3 levels "well","moderate",..: 2 2 2 2 2 2 2 2 3 2 ...
##  $ extent  : Factor w/ 4 levels "submucosa","muscle",..: 3 3 2 3 3 3 3 3 3 3 ...
##  $ surg    : Factor w/ 2 levels "short","long": 1 1 1 2 2 1 1 2 2 1 ...
##  $ node4   : Factor w/ 2 levels "No","Yes": 2 1 2 2 2 2 1 1 1 1 ...

#不分亚组的分析####

#使用所有数据多因素Cox回归模型####

fit <- coxph(Surv(time, status) ~ rx, data = df)
broom::tidy(fit,exponentiate = T,conf.int = T)
## # A tibble: 1 × 7
##   term  estimate std.error statistic   p.value conf.low conf.high
##   <chr>    <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
## 1 rx1       1.67     0.119      4.32 0.0000156     1.32      2.11

#cox回归亚组分析（笨方法）（以sex为例）####

fit0 <- coxph(Surv(time, status) ~ rx, data = df[df$sex == "male",])
broom::tidy(fit0,exponentiate = T,conf.int = T)

fit0 <- coxph(Surv(time, status) ~ rx, data = df[df$sex == "female",])
broom::tidy(fit0,exponentiate = T,conf.int = T)

#亚组分析（tidyverse实现）####
#数据长宽转换####
dfl <- df %>% 
  pivot_longer(cols = 4:ncol(.),names_to = "var",values_to = "value") %>% 
  arrange(var)

head(dfl)

#亚组cox回归####
ress <- dfl %>% 
  group_by(var,value) %>% 
  group_nest(var,value) %>% 
  drop_na(value) %>% 
  mutate(#sample_size=map(data, ~ nrow(.x)),
    model=map(data, ~ coxph(Surv(time, status) ~ rx,data = .x)),
    res = map(model, broom::tidy,conf.int = T, exponentiate = T)
  ) %>% 
  dplyr::select(var,value,res)

glimpse(ress)

#先按照var value进行分组
#再将分组每个分组的数据嵌套为一个子数据框，存在列中
#model=map(data, ~ coxph(Surv(time, status) ~ rx,data = .x))
#作用是对列中所有嵌套的字数据框进行cox回归

#drop_na(value)
#将value中的NA删掉

#统计亚组治疗方式人群####
ss <- dfl %>% 
  group_by(var,value,rx) %>% 
  drop_na(value) %>% 
  summarise(sample_size=n()) %>% 
  dplyr::select(var,value,rx,sample_size)

#结果合并####
resss <- ress %>% 
  left_join(ss,by=c("var","value")) %>% 
  unnest(res,rx,sample_size) %>% 
  pivot_wider(names_from = "rx",values_from = "sample_size",names_prefix = "rx_") %>% 
  select(-c(term,std.error,statistic)) %>% 
  mutate(across(where(is.numeric), round,digits=2)) %>% 
  mutate(`HR(95%CI)`=paste0(estimate,"(",conf.low,"-",conf.high,")"))

str(resss)

head(resss)

#pivot_wider(names_from = "rx", values_from = "sample_size", names_prefix = "rx_")####
#功能：将长格式数据转换为宽格式
#按 rx（治疗组）分组显示样本量

#select(-c(term, std.error, statistic))####
#删除不需要的列（term, std.error, statistic）

#mutate(across(where(is.numeric), round,digits=2))####
#对所有数值列统一保留2位小数

#mutate(`HR(95%CI)`=paste0(estimate,"(",conf.low,"-",conf.high,")"))####
#非标准列名：当列名包含 特殊字符（如括号、空格、百分号等） 或 与R语法冲突的符号 时
#必须用反引号 ` 或引号 " 包裹
#为什么 "(", "-", ")" 要用引号框起来####
#括号 () 和连字符 - 是 需要原样输出的字符，必须用引号声明为字符串


#合并之前的fit模型####
fit <- coxph(Surv(time, status) ~ rx, data = df)
res_all <- broom::tidy(fit,exponentiate = T,conf.int = T)

res_all <- res_all %>% 
  mutate(var="All people",
         value=" ",
         rx_0=304,
         rx_1=305,
         across(where(is.numeric), round,digits=2)
  ) %>% 
  mutate(`HR(95%CI)`=paste0(estimate,"(",conf.low,"-",conf.high,")")
  ) %>% 
  select(var,value,estimate,p.value,conf.low,conf.high,rx_0,rx_1,`HR(95%CI)`)

#合并####

resss <- bind_rows(res_all,resss)
head(resss)


write.csv(resss, file = "resss.csv",quote = F,row.names = T)

plot_df <- read.csv(file = "C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\R_forestplot\\datasets\\resss.csv",check.names = F)
plot_df


#去除部分NA便于画图####
plot_df[,c(3,6,7)][is.na(plot_df[,c(3,6,7)])] <- " "
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
plot_df

#画图####
library(forestploter)
library(grid)

p <- forest(
  data = plot_df[,c(1,6,7,9,8,3)],
  lower = plot_df$conf.low,
  upper = plot_df$conf.high,
  est = plot_df$estimate,
  ci_column = 4,
  sizes = (plot_df$estimate+0.001)*0.3, 
  ref_line = 1, 
  xlim = c(0.1,4)
)
print(p)

#ci_column = 4####
#指定森林图中置信区间显示的列位置（第四列开始画森林）

#est = plot_df$estimate####
#作用：效应量的点估计值（如HR的对数值或原始值）

#sizes = (plot_df$estimate + 0.001) * 0.3####
#作用：控制森林图中点（效应量）的大小
#调整点的大小（值越小，点越小）



























