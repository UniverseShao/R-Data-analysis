# 准备数据####
rm(list = ls())
library(survival)
str(colon)
#拟合回归模型之前先将所有的分类变量进行factor处理 
#由于要要是逻辑回归，所以我们不用其中的生存时间这个变量
suppressMessages(library(tidyverse))

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
library(jstable)
#亚组分析####
res <- TableSubgroupMultiGLM(
  
  # 指定公式，不要乱写！
  formula = status ~ rx, 
  
  # 指定哪些变量有亚组
  var_subgroups = c("sex","age","obstruct","perfor","adhere",
                    "differ","extent","surg","node4"), 
  data = df #指定你的数据
)
res

plot_df <- res[,c("Variable","Count","OR","Lower","Upper","P value",
                  "P for interaction")]
#书本上为加 转 去NA顺序####
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
plot_df[,2:7] <- apply(plot_df[,2:7],2,as.numeric)
plot_df[,c(2,6,7)][is.na(plot_df[,c(2,6,7)])] <- " "

#森林图数据处理操作顺序####
#增加空行画森林####
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
#增加OR(95%CI)列####
plot_df <- plot_df%>%
  mutate(`OR(95%CI)`=paste0(`OR`,"(",Lower,"-",Upper,")")
  )
#去除所有NA####
plot_df[,c(2,6,7)][is.na(plot_df[,c(2,6,7)])] <- " "  
#去除所有NA(NA-NA)
plot_df$`OR(95%CI)`[plot_df$`OR(95%CI)` == "NA(NA-NA)" ] <- ""
#将数据转换成数值型####
plot_df[,3:5] <- apply(plot_df[,3:5],2,as.numeric)

#可以加去转，也可以加转去
#唯一要注意的就是在lower upper est这三个要进行计算的变量
#转换成数值型后会自动生成NA
#这三列的NA就不要再去除操作了，这时候去除会将这三列又变成字符





#画图####

library(forestploter)
library(grid)

p <- forest(
  data = plot_df[,c(1,2,9,6,8,7)],
  lower = plot_df$Lower,
  upper = plot_df$Upper,
  est = plot_df$OR,
  ci_column = 5,
  sizes = (plot_df$OR+0.001)*0.3, 
  ref_line = 1, 
  xlim = c(0,4)
)
print(p)







































































