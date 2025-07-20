#jstable####
#该包作用非常强大，有着一行代码实现亚组分析能力
rm(list = ls())
#准备数据####
library(survival)
str(colon)

#只选择Obs组和Lev+5FU组的患者
#所有的分类变量都变为factor
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


#亚组分析####
library(jstable)
res <- TableSubgroupMultiCox(
  
  # 指定公式
  formula = Surv(time, status) ~ rx, 
  
  # 指定哪些变量有亚组
  var_subgroups = c("sex","age","obstruct","perfor","adhere",
                    "differ","extent","surg","node4"), 
  data = df #指定你的数据
)

res

#画森林图####
plot_df <- res

plot_df[,c(2,3,4,5,6,7,8,9,10)][is.na(plot_df[,c(2,3,4,5,6,7,8,9,10)])] <- " "
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")
plot_df[,4:6] <- apply(plot_df[,4:6],2,as.numeric)
plot_df <- plot_df%>%
  mutate(`HR(95%CI)`=paste0(`Point Estimate`,"(",Lower,"-",Upper,")")
  )

plot_df


plot_df$`HR(95%CI)`[plot_df$`HR(95%CI)` == "NA(NA-NA)" ] <- ""
plot_df$` ` <- paste(rep(" ", nrow(plot_df)), collapse = " ")


library(forestploter)
library(grid)

p <- forest(
  data = plot_df[,c(1,2,3,7,8,11,9,12,10)],
  lower = plot_df$Lower,
  upper = plot_df$Upper,
  est = plot_df$`Point Estimate`,
  ci_column = 6,
  #sizes = (plot_df$estimate+0.001)*0.3, 
  ref_line = 1, 
  xlim = c(0.1,4)
)
print(p)

#森林图数据注意事项####
#森林图中的数据一定要记得是转换成数值型####
#简易代码####
#对数据框进行森林图画图前处理的简易代码

resss <- ress %>% 
  left_join(ss,b=c("var","value")) %>% 
  unnest(res,rx,sample_size) %>% 
  pivot_wider(names_from = "rx",values_from = "sample_size",names_prefix = "rx_") %>% 
  select(-c(term,std.error,statistic)) %>% 
  mutate(across(where(is.numeric), round,digits=2)) %>% 
  mutate(`HR(95%CI)`=paste0(estimate,"(",conf.low,"-",conf.high,")"))

#直接使用管道操作符更简单
#mutate(across(where(is.numeric), round,digits=2))
#只是选择是数值型向量的列，并将其的小数保留两位




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








