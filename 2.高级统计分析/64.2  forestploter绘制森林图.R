#准备数据####
tabletext <- data.frame(
  # 文本数据
  study =  c("Auckland", "Block", "Doran", "Gamsu", "Morrison", "Papageorgiou", 
             "Tauesch", "Summary"),
  steroid =  c("36", "1", "4", "14", "3", "1", "8", ""),
  placebo = c("60", "5", "11", "20", "7", "7", "10", ""),
  # 画“森林需要的数据”
  mean  = c(0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, 0.531), 
  lower = c(0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, 0.386),
  upper = c(0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, 0.731)
)

# 增加一列
tabletext$`HR (95% CI)` <- ifelse(is.na(tabletext$mean), "",
                                  sprintf("%.2f (%.2f to %.2f)",
                                          tabletext$mean, tabletext$lower, 
                                          tabletext$upper))

# 增加一列空值用于画“森林”
tabletext$` ` <- paste(rep(" ", 8), collapse = " ")

tabletext
##          study steroid placebo  mean lower upper         HR (95% CI)
## 1     Auckland      36      60 0.578 0.372 0.898 0.58 (0.37 to 0.90)
## 2        Block       1       5 0.165 0.018 1.517 0.16 (0.02 to 1.52)
## 3        Doran       4      11 0.246 0.072 0.833 0.25 (0.07 to 0.83)
## 4        Gamsu      14      20 0.700 0.333 1.474 0.70 (0.33 to 1.47)
## 5     Morrison       3       7 0.348 0.083 1.455 0.35 (0.08 to 1.46)
## 6 Papageorgiou       1       7 0.139 0.016 1.209 0.14 (0.02 to 1.21)
## 7      Tauesch       8      10 1.017 0.365 2.831 1.02 (0.36 to 2.83)
## 8      Summary                 0.531 0.386 0.731 0.53 (0.39 to 0.73)
##                  
## 1                
## 2                
## 3                
## 4                
## 5                
## 6                
## 7                
## 8


#ifelse(is.na(tabletext$mean), "", ...)
#作用：检查 tabletext$mean 列中的值是否为 NA（缺失值）
#如果 tabletext$mean 是 NA，则返回空字符串 ""
#如果 tabletext$mean 不是 NA，则执行 sprintf 函数

#sprintf("%.2f (%.2f to %.2f)", tabletext$mean, tabletext$lower, tabletext$upper)
#作用：将 mean、lower 和 upper 列的值格式化为一个字符串
#%.2f 表示将数值格式化为保留两位小数的浮点数
#第一个 %.2f 对应 tabletext$mean
#第二个 %.2f 对应 tabletext$lower
#第三个 %.2f 对应 tabletext$upper


#paste(rep(" ", 8), collapse = " ")
#作用：将 rep(" ", 8) 生成的向量中的空格字符拼接成一个字符串
#paste() 是 R 中用于拼接字符串的函数
#collapse = " " 表示将向量中的每个元素用空格连接起来
#因此，paste(rep(" ", 8), collapse = " ") 的结果是一个由 8 个空格组成的字符串：" "


#画图####
# 加载R包
library(forestploter)

p <- forest(data = tabletext[,c(1:3,8,7)], 
            # 选择文字部分，这里选了5列，
            # 其中4列是要呈现的文字信息，还有1列空值用于画“森林”
            
            lower = tabletext$lower, # 可信区间下限
            upper = tabletext$upper, # 可信区间上限
            est = tabletext$mean, # 估计值
            ci_column = 4 # “森林”出现在图的第几列
)

print(p)
#稍加美化####
p <- forest(data = tabletext[,c(1:3,8,7)],
            lower = tabletext$lower,
            upper = tabletext$upper,
            est = tabletext$mean,
            ci_column = 4,#“森林”出现在图的第几列
            sizes = tabletext$mean, # 控制方框的大小
            
            is_summary = c(rep(FALSE,nrow(tabletext)-1), TRUE), # 最后一列是汇总行
            ref_line = 1, # 把竖线放到1的位置
            xlim = c(0,3), # x轴范围
            ticks_at = c(0,1,2,3), # x轴刻度显示
            arrow_lab = c("this better","that better"), # x轴下面的文字
            footnote = "A simple example of forestploter" # 左下角脚注
)

print(p)

#is_summary = c(rep(FALSE,nrow(tabletext)-1), TRUE)
#作用：指定哪些行是汇总行
#rep(FALSE, nrow(tabletext) - 1) 表示前 n-1 行不是汇总行

#更改主题####
# 自定义主题，修改各种细节
tm <- forest_theme(base_size = 10, # 基础大小
                   
                   # 可信区间点的形状，线型、颜色、宽度
                   ci_pch = 16,
                   ci_col = "#4575b4", # #762a83
                   ci_lty = 1,
                   ci_lwd = 1.5,
                   ci_Theight = 0.2, # 可信区间两端加短竖线
                   
                   # 参考线宽度、形状、颜色
                   refline_lwd = 1,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   
                   # 汇总菱形的填充色和边框色
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   
                   # 脚注大小、字体、颜色
                   footnote_cex = 0.6,
                   footnote_fontface = "italic",
                   footnote_col = "blue")

p1 <- forest(data = tabletext[,c(1:3,8,7)],
             lower = tabletext$lower,
             upper = tabletext$upper,
             est = tabletext$mean,
             ci_column = 4,
             
             is_summary = c(rep(FALSE,nrow(tabletext)-1), TRUE), # 最后一列是汇总行
             ref_line = 1, # 把竖线放到1的位置
             xlim = c(0,3), # x轴范围
             ticks_at = c(0,1,2,3), # x轴刻度显示
             arrow_lab = c("this better","that better"), # x轴下面的文字
             footnote = "A simple example of forestploter", # 左下角脚注
             
             theme = tm
)

print(p1)
# ci_pch = 16指的是实心圆形
#ci_lwd = 1.5为设置线的粗度


#编辑图形####
library(grid)
# 改变第3行文字颜色
g <- edit_plot(p1, row = 3, gp = gpar(col = "red", fontface = "italic"))

# 加粗字体
g <- edit_plot(g,
               row = c(2, 5),
               gp = gpar(fontface = "bold"))

# 改变第6行的背景色
g <- edit_plot(g, row = 6, which = "background",
               gp = gpar(fill = "darkolivegreen1"))

# 在顶部增加文字
g <- insert_text(g,
                 text = "A short title",
                 col = 3:4,
                 part = "header",
                 gp = gpar(fontface = "bold"))

# 增加下划线
g <- add_border(g, part = "header",where = "bottom")


# 插入文字
g <- insert_text(g,
                 text = "This is a long text. Age and gender summarised above.
                 \nBMI is next",
                 row = 7,
                 just = "left",#指定文字左对齐
                 gp = gpar(cex = 0.6, col = "green", fontface = "italic"))

plot(g)


#有亚组的森林图####
rm(list = ls())

df <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\R_forestplot\\datasets\\forestplot.csv", header = T)

df$` ` <- paste(rep(" ", 26), collapse = " ")

# 改个列名
colnames(df)[4:7] <- c("HR(95%CI)","mean","low","high")

df
##                         Subgroup   AP   PA       HR(95%CI) mean  low high
## 1                   All patients   NR 27.2 0.75(0.61-0.93) 0.75 0.61 0.93
## 2                  Baseling ECOG                             NA   NA   NA
## 3                              0   NR 27.2 0.71(0.55-0.92) 0.71 0.55 0.92
## 4                              1   NR 26.4 0.86(0.58-1.28) 0.86 0.58 1.28
## 5                Baseling BPI-SF                             NA   NA   NA
## 6                            0~1   NR 27.2 0.71(0.54-0.94) 0.71 0.54 0.94
## 7                            2~3 25.5   NR 0.87(0.59-1.29) 0.87 0.59 1.29
## 8  Bone metastases only at entry                             NA   NA   NA
## 9                            Yes   NR 27.2 0.68(0.48-0.96) 0.68 0.48 0.96
## 10                            No   NR 27.5 0.81(0.61-1.06) 0.81 0.61 1.06
## 11                           Age                             NA   NA   NA
## 12                           <65   NR   NR  0.8(0.51-1.24) 0.80 0.51 1.24
## 13                           ≥65   NR 26.4  0.73(0.57-0.94 0.73 0.57 0.94
## 14                           ≥75   NR 23.8    0.71(0.51-1) 0.71 0.51 1.00
## 15     Baseline PSA above median                             NA   NA   NA
## 16                           Yes 26.9 23.8 0.72(0.43-0.94) 0.72 0.43 0.94
## 17                            No   NR   NR  0.77(0.38-1.09 0.77 0.38 1.09
## 18     Baseline LDH above median                             NA   NA   NA
## 19                           Yes   NR 23.6 0.69(0.53-0.91) 0.69 0.53 0.91
## 20                            No   NR 27.5 0.79(0.55-1.12) 0.79 0.55 1.12
## 21   Baseline ALK-P above median                             NA   NA   NA
## 22                           Yes   NR 23.6  0.79(0.6-1.04) 0.79 0.60 1.04
## 23                            No   NR 27.5 0.66(0.46-0.94) 0.66 0.46 0.94
## 24                        Region                             NA   NA   NA
## 25                 North America   NR 27.2 0.66(0.49-0.88) 0.66 0.49 0.88
## 26                         Other   NR   NR 0.89(0.65-1.22) 0.89 0.65 1.22
##                                                       
## 1                                                     
## 2                                                     
## 3                                                     
## 4                                                     
## 5                                                     
## 6                                                     
## 7                                                     
## 8                                                     
## 9                                                     
## 10                                                    
## 11                                                    
## 12                                                    
## 13                                                    
## 14                                                    
## 15                                                    
## 16                                                    
## 17                                                    
## 18                                                    
## 19                                                    
## 20                                                    
## 21                                                    
## 22                                                    
## 23                                                    
## 24                                                    
## 25                                                    
## 26

#rm(list = ls()) 会永久删除工作环境中的所有对象
#画亚组森林图####
library(forestploter)
library(grid)

# 定义主题
tm <- forest_theme(base_size = 14, # 基础大小
                   
                   # 可信区间点的形状，线型、颜色、宽度
                   ci_pch = 16,
                   ci_col = "#4575b4", # #762a83
                   ci_lty = 1,
                   ci_lwd = 2,
                   ci_Theight = 0.2, # 可信区间两端加短竖线
                   
                   # 参考线宽度、形状、颜色
                   refline_lwd = 1.5,
                   refline_lty = "dashed",
                   refline_col = "grey20",
                   
                   # 汇总菱形的填充色和边框色
                   summary_fill = "#4575b4",
                   summary_col = "#4575b4",
                   
                   # 脚注大小、字体、颜色
                   footnote_cex = 0.8,
                   footnote_fontface = "italic",
                   footnote_col = "grey30",
                   
                   # 自定义背景色、前景色。fontface:1常规，2粗体，3斜体，4粗斜体 
                   core = list(bg_params = list(fill = c("#FFFFFF","#E6E6E6"), 
                                                col=NA),
                               fg_params = list(
                                 fontface=c(2,rep(c(2,3,3),3),2,3,3,3,
                                            rep(c(2,3,3),4)))
                   ),
                   
                   # 标题行颜色、字体
                   colhead = list(fg_params = list(col="navyblue", fontface=2))
)

# 画图
p <- forest(
  data = df[,c(1:3,8,4)],
  lower = df$low,
  upper = df$high,
  est = df$mean,
  ci_column = 4,
  sizes = df$mean,
  is_summary = c(TRUE,rep(FALSE,nrow(df)-1)), # 最后一列是汇总行
  ref_line = 1, # 把竖线放到1的位置
  xlim = c(0.3,1.5), # x轴范围
  ticks_at = c(0.5,1,1.5), # x轴刻度显示
  arrow_lab = c("this better","that better"), # x轴下面的文字
  footnote = "Add other information here.\nAnd free to change to the next line.", # 左下角脚注
  
  theme = tm
  
)

# 增加下划线
g <- add_border(p, part = "header",where="bottom",gp = gpar(lwd=2)) # add_border

print(g)



























































