#森林图只要提供以下4个参数####
forestplot(
  labletext = xxx,
  lower = aaa,
  upper = bbb,
  mean = ccc,
)
#构造数据####
# 这个包竟然支持管道符！但其实我觉得不实用
library(forestplot)

# 构造森林图部分的数据,需要最大值最小值和均值3列,需要是数据框格式
df <- data.frame(
  mean  = c(NA, NA, 0.578, 0.165, 0.246, 0.700, 0.348, 0.139, 1.017, NA, 0.531), 
  lower = c(NA, NA, 0.372, 0.018, 0.072, 0.333, 0.083, 0.016, 0.365, NA, 0.386),
  upper = c(NA, NA, 0.898, 1.517, 0.833, 1.474, 1.455, 1.209, 2.831, NA, 0.731)
)

df
# 构造文字部分的数据，需要矩阵格式
# 使用  \n  换行
tabletext <- cbind(
  c("", "Study", "Auckland", "Block", "Doran", "Gamsu", "Morrison", 
    "Papageorgiou", "Tauesch", NA, "Summary"),
  c("Deaths\ntest", "(steroid)", "36", "1", "4", "14", "3", "1", "8", NA, NA),
  c("Deaths\njust show", "(placebo)", "60", "5", "11", "20", "7", "7", "10", NA, NA),
  c("", "OR", "0.58", "0.16", "0.25", "0.70", "0.35", "0.14", "1.02", NA, "0.53")
)
#cbind()函数是常用的合并矩阵和数据库的函数
#上述操作就是将几个字符型型的向量合并在一起
tabletext
##       [,1]           [,2]           [,3]                [,4]  
##  [1,] ""             "Deaths\ntest" "Deaths\njust show" ""    
##  [2,] "Study"        "(steroid)"    "(placebo)"         "OR"  
##  [3,] "Auckland"     "36"           "60"                "0.58"
##  [4,] "Block"        "1"            "5"                 "0.16"
##  [5,] "Doran"        "4"            "11"                "0.25"
##  [6,] "Gamsu"        "14"           "20"                "0.70"
##  [7,] "Morrison"     "3"            "7"                 "0.35"
##  [8,] "Papageorgiou" "1"            "7"                 "0.14"
##  [9,] "Tauesch"      "8"            "10"                "1.02"
## [10,] NA             NA             NA                  NA    
## [11,] "Summary"      NA             NA                  "0.53"
#基本森林图####
# 画图
forestplot(labeltext = tabletext, 
           lower = df$lower,
           upper = df$upper,
           mean = df$mean)
#细节调整####
forestplot(labeltext = tabletext, 
           lower = df$lower,
           upper = df$upper,
           mean = df$mean,
           
           # 这个参数设置每一行的文字部分是否加粗，
           # 一行一个逻辑值表示是否加粗，构成一个向量，
           is.summary = c(rep(TRUE, 2), rep(FALSE, 8), TRUE),
           
           # 加横线，默认加3条，类似三线表，可以自由控制添加位置
           hrzl_lines = gpar(col = "#444444"),
           
           clip = c(0.1, 2.5), # 控制箭头
           xlog = TRUE, # 横坐标是否转换，可以看到竖线变成1的位置了，
           # 也可以通过zero=1设置
           
           # fpColors控制森林图部分的各组件颜色
           col = fpColors(box = "royalblue", # 正方形颜色
                          lines = "red", # 误差线颜色
                          summary = "black", # 汇总标志颜色
                          zero = "yellow" # 0（1）竖线的颜色
           )
)
is.summary = c(rep(TRUE, 2), rep(FALSE, 8), TRUE)
#这是一个逻辑向量
#[1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE
#rep()函数是重复函数，指的是将TRUE重复两遍，将FALSE重复8遍
#在forestplot()函数中TRUE指的是——每一行的文字部分是否加粗

#hrzl_lines（horizontal lines 的缩写）####
#用于控制森林图中的水平分隔线**
#例如表格顶部的标题线、数据行之间的分隔线、底部的汇总线等

#gpar()——图形参数函数——定义颜色、线宽、线型等图形属性
#col = "#444444"——指定水平分隔线的颜色为十六进制颜色代码 #444444，这是一种深灰色

#clip = c(0.1, 2.5)控制箭头####
#定义横坐标的显示范围：强制将横轴（OR值）限制在 [0.1, 2.5] 之间
#超出范围的置信区间：若某个研究的置信区间超出此范围，其线段末端会显示箭头
#表示实际值超出当前显示范围

#xlog = TRUE：横坐标对数转换
#将横轴（OR值）转换为对数刻度，
#使 OR=1 的位置位于横轴中点，左右刻度对称（如 0.5, 1, 2）
#对称性意义：OR值的倍数关系在对数刻度下呈线性，便于直观比较效应方向
#OR=0.5 → log(0.5)≈-0.3，OR=2 → log(2)≈0.3，二者对称

#加横线####
forestplot(labeltext = tabletext, 
           lower = df$lower,
           upper = df$upper,
           mean = df$mean,
           
           # 这个参数设置每一行的文字部分是否加粗，
           # 一行一个逻辑值表示是否加粗，构成一个向量，
           is.summary = c(rep(TRUE, 2), rep(FALSE, 8), TRUE),
           
           # 加横线,控制位置,第一行文字上面序号是1，下面序号是2，
           # 第2行文字下面序号是3，以此类推
           # column参数控制横线出现在哪几列
           hrzl_lines = list("1" = gpar(lwd = 2),
                             "3" = gpar(lwd = 2), 
                             "4" = gpar(lwd = 60, lineend="butt",col = "#99999922"),
                             "8" = gpar(lwd = 60, lineend="butt",col = "#99999922"),
                             "11" = gpar(lwd = 2, columns = 1:4)
           )
)
#"1" = gpar(lwd = 2)
#作用——表格的第1行顶部添加一条线宽为 2 的实线突出标题分隔
#"4" = gpar(lwd = 60, lineend="butt", col = "#99999922")
#作用位置：表格的第4行区域
#lwd = 60：线宽极宽（60磅），实际形成背景色块
#ineend = "butt"线端平切（避免圆角或突出）
#col = "#99999922"颜色为浅灰色（#999999）且透明度 22
#效果：在第4行区域添加半透明的灰色背景，用于高亮显示特定数据行（如亚组分析）

#list() 函数的核心作用
#功能：将多个键值对（Key-Value Pairs）组合成一个列表对象，用于存储复杂结构的数据
#在森林图中的意义：通过 list() 将不同行的水平线设置整合为单一参数 hrzl_lines，使代码更清晰



#加竖线####
forestplot(labeltext = tabletext, 
           lower = df$lower,
           upper = df$upper,
           mean = df$mean,
           
           # 这个参数设置每一行的文字部分是否加粗，
           # 一行一个逻辑值表示是否加粗，构成一个向量，
           is.summary = c(rep(TRUE, 2), rep(FALSE, 8), TRUE),
           
           vertices = TRUE, # 误差线末尾加竖线
           
           # 加竖线，可以直接用TRUE
           grid = structure(c(0.5,1, 1.5), 
                            gp = gpar(lty = 2, col = "#CCCCFF")), 
)

# vertices = TRUE误差线末尾加竖线####



#grid 参数的核心作用####
#forestplot包中，grid 参数用在森林图的横轴（如OR值）上添加垂直参考线
#OR=1（无效应参考线）
#OR=0.5（保护性效应阈值）
#OR=2.0（风险性效应阈值）

#c(0.5, 1, 1.5)在横轴的 0.5、1、1.5 处添加垂直参考线
#gp = gpar(...)定义参考线的图形属性
#lty = 2：线型为虚线（1=实线，2=虚线，3=点线）


#控制横坐标标签####
forestplot(labeltext = tabletext, 
           lower = df$lower,
           upper = df$upper,
           mean = df$mean,
           
           # 这个参数设置每一行的文字部分是否加粗，
           # 一行一个逻辑值表示是否加粗，构成一个向量，
           is.summary = c(rep(TRUE, 2), rep(FALSE, 8), TRUE),
           
           xticks = c(0.1,0.5,1,1.5,2)
)
#使用 xticks参数来控制横坐标标签可以保留想要的横坐标
#xticks 参数####
#完全覆盖默认刻度：通过显式指定 xticks 的数值，
#横坐标将仅显示这些位置的刻度，并移除所有自动生成的默认刻度
#定义刻度位置：横轴会严格按照 c(0.1, 0.5, 1, 1.5, 2) 数值位置显示刻度线及标签

#对横坐标的影响
#若 xlog = TRUE（对数刻度）
#刻度位置为 0.1, 0.5, 1, 1.5, 2，但实际绘制时会转换为对数坐标（如 log(0.1), log(0.5) 等）
#刻度标签仍显示原始值（0.1, 0.5, 1, 1.5, 2），而非对数值
#若 xlog = FALSE（线性刻度）
#刻度位置和标签均为 0.1, 0.5, 1, 1.5, 2



#增加标题，调整各组件大小####
forestplot(labeltext = tabletext, 
           lower = df$lower,
           upper = df$upper,
           mean = df$mean,
           
           # 这个参数设置每一行的文字部分是否加粗，
           # 一行一个逻辑值表示是否加粗，构成一个向量，
           is.summary = c(rep(TRUE, 2), rep(FALSE, 8), TRUE),
           
           # 加横线,控制位置,第一行文字上面序号是1，下面序号是2,
           # 第2行文字下面序号是3，以此类推
           # column参数控制横线出现在哪几列
           hrzl_lines = list(#"1" = gpar(lwd = 2),
             "3" = gpar(lwd = 2), 
             "4" = gpar(lwd = 60, lineend="butt",col = "#99999922"),
             "8" = gpar(lwd = 60, lineend="butt",col = "#99999922"),
             "11" = gpar(lwd = 2, columns = c(1:5))
           ),
           
           #clip = c(0.1, 2.5), # 设置横坐标的范围
           xlog = TRUE, # 横坐标是否log
           
           # fpColors控制森林图部分的各组件颜色
           col = fpColors(box = "#1c61b6", # 正方形颜色
                          lines = "#1c61b6", # 误差线颜色
                          summary = "black", # 汇总标志颜色
                          zero = "grey60"
           ),
           vertices = TRUE, # 误差线末尾加竖线
           
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(1.2,'cm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           
           #定义标题
           title="A forestplot example",
           
           ##定义x轴
           xlab="<--------     ------>\nThis Better   That Better",
           
           lwd.xaxis=2,#设置X轴线的粗细
           
           #fpTxtGp函数中的cex参数设置各个组件的大小
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)
           ),
           
           lty.ci = "solid", # 可信区间线的类型
           graph.pos = 5 #设置森林图的位置，此处设置为5，则出现在第四列
)
#txt_gp 参数用于自定义森林图中所有文本元素的样式（如字体大小、颜色、字体类型等）
#label = gpar(cex = 1.25)
#控制对象：左侧表格中的所有文本
#研究名称（如 Auckland）
#数据列（如 Deaths (steroid), Deaths (placebo), OR）
#cex = 1.25
#将字体大小设置为默认值的 1.25 倍（放大25%）

#txt_gp 参数####
#：通过 fpTxtGp() 和 gpar() 分层控制文本样式


#ticks = gpar(cex = 1.1)
#控制对象：横坐标轴上的刻度标签（如 0.5, 1, 1.5, 2）
#cex = 1.1字体大小放大10%


#xlab = gpar(cex = 1.2)
#横坐标轴轴标签
#这里是控制"<--------     ------>\nThis Better   That Better"字体大小

#title = gpar(cex = 1.2)
#控制对象：图表的主标题
#cex = 1.2字体大小放大20%


#有亚组的森林图####
rm(list = ls())
library(forestplot)

rs_forest <- read.csv("C:\\Users\\Administrator\\Desktop\\R语言脚本(SWY精心编辑版本)\\R_forestplot\\datasets\\forestplot.csv", header = F)

#画图

#tiff('Figure1.tiff',height = 24,width = 24,res= 96,units = "cm")

forestplot(labeltext = as.matrix(rs_forest[,1:4]), #设置用于文本展示的列，此处我们用前四列
           mean = rs_forest$V5, #设置均值
           lower = rs_forest$V6, #设置均值的lowlimits限
           upper = rs_forest$V7, #设置均值的uplimits限
           
           #该参数接受一个逻辑向量，用于定义数据中每一行是否是汇总值，
           #若是，则在对应位置设置为TRUE，若否，则设置为FALSE；
           #设置为TRUE的行则以粗体出现
           is.summary=c(T,T,T,F,F,T,F,F,T,F,F,T,F,F,F,T,F,F,T,F,F,T,F,F,T,F,F),
           
           zero = 1, #设置参照值，此处我们展示的是HR值，故参照值是1，而不是0
           boxsize = 0.4, #设置点估计的方形大小
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           
           #定义标题
           #title="Overall Survival",
           
           ##定义x轴
           xlab="     <----------------   ---------------->\nAbiraterone Prednisone Better   Prednisone Alone Better",
           
           lwd.xaxis=2,#设置X轴线的粗细
           
           #fpTxtGp函数中的cex参数设置各个组件的大小
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          #title=gpar(cex = 1.2)
           ),
           
           #使用fpColors()函数定义图形元素的颜色，
           # 从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           col=fpColors(box='#458B00',
                        summary="#458B00", #8B008B
                        lines = 'black',
                        zero = '#7AC5CD'),
           
           #根据亚组的位置，设置线型，宽度造成“区块感”
           hrzl_lines=list(#"1" = gpar(lwd=2, lineend="butt", col="black"),
             "2" = gpar(lwd=2, lineend="butt", col="black"),
             "5" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "8" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "11" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "14" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "18" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "21" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "24" = gpar(lwd=50, lineend="butt", col="#99999922"),
             "27" = gpar(lwd=50, lineend="butt", col="#99999922")
           ),
           
           lty.ci = "solid",
           graph.pos = 4 #设置森林图的位置，此处设置为4，则出现在第四列
)

#注意这里可以直接套代码的原因是
#数据表中的所有labeltext的格式都是已经符合我们所需要的格式








