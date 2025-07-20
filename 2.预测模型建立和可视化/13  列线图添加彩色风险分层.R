# 13.1 加载数据和R包------------------------------------------------------------
library(survival)
library(rms)
dim(lung)
str(lung)
# 13.2 传统列线图---------------------------------------------------------------

dd <- datadist(lung)
options(datadist = "dd")

coxfit <- cph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
              data = lung, surv = T)

# 构建生存函数，注意你的最大生存时间
surv <- Survival(coxfit) 
surv1 <- function(x) surv(365,x) # 1年OS
surv2 <- function(x) surv(365*2,x) # 2年OS

nom <- nomogram(coxfit,
                fun = list(surv1,surv2),
                lp = T,
                funlabel = c('1-year survival Probability',
                             '2-year survival Probability'),
                maxscale = 100,
                fun.at = c(0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1))


plot(nom, 
     lplabel="Linear Predictor",
     xfrac = 0.2, # 左侧标签距离坐标轴的距离
     #varname.label = TRUE, 
     tcl = -0.2, # 刻度长短和方向 
     lmgp = 0.1, # 坐标轴标签距离坐标轴远近
     points.label ='Points', 
     total.points.label = 'Total Points',
     cap.labels = FALSE,
     cex.var = 1, # 左侧标签字体大小
     cex.axis = 1, # 坐标轴字体大小
     col.grid = gray(c(0.8, 0.95))) # 竖线颜色

# 13.3 新型列线图---------------------------------------------------------------
plot(nom, 
     lplabel="Risk Stratification",
     xfrac = 0.2, # 左侧标签距离坐标轴的距离
     #varname.label = TRUE, 
     tcl = -0.2, # 刻度长短和方向 
     lmgp = 0.1, # 坐标轴标签距离坐标轴远近
     points.label ='Points', 
     total.points.label = 'Total Points',
     cap.labels = FALSE,
     cex.var = 1, # 左侧标签字体大小
     cex.axis = 1, # 坐标轴字体大小
     col.grid = gray(c(0.8, 0.95))) # 竖线颜色
rect(0.29,0.20,0.5,0.26,col = "#01847F") # 添加彩色条带
rect(0.5,0.20,0.7,0.26,col = "#FBD26A")
rect(0.7,0.20,0.935,0.26,col = "#F40002")
text(0.4,0.18,"Low")
text(0.6,0.18,"Medium")
text(0.83,0.18,"High")

# 13.4 继续改进-----------------------------------------------------------------
#pdf("nomogram.pdf")
plot(nom, 
     lplabel="Risk Stratification",#名字就不改了
     xfrac = 0.2, # 左侧标签距离坐标轴的距离
     #varname.label = TRUE, 
     tcl = -0.2, # 刻度长短和方向 
     lmgp = 0.1, # 坐标轴标签距离坐标轴远近
     points.label ='Points', 
     total.points.label = 'Total Points',
     cap.labels = FALSE,
     cex.var = 1, # 左侧标签字体大小
     cex.axis = 1, # 坐标轴字体大小
     col.grid = gray(c(0.8, 0.95))) # 竖线颜色
rect(0.29,0.245,0.5,0.26,col = "#01847F") # 添加彩色条带
rect(0.5,0.245,0.7,0.26,col = "#FBD26A")
rect(0.7,0.245,0.935,0.26,col = "#F40002")
text(0.4,0.28,"Low")
text(0.6,0.28,"Medium")
text(0.83,0.28,"High")
#在底部再增加3个彩色条带，高度错开，显得有层次感
rect(0.37,0.14,0.5,0.144,col = "#01847F")
rect(0.5,0.144,0.7,0.148,col = "#FBD26A")
rect(0.7,0.148,0.835,0.152,col = "#F40002")

# 添加箭头
arrows(0.205,0.86,0.205,0.96,col = "steelblue",lwd = 4,length = 0.1)
arrows(0.4,0.76,0.4,0.96,col = "steelblue",lwd = 4,length = 0.1)
arrows(0.68,0.655,0.68,0.96,col = "steelblue",lwd = 4,length = 0.1)
arrows(0.28,0.55,0.28,0.96,col = "steelblue",lwd = 4,length = 0.1)
arrows(0.47,0.45,0.47,0.96,col = "steelblue",lwd = 4,length = 0.1)

# 总分箭头，加起来可能不对，单纯演示下
arrows(0.84,0.40,0.84,0.35,col = "#F40002",lwd = 4,length = 0.1)

#dev.off()


