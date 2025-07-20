# 1.数据------------------------------------------------------------------------
library(survival)
library(rms)
rm(list = ls())
dim(lung)
str(lung)
# 2.方法1-----------------------------------------------------------------------
##### 2.1 rms包打包数据---------------------------------------------------------
dd <- datadist(lung)
options(datadist = "dd")
##### 2.2构建cox比例风险模型----------------------------------------------------
coxfit <- cph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
              data = lung, x=T,y=T,surv = T
)

# 构建生存函数，注意你的最大生存时间
surv <- Survival(coxfit) 
surv1 <- function(x) surv(365,x) # 1年OS，定义一个函数，计算 1年生存概率（365天）
surv2 <- function(x) surv(365*2,x) # 2年OS

nom <- nomogram(coxfit,
                fun = list(surv1,surv2),
                lp = T,
                funlabel = c('1-year survival Probability',
                             '2-year survival Probability'),
                maxscale = 100,
                fun.at = c(0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1))
##### 2.3画图-------------------------------------------------------------------
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


# 3.方法2-----------------------------------------------------------------------
#我自己也没看懂这种方法
library(DynNom)

coxfit <- cph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
              data = lung, x=T,y=T,surv = T
)
DynNom(coxfit,
       DNxlab = "Survival probability",
       KMtitle="Kaplan-Meier plot", 
       KMxlab = "Time (Days)", 
       KMylab = "Survival probability")

# 4.方法3-----------------------------------------------------------------------
library(regplot)

coxfit <- cph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno + pat.karno,
              data = lung, x=T,y=T,surv = T
)

regplot(coxfit,
        plots = c("violin", "boxes"), ##连续性变量形状，可选"no plot" "density" "boxes" "ecdf" "bars" "boxplot" "violin" "bean" "spikes"；分类变量的形状，可选"no plot" "boxes" "bars" "spikes"
        observation = lung[1,], #用哪行观测，或者T F
        center = T, # 对齐变量
        subticks = T,
        droplines = T,#是否画竖线
        title = "nomogram",
        points = T, # 截距项显示为0-100
        odds = T, # 是否显示OR值
        showP = T, # 是否显示变量的显著性标记
        rank = "sd", # 根据sd给变量排序
        interval="confidence", # 展示可信区间
        clickable = F # 是否可以交互
)

