# 加载需要的R包和数据-----------------------------------------------------------
library(survival)

rm(list = ls())

dim(lung)

str(lung)

# 8.2 方法1：rms--------------------------------------------------------------------------

library(rms)
# 使用rms包需要对数据进行“打包”操作
dd <- datadist(lung)
options(datadist = "dd")

#构建cox比例风险模型
# 选择部分变量演示
coxfit <- cph(Surv(time, status) ~ age+sex+ph.ecog+ph.karno+pat.karno,
              data = lung, x=T,y=T,surv = T
)
coxfit
#指定你要计算哪一年的生存率
# 构建生存函数，注意你的最大生存时间
surv <- Survival(coxfit) #Survival()函数根据模型计算生存概率，为rms包专属函数
surv1 <- function(x) surv(365,x) # 1年OS，创建函数，根据线性预测值计算一年生存率
surv2 <- function(x) surv(365*2,x) # 2年OS，创建函数，根据线性预测值计算二年生存率

# 构建列线图数据
nom <- nomogram(coxfit,
                fun = list(surv1,surv2),
                funlabel = c('1-year survival Probability',
                             '2-year survival Probability'),
                fun.at = c(0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1))

#画图
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

# 8.3 方法2：regplot---------------------------------------------------------------------


library(regplot)

# 建立cox回归模型
coxfit <- cph(Surv(time, status) ~ age+sex+ph.ecog+ph.karno+pat.karno,
              data = lung, x=T,y=T,surv = T
)

# 画图即可
aa <- regplot(coxfit,
              #连续性变量形状，"no plot""density""boxes""ecdf"
              #"bars""boxplot""violin""bean" "spikes"；
              #分类变量的形状，可选"no plot" "boxes" "bars" "spikes"
              plots = c("violin", "boxes"), 
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




























