# 1.完全随机设计资料的方差分析--------------------------------------------------
trt<-c(rep("group1",30),rep("group2",30),rep("group3",30),rep("group4",30))
weight<-c(3.53,4.59,4.34,2.66,3.59,3.13,3.30,4.04,3.53,3.56,3.85,4.07,1.37,
          3.93,2.33,2.98,4.00,3.55,2.64,2.56,3.50,3.25,2.96,4.30,3.52,3.93,
          4.19,2.96,4.16,2.59,2.42,3.36,4.32,2.34,2.68,2.95,2.36,2.56,2.52,
          2.27,2.98,3.72,2.65,2.22,2.90,1.98,2.63,2.86,2.93,2.17,2.72,1.56,
          3.11,1.81,1.77,2.80,3.57,2.97,4.02,2.31,2.86,2.28,2.39,2.28,2.48,
          2.28,3.48,2.42,2.41,2.66,3.29,2.70,2.66,3.68,2.65,2.66,2.32,2.61,
          3.64,2.58,3.65,3.21,2.23,2.32,2.68,3.04,2.81,3.02,1.97,1.68,0.89,
          1.06,1.08,1.27,1.63,1.89,1.31,2.51,1.88,1.41,3.19,1.92,0.94,2.11,
          2.81,1.98,1.74,2.16,3.37,2.97,1.69,1.19,2.17,2.28,1.72,2.47,1.02,
          2.52,2.10,3.71)
data1<-data.frame(trt,weight)
head(data1)
##### 1.1查看数据分布-----------------------------------------------------------
data1$trt <- factor(data1$trt)
boxplot(weight ~ trt, data = data1)
##### 1.2完全随机设计资料的方差分析---------------------------------------------
fit <- aov(weight ~ trt, data = data1)
summary(fit)
###### 1.2.1可视化平均数和可信区间----------------------------------------------
library(gplots)
plotmeans(weight~trt,xlab = "treatment",ylab = "weight",
          main="mean plot\nwith95% CI")
###### 1.2.2小提琴图------------------------------------------------------------
ggplot(data1, aes(x=trt, y=weight)) + 
  geom_boxplot(width=0.2, 
               fill="green") +
  geom_violin(fill="gold", 
              alpha=0.3) +
  labs(x="trt", 
       y="weight", 
       title="Violin Plots of Miles Per Gallon")
###### 1.2.3并列箱线图----------------------------------------------------------
ggplot(data1, aes(x=trt, y=weight, fill = trt)) +           
  geom_boxplot() +                                           
  labs(x="trt",                              
       y="eight",                                 
       title="并列箱线图") +
  scale_fill_manual(values=c("gold", "green", "red", "blue"))
##### 1.3使用Tukey HSD成对多重组间比较------------------------------------------
pairwise <- TukeyHSD(fit)
pairwise
"$trt
                     diff        lwr        upr     p adj
group2-group1 -0.71500000 -1.1567253 -0.2732747 0.0002825
group3-group1 -0.73233333 -1.1740587 -0.2906080 0.0001909
group4-group1 -1.46400000 -1.9057253 -1.0222747 0.0000000
group3-group2 -0.01733333 -0.4590587  0.4243920 0.9996147
group4-group2 -0.74900000 -1.1907253 -0.3072747 0.0001302
group4-group3 -0.73166667 -1.1733920 -0.2899413 0.0001938"
###### 1.3.1单步法（single-step method） 计算调整后的 p 值----------------------
data1$trt <- factor(data1$trt)
fit <- aov(weight ~ trt, data = data1)
library(multcomp)
tuk <- glht(fit, linfct=mcp(trt="Tukey")) 
summary(tuk)
##### 1.4成对组间比较结果可视化-------------------------------------------------
###### 1.4.1第一种可视化方法(森林图)--------------------------------------------
plot(pairwise)
plotdata <- as.data.frame(pairwise[[1]])
plotdata$conditions <- row.names(plotdata)

library(ggplot2)
ggplot(data=plotdata, aes(x=conditions, y=diff)) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width=.2)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_point(size=3, color="red") +
  theme_bw() +
  labs(y="Difference in mean levels", x="", 
       title="95% family-wise confidence level") +
  coord_flip()
###### 1.4.2第二种可视化方法(箱线图)--------------------------------------------
labels1 <- cld(tuk, level=.05)$mcletters$Letters
labels2 <- paste(names(labels1), "\n", labels1)
ggplot(data=fit$model, aes(x=trt, y=weight)) +
  scale_x_discrete(breaks=names(labels1), labels=labels2) +
  geom_boxplot(fill="lightgrey") +
  theme_bw() +
  labs(x="Treatment",
       title="Distribution of Response Scores by Treatment",
       subtitle="Groups without overlapping letters differ signifcantly (p < .05)")
##### 1.5正态性检验假设---------------------------------------------------------
# Assessing normality
library(car)
qqPlot(fit, simulate=TRUE, main="Q-Q Plot")
##### 1.6方差齐性检验-----------------------------------------------------------
bartlett.test(weight ~ trt, data=data1)


# 2.随机区组设计资料的方差分析--------------------------------------------------
weight <- c(0.82,0.65,0.51,0.73,0.54,0.23,0.43,0.34,0.28,0.41,0.21,
            0.31,0.68,0.43,0.24)
block <- c(rep(c("1","2","3","4","5"),each=3))
group <- c(rep(c("A","B","C"),5))
data4_4 <- data.frame(weight,block,group)
head(data4_4)

fit <- aov(weight ~ block + group,data = data4_4)
summary(fit)


# 3.拉丁方设计方差分析(多个区组变量)--------------------------------------------
psize <- c(87,75,81,75,84,66,73,81,87,85,64,79,73,73,74,78,73,77,77,68,69,74,76,73,
           64,64,72,76,70,81,75,77,82,61,82,61)
drug <- c("C","B","E","D","A","F","B","A","D","C","F","E","F","E","B","A","D","C",
          "A","F","C","B","E","D","D","C","F","E","B","A","E","D","A","F","C","B")
col_block <- c(rep(1:6,6))
row_block <- c(rep(1:6,each=6))
mydata <- data.frame(psize,drug,col_block,row_block)
mydata$col_block <- factor(mydata$col_block)
mydata$row_block <- factor(mydata$row_block)
mydata$drug <- factor(mydata$drug)
str(mydata)

fit <- aov(psize ~ row_block + col_block + drug, data = mydata)
summary(fit)

# 4.两阶段交叉设计资料方差分析--------------------------------------------------
"
实验设计示例
假设比较两种药物 A 和 B 的疗效，实验分为两个阶段：

第一阶段：部分受试者接受 A，另一部分接受 B。
第二阶段：受试者交换处理（即第一阶段接受 A 的受试者改为 B，反之亦然）。
"
contain <- c(760,770,860,855,568,602,780,800,960,958,940,952,635,650,440,450,
             528,530,800,803)
phase <- rep(c("phase_1","phase_2"),10)
type <- c("A","B","B","A","A","B","A","B","B","A","B","A","A","B","B","A",
          "A","B","B","A")
testid <- rep(1:10,each=2)
mydata <- data.frame(testid,phase,type,contain)

str(mydata)
mydata$testid <- factor(mydata$testid)
table(mydata$phase,mydata$type)
fit <- aov(contain~phase+type+testid,mydata)
summary(fit)

# 多个样本均数间的多重比较------------------------------------------------------
rm(list = ls())
trt<-c(rep("group1",30),rep("group2",30),rep("group3",30),rep("group4",30))

weight<-c(3.53,4.59,4.34,2.66,3.59,3.13,3.30,4.04,3.53,3.56,3.85,4.07,1.37,
          3.93,2.33,2.98,4.00,3.55,2.64,2.56,3.50,3.25,2.96,4.30,3.52,3.93,
          4.19,2.96,4.16,2.59,2.42,3.36,4.32,2.34,2.68,2.95,2.36,2.56,2.52,
          2.27,2.98,3.72,2.65,2.22,2.90,1.98,2.63,2.86,2.93,2.17,2.72,1.56,
          3.11,1.81,1.77,2.80,3.57,2.97,4.02,2.31,2.86,2.28,2.39,2.28,2.48,
          2.28,3.48,2.42,2.41,2.66,3.29,2.70,2.66,3.68,2.65,2.66,2.32,2.61,
          3.64,2.58,3.65,3.21,2.23,2.32,2.68,3.04,2.81,3.02,1.97,1.68,0.89,
          1.06,1.08,1.27,1.63,1.89,1.31,2.51,1.88,1.41,3.19,1.92,0.94,2.11,
          2.81,1.98,1.74,2.16,3.37,2.97,1.69,1.19,2.17,2.28,1.72,2.47,1.02,
          2.52,2.10,3.71)

data1<-data.frame(trt,weight)
data1$trt <- factor(data1$trt)

str(data1)
fit <- aov(weight ~ trt, data = data1)
summary(fit)


# 1.LSD-t检验-------------------------------------------------------------------
library(agricolae)
res <- LSD.test(fit, "trt", p.adj = "none") # LSD-t检验#p.adj：p值校正方法（"none"为不校正）
res
##### 1.1结果解释---------------------------------------------------------------
##          weight groups
## group1 3.430333      a
## group2 2.715333      b
## group3 2.698000      b
## group4 1.966333      c
# 结果就是看后面的字母，不同字母之间的组是有显著差异

##### 1.2结果可视化-------------------------------------------------------------
plot(res)


# 2.TukeyHSD--------------------------------------------------------------------
pairwise <- TukeyHSD(fit) ### 每个组之间进行比较,多重比较
pairwise
##### 2.1可视化TukeyHSD(fit)的结果(绘制森林图)----------------------------------
plot(TukeyHSD(fit))
plotdata <- as.data.frame(pairwise[[1]])
plotdata$conditions <- row.names(plotdata)

library(ggplot2)
ggplot(data=plotdata, aes(x=conditions, y=diff)) + 
  geom_errorbar(aes(ymin=lwr, ymax=upr, width=.2)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_point(size=3, color="red") +
  theme_bw() +
  labs(y="Difference in mean levels", x="", 
       title="95% family-wise confidence level") +
  coord_flip()
##### 2.2可视化TukeyHSD(fit)的结果(箱线图)--------------------------------------
tuk <- glht(fit, linfct=mcp(trt="Tukey")) 
summary(tuk)
labels1 <- cld(tuk, level=.05)$mcletters$Letters
labels2 <- paste(names(labels1), "\n", labels1)
ggplot(data=fit$model, aes(x=trt, y=weight)) +
  scale_x_discrete(breaks=names(labels1), labels=labels2) +
  geom_boxplot(fill="lightgrey") +
  theme_bw() +
  labs(x="Treatment",
       title="Distribution of Response Scores by Treatment",
       subtitle="Groups without overlapping letters differ signifcantly (p < .05)")

# 3.Dunnett-t检验---------------------------------------------------------------
library(multcomp)
res <- glht(fit, linfct = mcp(trt = "Dunnett"))
# trt = "Dunnett" 所有处理组与单个对照组比较（而不是所有两两比较）
summary(res)
plot(res)

# 4.SNK-q检验-------------------------------------------------------------------
library(agricolae)
res <- SNK.test(fit, "trt") 
res
plot(res)









































