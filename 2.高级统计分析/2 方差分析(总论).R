# 方差分析其实可以理解成连续变量和分类变量的一种相关性分析----------------------
# 只是方法是连续变量根据分类变量分组，然后比较组间的均值差异
# 前提要求各组数据正太分布，并且满足方差齐性
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

# 3.单因素协方差分析------------------------------------------------------------
library(multcomp)
library(dplyr)
litter %>%
  group_by(dose) %>%
  summarise(n=n(), mean=mean(gesttime), sd=sd(gesttime))
litter$dose <- factor(litter$dose)
fit <- aov(weight ~ gesttime + dose, data=litter)                             
summary(fit)
##### 3.1单因素协方差分析细节解读-----------------------------------------------
# 注意要控制的连续性协变量要放在~后的第一个
# 这是因为和回归模型的解读不同的地方是回归模型的系数是控制其他变量不变的时候，观察变量的变化
# 而p值控制以上变量，现变量和因变量关系的p值
# 比如该结果的解读就是
"(1)怀孕时间和出生体重相关
(2)控制怀孕时间，药物剂量和出生体重相关"
# 如果gesttime和dose反过来变成，dose + gesttime那结果就是
"(1)药物剂量和出生体重相关
(2)控制药物剂量,怀孕时间和出生体重相关"
#这种结果就没有意义了

##### 3.2单因素协方差分析检验正态性及方差齐性-----------------------------------
library(car)
qqPlot(fit, simulate=TRUE, main="Q-Q Plot")
bartlett.test(weight ~ gesttime, data=litter)
bartlett.test(weight ~ dose, data=litter)
##### 3.3单因素协方差分析回归斜率同质性检验-------------------------------------
library(multcomp)
fit2 <- aov(weight ~ gesttime*dose, data=litter)
summary(fit2)
# 公式和双因素方差分析相同但是变量的种类不同
# 交互项p不显著，说明满足回归斜率相同假设

##### 3.4单因素协方差分析结果可视化---------------------------------------------
###### 3.4.1R实战可视化代码-----------------------------------------------------
pred <- predict(fit)

library(ggplot2)
ggplot(data = cbind(litter, pred),
       aes(gesttime, weight)) + geom_point() +
  facet_grid(. ~ dose) + geom_line(aes(y=pred)) +
  labs(title="ANCOVA for weight by gesttime and dose") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position="none")
###### 3.4.2HH包结果可视化------------------------------------------------------
library(HH)
ancovaplot(y ~ x + group, data = df13_11)

####### 3.4.3ggplot2来画分面图--------------------------------------------------

theme_set(theme_minimal())

p1 <- ggplot(df13_11, aes(x=x,y=y))+
  geom_point(aes(color=group,shape=group))+
  geom_smooth(method = "lm",se=F,aes(color=group))+
  labs(y=NULL)
p1
p2 <- ggplot(df13_11, aes(x=x,y=y))+
  geom_point(aes(color=group,shape=group))+
  geom_smooth(method = "lm",se=F,aes(color=group))+
  facet_wrap(~group)
p2
library(patchwork)
p2 + p1 + plot_layout(guides = 'collect',widths = c(3, 1))



# 4.随机区组设计资料的方差分析和单因素协方差分析的区别--------------------------
"特征	          随机区组设计（RBD）	              |单因素协方差分析（ANCOVA）
控制变量的类型	分类变量（如区块、批次、受试者）	|连续变量（如基线值、年龄、体重）
变量角色	      区组变量是分类变量，作为随机效应	|协变量是连续变量，用于调整因变量
分析目标	      减少组内变异，提高处理效应的检测力|排除连续变量的影响，更纯净地分析处理效应
模型示例（R）	  aov(y ~ block + treatment)	      |aov(y ~ covariate + treatment)"

# 双因素方差分析和以上两种的区别就是，双因素方差分析还要分析因变量之间的交互作用
# 双因素方差分析的自变量都是因子型分类变量aov(y ~ treatment1*treatment2)

# 5.双因素方差分析--------------------------------------------------------------
data(ToothGrowth)
library(dplyr)
ToothGrowth$dose <- factor(ToothGrowth$dose)
stats <- ToothGrowth %>%
  group_by(supp, dose) %>%
  summarise(n=n(), mean=mean(len), sd=sd(len),
            ci = qt(0.975, df = n - 1) * sd / sqrt(n))
stats


fit <- aov(len ~ supp*dose, data=ToothGrowth)
summary(fit)
# 结果显示主效应以及因子之间的交互效应都非常显著
##### 5.1双因素方差分析结果可视化-----------------------------------------------
library(ggplot2)
pd <- position_dodge(0.2)
ggplot(stats, 
       aes(x = dose, y = mean, 
           group=supp, 
           color=supp, 
           linetype=supp)) +
  geom_point(size = 2, 
             position=pd) +
  geom_line(position=pd) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci), 
                width = .1, 
                position=pd) +
  theme_bw() + 
  scale_color_manual(values=c("blue", "red")) +
  labs(x="Dose",
       y="Mean Length",
       title="Mean Plot with 95% Confidence Interval") 

# 6.拉丁方设计方差分析(多个区组变量)--------------------------------------------
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

# 7.两阶段交叉设计资料方差分析--------------------------------------------------
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

