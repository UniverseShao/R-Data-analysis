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


