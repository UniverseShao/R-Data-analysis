# 1.AUC的含义和意义-------------------------------------------------------------
#AUC就是ROC的曲线下面积
#AUC范围	模型评价	应用建议
# 0.5-0.6	无用	拒绝使用
# 0.6-0.7	较弱	需改进
# 0.7-0.8	可用	辅助决策
# 0.8-0.9	优秀	临床采纳
# >0.9	极强	金标准
##### 1.1ROC曲线的生成过程------------------------------------------------------
#以 aSAH$s100b作为预测变量绘制ROC曲线为例
#一、函数如何确定"预测死亡"的截断值？
#1. 自动扫描所有可能的临界点
#假设aSAH$s100b有100个不同值，函数会：

#将每个独特值都作为潜在截断值(c)进行测试

#例如：c=0.5μg/L, 1.0μg/L, 1.5μg/L...
#对于每一个截断值c：都可以算出灵敏度还有1-特异度，那么这时候就得出了真阳性率（tpr）还有假阳性率（fpr）
# 排序所有s100b值：
#[0.8, 1.2, 1.5, 2.1]

#遍历每个可能的c值：

#截断值c	分类结果	TPR	FPR
#c≤0.8	全预测为死亡	1.0	1.0
#0.8<c≤1.2	B预测为存活	1.0	0.5
#1.2<c≤1.5	B,D预测为存活	0.5	0.5
#c>1.5	仅A预测为死亡	0.5	0.0
#绘制ROC曲线：
#连接点(1.0,1.0)→(0.5,1.0)→(0.5,0.5)→(0.0,0.5)


# 2.方法1(pROC包)---------------------------------------------------------------
##### 2.1单调画法---------------------------------------------------------------
#使用pROC包，不过使用这个包需要注意，一定要指定direction
library(pROC)
data(aSAH)
dim(aSAH)
str(aSAH)

res <- roc(aSAH$outcome,aSAH$s100b,ci=T,auc=T)
res
plot(res,legacy.axes = TRUE)
##### 2.2显示最佳截断点---------------------------------------------------------
# 显示最佳截点，比如最优分类性能的截断值的点
plot(res,
     legacy.axes = TRUE,
     thresholds="best", # 标记出最优分类性能的截断值的点
     print.thres="best") # 在图中标注该点的阈值和性能指标
best_coords <- coords(res, "best", ret = c("threshold", "specificity", "sensitivity"))
#标注出来的点显示的0.205(0.806，0.634)指的是threshold是0.205 specificity是0.806
##### 2.3添加置信区间-----------------------------------------------------------
# 显示AUC的可信区间
rocobj <- plot.roc(aSAH$outcome, aSAH$s100b,
                   main="Confidence intervals", 
                   percent=TRUE,ci=TRUE, #percent=TRUE	坐标轴以百分比显示（0-100%）
                   print.auc=TRUE
) 
ciobj <- ci.se(rocobj,
               specificities=seq(0, 100, 5)#计算灵敏度（Sensitivity）的置信区间
)
#在**特异度（Specificity）**的0%到100%范围内，每5%间隔计算一次对应的:
#灵敏度点估计值
# 95%置信区间（默认）
plot(ciobj, type="shape", col="#1c61b6AA")
plot(ci(rocobj, of="thresholds", thresholds="best")) 
#of="thresholds"：指定计算阈值相关置信区间

##### 2.3多条ROC曲线画在一起----------------------------------------------------
rocobj1 <- plot.roc(aSAH$outcome, aSAH$s100,percent=TRUE, col="#1c61b6")
rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="#008600")
legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)
#图例显示在右下角

##### 2.4添加P值----------------------------------------------------------------
testobj <- roc.test(rocobj1, rocobj2)

text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))

# 3.方法2(ROCR包)---------------------------------------------------------------
library(ROCR)
pred <- prediction(aSAH$s100b,aSAH$outcome)
perf <- performance(pred, "tpr","fpr")
auc <- round(performance(pred, "auc")@y.values[[1]],digits = 4)
auc
plot(perf,lwd=2,col="tomato")
abline(0,1,lty=2)
legend("bottomright", legend="AUC of s100b: 0.7314", col="tomato", lwd=2,bty = "n")

##### 3.1添加箱线图-------------------------------------------------------------
perf <- performance(pred, "tpr","fpr")
plot(perf,
     avg="threshold",# 显示阈值平均效果
     spread.estimate="boxplot")# 用箱线图展示离散程度

##### 3.2绘制PR曲线-------------------------------------------------------------

perf <- performance(pred, "prec", "rec")
plot(perf,
     avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main= "Precision-Recall plot")
plot(perf,
     lty=3,
     col="grey78",
     add=TRUE)
#rec也就是召回率也就是recall，也就是真阳性率，和灵敏度一个意思
##### 3.3特异度为横坐标，灵敏度为纵坐标-----------------------------------------

perf <- performance(pred, "sens", "spec")
plot(perf,
     avg= "threshold",
     colorize=TRUE,
     lwd= 3,
     main="Sensitivity/Specificity plots")
plot(perf,
     lty=3,
     col="grey78",
     add=TRUE)

# 4.真阳性率的多种称呼----------------------------------------------------------
# 召回率recall
# 真阳性率
# 灵敏度


# 5.方法3(tidymodels)-----------------------------------------------------------

suppressPackageStartupMessages(library(tidymodels))
aSAH %>% roc_auc(outcome, s100b,event_level="second")
aSAH %>% roc_curve(outcome, s100b,event_level="second") %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(size=1.2,color="firebrick") +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()
library(pkgsearch) 

rocPkg <-  pkg_search(query="ROC",size=200)

rocPkgShort <- rocPkg %>% 
  filter(maintainer_name != "ORPHANED") %>%
  select(score, package, downloads_last_month) %>%
  arrange(desc(downloads_last_month))
head(rocPkgShort,20)

