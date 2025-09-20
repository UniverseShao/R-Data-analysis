# 第一步：创建模拟数据----------------------------------------------------------
library(pROC)
# 设置随机数种子以保证结果可复现
set.seed(42)

# 创建一个包含100个病人的模拟数据集
df <- data.frame(
  # 结局变量：假设有30个病人死亡
  mortality = sample(c(rep(1, 30), rep(0, 70))),
  
  # 预测变量
  lar = rnorm(100, mean = 0.8, sd = 0.3),
  lactate = rnorm(100, mean = 2.0, sd = 0.8),
  sofa_score = sample(1:15, 100, replace = TRUE),
  age = rnorm(100, mean = 65, sd = 10)
)

# 让我们人为地让死亡组的指标值更高一些，使结果更明显
df$lar[df$mortality == 1] <- df$lar[df$mortality == 1] + 0.3
df$sofa_score[df$mortality == 1] <- df$sofa_score[df$mortality == 1] + 3

# 查看数据前几行
head(df)

# 情况一：为单个预测指标绘制ROC曲线(方法一)-------------------------------------
# 1. 使用 lar 直接对 mortality 进行预测，并创建ROC对象
# 公式语法是: roc(结局变量 ~ 预测变量, data = 数据集)
roc_lar <- roc(mortality ~ lar, data = df)

# 2. 计算并打印AUC面积
# auc() 函数可以提取AUC值，或者直接打印roc对象也会显示
auc_lar <- auc(roc_lar)
print(paste("LAR 单指标的 AUC:", round(auc_lar, 3)))

# 3. 绘制ROC曲线
plot(roc_lar, 
     main = "单个指标(LAR)的ROC曲线", 
     col = "#1c61b6", # 设置曲线颜色
     print.auc = TRUE, # 在图上直接打印AUC值
     legacy.axes = TRUE) # 使用传统的坐标轴样式

# 情况二：为预测模型绘制ROC曲线-------------------------------------------------
# 1. 建立一个逻辑回归模型
# 我们用 lar, sofa_score, 和 age 一起来预测 mortality
model <- glm(mortality ~ lar + sofa_score + age, 
             data = df, 
             family = "binomial") # family="binomial" 指明是逻辑回归
# 情况一：为单个预测指标绘制ROC曲线(方法二)-------------------------------------
res <- roc(df$mortality,df$lar,ci=T,auc=T)
res
plot(res,legacy.axes = TRUE)
plot(res,
     legacy.axes = TRUE,
     thresholds="best", # 标记出最优分类性能的截断值的点
     print.thres="best") # 在图中标注该点的阈值和性能指标
best_coords <- coords(res, "best", ret = c("threshold", "specificity", "sensitivity"))
rocobj <- plot.roc(df$mortality, df$lar,
                   main="Confidence intervals", 
                   percent=TRUE,ci=TRUE, #percent=TRUE	坐标轴以百分比显示（0-100%）
                   print.auc=TRUE
) 
ciobj <- ci.se(rocobj,
               specificities=seq(0, 100, 5)#计算灵敏度（Sensitivity）的置信区间
)

library(ROCR)
pred <- prediction(df$lar,df$mortality)
perf <- performance(pred, "tpr","fpr")
auc <- round(performance(pred, "auc")@y.values[[1]],digits = 4)
auc
plot(perf,lwd=2,col="blue")
abline(0,1,lty=2)
legend("bottomright", legend="AUC of LAR: 0.8367", col="blue", lwd=2,bty = "n")
perf <- performance(pred, "tpr","fpr")
plot(perf,
     avg="threshold",# 显示阈值平均效果
     spread.estimate="boxplot")# 用箱线图展示离散程度
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
# 2. 使用模型生成每个病人的死亡预测概率
# predict() 函数的 type="response" 参数会输出0到1之间的概率值
df$model_pred_prob <- predict(model, type = "response")

# 3. 使用模型的预测概率来创建ROC对象
# 这里的预测变量不再是单个指标，而是模型计算出的综合概率
roc_model <- roc(df$mortality ~ df$model_pred_prob)

# 4. 计算并打印模型的AUC面积
auc_model <- auc(roc_model)
print(paste("模型的 AUC:", round(auc_model, 3)))

# 5. 绘制模型的ROC曲线
plot(roc_model,
     main = "逻辑回归模型的ROC曲线",
     col = "#008600",
     print.auc = TRUE,
     legacy.axes = TRUE)
"
单个指标的ROC曲线: 评估的是这个指标自身的、独立的预测能力。
它回答的问题是：“如果我只看LAR这一个数值，我能多准确地猜出病人会不会在28天内死亡？”
模型的ROC曲线: 评估的是由多个变量组合而成的整个模型的综合预测能力。
它回答的问题是：“当我综合考虑了LAR、年龄、SOFA评分等多个因素后，我的整体预测能力有多强？”

"


