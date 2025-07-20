#哑变量设置---------------------------------------------------------------------


#以earth包中的etitanic数据集为例
#包含两个分类变量，pclass：乘客等级，1st, 2nd, 3rd；sex：乘客性别，male和female
#survived是结果变量

library(earth)
data(etitanic)
str(etitanic)
psych::headTail(etitanic)
# 直接使用model.matrix()，会自动把因子型变量进行哑变量设置
# pclass, sex变成了哑变量
head(model.matrix(survived ~ ., data = etitanic))
# 使用dummyVars()，会自动把因子型变量进行哑变量设置
# pclass, sex变成了哑变量
library(caret)
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))
#caret包中部分数据预处理的使用方法，在使用了相关的数据预处理函数后，一定要使用predict()才可以把数据预处理应用于数据

#零方差和近零方差变量处理------------------------------------------------------
data(mdrr)
table(mdrrDescr$nR11) # 大部分值都是0
## 
##   0   1   2 
## 501   4  23

sd(mdrrDescr$nR11)^2 # 方差很小！
## [1] 0.1731787

#使用nearZeroVar()找出零方差和近零方差变量
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)#nzv是列索引
nzv[nzv$nzv,][1:10,]
#这时候可以直接去除nzv列也可以再设置阈值
nzv <- nearZeroVar(mdrrDescr,freqCut = 95/5,uniqueCut = 10)
#freqCut = 95/5最常见值出现 95 次，第二常见值出现 5 次，则频数比 = 95/5 = 19
#若实际频数比 > 19，该变量会被标记为低方差
#uniqueCut = 10某变量的唯一值数量 < 总样本数的 10%，则被标记
filteredDescr <- mdrrDescr[, -nzv] # 去掉近零方差变量
dim(mdrrDescr) # 原数据有342列
dim(filteredDescr) # 还剩297列

#识别高度相关的变量------------------------------------------------------------------------
# 计算相关系数
descrCor <-  cor(filteredDescr)
# 这个数据集里相关系数最大是1！
summary(descrCor[upper.tri(descrCor)])
# 阈值设定为0.75
highlyCorDescr <- findCorrelation(descrCor, cutoff = 0.75)#绝对值0.75
# 相关系数绝对值大于0.75的变量不要了
filteredDescr <- filteredDescr[,-highlyCorDescr]
# 再次查看相关系数
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])


#共线性----------------------------------------------------------------------------
ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)

ltfrDesign
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
# 去除第3列和第6列
ltfrDesign[, -comboInfo$remove]

#中心化和标准化------------------------------------------------------------------------------------
set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

# 中心化，标准化
preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)


#变量转化--------------------------------------------------------------------------
# 没变换之前是这样的
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)

plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")])) 
xyplot(nC ~ X4v,
       data = plotSubset,
       groups = mdrrClass, 
       auto.key = list(columns = 2)) 
# After the spatial sign:
transformed <- spatialSign(plotSubset)
transformed <- as.data.frame(transformed)
xyplot(nC ~ X4v, 
       data = transformed, 
       groups = mdrrClass, 
       auto.key = list(columns = 2)) 
