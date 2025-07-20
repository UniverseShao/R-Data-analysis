#是金子总会发光
#鸭子是不会变成天鹅的

#6.1 加载R包和数据####----------------------------------------------------------------

library(AppliedPredictiveModeling)
library(caret)

data("segmentationOriginal")

segData <- subset(segmentationOriginal, Case == "Train")
#使用subset()筛选出Case列为"Train"的样本(训练集)
cellID <- segData$Cell
clsss <- segData$Class
case <- segData$Case
segData <- segData[ ,  -(1:3)]
statusColNum <- grep("Status", names(segData))
#grep() 是R中用于模式匹配的函数
#返回值：匹配元素的索引位置
statusColNum
segData <- segData[ , -statusColNum]
#将数据中包含status名的列都删除
str(segData)

#6.2 中心化和标准化####----------------------------------------------------------------
#中心化是将所有变量减去其均值，其结果是变换后的变量均值为0
#标准化是将每个变量除以其自身的标准差，标准化迫使变量的标准差为1
#scale()函数可实现中心化和标准化

#6.3 偏度问题####----------------------------------------------------------------
#无偏分布类似我们常说的正态分布
#有偏分布又分为右偏和左偏，分别类似正偏态分布和负偏态分布

#一个判断数据有偏的黄金标准：如果最大值与最小值的比例超过20，那么我们认为数据有偏

#可以通过计算偏度统计量来衡量偏度
#如果预测变量分布是大致对称的，那么偏度将接近于0
#右偏分布偏度大于0，越大说明偏的越厉害
#左偏分布偏度小于0，越小说明偏的越厉害

#e1071包查看变量的偏度####----------------------------------------------------------------
library(e1071)
# 查看偏度
skewness(segData$AngleCh1)
## [1] -0.02426252
## [1] -0.02426252

# 查看每一列的偏度
skewValues <- apply(segData, 2, skewness)
head(skewValues)
##    AngleCh1     AreaCh1 AvgIntenCh1 AvgIntenCh2 AvgIntenCh3 AvgIntenCh4 
## -0.02426252  3.52510745  2.95918524  0.84816033  2.20234214  1.90047128

#psych包查看偏度####----------------------------------------------------------------
psych::skew(segData$AngleCh1) # 偏度
## [1] -0.02426252

psych::kurtosi(segData$AngleCh1) # 峰度
## [1] -0.8594789
#三种典型峰态

#尖峰态（Leptokurtic）
#超额峰度 > 0
#特征：尖峰、厚尾
#例子：金融收益率数据（极端值更多）

#常峰态（Mesokurtic）
#超额峰度 ≈ 0
#特征：与正态分布相似
#例子：人类身高数据

#低峰态（Platykurtic）：
#超额峰度 < 0
#特征：扁平、薄尾
#例子：均匀分布数据

#取对数(log)，平方根，倒数，Box&Cox法等一定程度解决偏度####----------------------------------------------------------------
#BoxCox变换####----------------------------------------------------------------
# 准备对数据进行BoxCox变换
Ch1AreaTrans <- BoxCoxTrans(segData$AreaCh1)
Ch1AreaTrans
# 进行变换
AreaCh1_transed <- predict(Ch1AreaTrans, segData$AreaCh1)
# 查看变换前、后的数据
head(segData$AreaCh1)
head(AreaCh1_transed)
# 画图看变换前后
opar <- par(mfrow=c(1,2))
#mfrow=c(x,y)就是指图呈x行x列来排列
hist(segData$AreaCh1)
hist(AreaCh1_transed)
par(opar)#撤销之前通过 par() 所做的所有图形参数修改
#比较变换前后的偏度####----------------------------------------------------------------
psych::skew(segData$AreaCh1)
psych::skew(AreaCh1_transed)
#偏度正负指的是尾巴的方向，不是数据堆积的方向
#右偏就指的是均值>> 中位数 左偏就是均值<< 中位数


#6.4 解决离群值（空间表示变换）####----------------------------------------------------------------

#通常我们会选择直接删除离群值
#有的离群值是非常有意义的，这样的离群值不能直接删除

#有的离群值可能是数据录入时不小心输错了，比如错把收缩压132mmHg录成了 -132mmHg，只需要改正即可
#在样本量较小时，不宜直接删除离群值
#有些离群值可能来自一个特殊的子集，只是这个子集才刚开始被收集到

#有些模型对离群值很敏感，比如线性模型
#空间表示变换####----------------------------------------------------------------
#在进行空间表示变换前，最好先进行中心化和标准化

# 变换前的图形
data(mdrr)
transparentTheme(trans = .4)

plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")])) 
xyplot(nC ~ X4v,
       data = plotSubset,
       groups = mdrrClass, 
       auto.key = list(columns = 2))#自动生成图例，并分 2 列显示


# 变换后的图形
transformed <- spatialSign(plotSubset)
transformed <- as.data.frame(transformed)
xyplot(nC ~ X4v, 
       data = transformed, 
       groups = mdrrClass, 
       auto.key = list(columns = 2)) 
#后续拟合模型可以直接用转换后的数据直接将拟合
#但是预测新数据，同样需要先对test数据进行空间表示变换


#6.5 降维和特征提取####----------------------------------------------------------------


# 主成分分析，可参考我之前的推文
pr <- prcomp(~ AvgIntenCh1 + EntropyIntenCh1, 
             data = segData, 
             scale. = TRUE)

# 可视化前后图形
library(ggplot2)

p1 <- ggplot(segData, aes(AvgIntenCh1,EntropyIntenCh1))+
  geom_point()+
  labs(x="Channel 1 Fiber Width",y="Intensity Entropy Channel 1")+
  theme_bw()
p2 <- ggplot(as.data.frame(pr$x), aes(PC1,PC2))+
  geom_point()+
  theme_bw()
cowplot::plot_grid(p1,p2)

#6.6 处理缺失值####----------------------------------------------------------------

#完全随机缺失####----------------------------------------------------------------
#若某变量的缺失数据与其他任何观测或未观测变量都不相关，则数据 为完全随机缺失（MCAR）
#随机缺失
#若某变量上的缺失数据与其他观测变量相关，与它自己的未观测值不相关， 则数据为随机缺失（MAR）
#非随机缺失
#若缺失数据不属于MCAR和MAR，则数据为非随机缺失（NMAR）

#具体处理缺失值的方法见笔记文件夹####

#6.7 过滤####----------------------------------------------------------------
#这里的过滤和解决共线性，其实部分属于特征选择的范围
#自变量选择问题
#冗余的变量通常增加了模型的复杂度而非信息量

#主要是过滤两种变量：
#(近)零方差变量（其实就是重复的值过多）和
#高度相关变量

#如果一个变量只有1个值，那么这个变量的方差为0
#如果一个变量只有少量不重复的取值，这种变量称为近零方差变量
#这2种变量包含的信息太少了，应当过滤；

#检测近零方差变量的准则是：
#不重复取值的数目与样本量的比值低（比如10%）
#最高频数和次高频数的比值高（比如20%）

#移除共线变量的方法如下：
#计算预测变量的相关系数矩阵
#找出相关系数绝对值最大的那对预测变量（记为变量A和B）
#分别计算A和B和其他预测变量的相关系数
#如果A的平均相关系数更大，移除A，否则移除B
#重复步骤2-4，直至所有相关系数的绝对值都低于设定的阈值


#先处理零方差变量和近零方差变量####----------------------------------------------------------------
data(mdrr)
table(mdrrDescr$nR11) # 大部分值都是0
sd(mdrrDescr$nR11)^2 # 方差很小！
#使用nearZeroVar()找出零方差和近零方差变量
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
#saveMetrics = TRUE：返回详细的指标数据框而非简单的列索引
#如果没有saveMetrics = TRUE就是返回一个列索引
nzv[nzv$nzv,][1:10,]
#因为nzv$nzv是一列逻辑向量，将逻辑向量放在行上，会直接输出为TRUE的行
#去掉近零方差变量：
dim(mdrrDescr)
## [1] 528 342
#这是指mdrrDescr有528行342列
nzv <- nearZeroVar(mdrrDescr)
#这里没有saveMetrics = TRUE，所以返回的nzv代表的是列索引
filteredDescr <- mdrrDescr[, -nzv]
#将列索引nzv代表的列都删除，即删除近零方差变量
dim(filteredDescr)
## [1] 528 297
#现在就是只有297列了

#处理高度相关的变量####----------------------------------------------------------------
# 相关系数矩阵
correlations <- cor(segData)
dim(correlations)
## [1] 58 58

# 可视化相关系数矩阵，中间几个颜色深的就是高度相关的变量
library(corrplot)
corrplot(correlations, order = "hclust",tl.col = "black")

# 阈值设为0.75
highCorr <- findCorrelation(correlations, cutoff = 0.75)
length(highCorr)
## [1] 32
head(highCorr)
## [1] 23 40 43 36  7 15

# 去掉高度相关的变量
filteredSegData <- segData[, -highCorr]


#但是这其实没有考虑到显著性检验这个问题
#同时考虑到显著性检验处理高度相关向量####----------------------------------------------------------------

# 加载必要的包
library(caret)
library(psych)
library(corrplot)
## 1. 计算相关系数矩阵及显著性p值矩阵
# 使用psych包的corr.test()同时计算r值和p值
cor_results <- corr.test(segData, adjust = "none")
correlations <- cor_results$r  # 相关系数矩阵
p_values <- cor_results$p      # 未校正的p值矩阵
## 2. 多重检验校正（控制假阳性）
# 使用Benjamini-Hochberg方法校正p值
p_adjusted <- matrix(p.adjust(p_values, method = "BH"), 
                     nrow = nrow(p_values),
                     dimnames = dimnames(p_values))

## 3. 定义综合筛选条件
# 条件1：相关系数绝对值 > 0.75
# 条件2：校正后的p值 < 0.05
significant_cor <- (abs(correlations) > 0.75) & (p_adjusted < 0.05)
## 4. 找出需要删除的变量（改进版findCorrelation）
findSignificantCorrelation <- function(cor_mat, sig_mat) {
  var_indices <- 1:ncol(cor_mat)
  to_remove <- integer(0)
  
  # 创建副本避免修改原矩阵
  current_sig <- sig_mat
  diag(current_sig) <- NA  # 忽略对角线
  
  while(any(current_sig, na.rm = TRUE)) {
    # 计算每个变量的显著相关计数
    cor_counts <- colSums(current_sig, na.rm = TRUE)
    
    # 找出与其他变量有最多显著高相关的变量
    max_count <- max(cor_counts)
    if(max_count == 0) break
    
    max_index <- which.max(cor_counts)
    
    to_remove <- c(to_remove, var_indices[max_index])
    
    # 更新矩阵（移除行列）
    current_sig <- current_sig[-max_index, -max_index, drop = FALSE]
    var_indices <- var_indices[-max_index]
  }
  
  return(to_remove)
}
## 5. 移除高度相关且统计显著的变量
filteredSegData <- segData[, -highCorr, drop = FALSE]
dim(filteredSegData)
## 6. 可视化验证
corrplot(cor(filteredSegData), 
         order = "hclust",
         tl.col = "black",
         title = "Filtered Correlation Matrix (|r|>0.75 & p<0.05)",
         mar = c(0,0,1,0))

# 查看被移除的变量名
colnames(segData)[highCorr]


#6.8 共线性----------------------------------------------------------------
ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)

ltfrDesign
#findLinearCombos()去除变量解决共线性问题####----------------------------------------------------------------
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo
# 去除第3列和第6列
ltfrDesign <- ltfrDesign[, -comboInfo$remove]
ltfrDesign

#vif()函数对拟合后的模型检查是否存在共线性问题---------------------------------------------------------------
#和findLinearCombos()的区别是，vif()是查模型，后者是查原矩阵
#VIF > 5 表示中度共线性，>10 表示严重共线性
#A和B存在共线性的时候，到底去除A还是去除B呢
#去除VIF高的，保留VIF低的



#6.9 构建虚拟变量####----------------------------------------------------------------
data("cars", package = "caret")
head(cars)
type <- c("convertible", "coupe", "hatchback", "sedan", "wagon")
cars$Type <- factor(apply(cars[, 14:18], 1, function(x) type[which(x == 1)]))

carSubset <- cars[sample(1:nrow(cars), 20), c(1, 2, 19)]
# 上面是数据生成过程，不重要，记住下面这个数据的样子即可！！
head(carSubset)
levels(carSubset$Type) # Type是一个因子型变量
impleMod <- dummyVars(~Mileage + Type, # 用mileage和Type对价格进行预测
                      data = carSubset,
                      levelsOnly = TRUE) # 从列名中移除因子变量的名称
simpleMod
predict(simpleMod, head(carSubset))
withInteraction <- dummyVars(~Mileage + Type + Mileage:Type,
                             data = carSubset,
                             levelsOnly = TRUE)
withInteraction
predict(withInteraction, head(carSubset))






