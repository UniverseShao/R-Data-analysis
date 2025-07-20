#倾向性评分（Propensity Score, PS）####
#一种控制混杂因素的统计学方法
#可以把基线控制在可比的水平

#倾向性评分最大的优势####
#将多个混杂因素的影响用(一个综合的值)来表示
#即倾向性评分值（Propensity Score, PS）
#从而降低协变量的维度
#尤其适用于协变量较多的情况

#倾向性评分的一般步骤####
#(1)估计 PS 值
#(2)利用 PS 值均衡协变量分布
#(3)均衡性检验及模型评价
#(4)处理效应估计

#PS 值的估计是以处理因素作为因变量，其他混杂因素作为自变量
#建立一个模型（可以是传统的回归模型，也可以是机器学习方法）
#估计每个研究对象接受处理因素的可能性
#估计 PS 值的方法有 (logistic 回归，
#Probit 回归、神经网络、支持向量机、分类与回归数、
#Boosting 算法、SuperLearner 等)

#倾向性评分有 (匹配) (分层) (协变量调整加权)三种方法####
#本文就先讲述(匹配)
#协变量调整又可以称为倾向性评分回归、倾向性评分矫正

#倾向性评分匹配####

#准备数据####

set.seed(2020)
x.Gender <- rep(0:1, c(400,600)) # 400女 600男
x.Age <- round(abs(rnorm(1000, mean=45, sd=15)))
# rnorm() 生成正态分布随机数 数量n, 均值mean, 标准差sd
# abs() 对 rnorm() 生成的随机数取绝对值（避免负年龄）
# round() 将年龄四舍五入到整数

# 对于这个数据来说，实际PS（tps）是可以计算出来的，如果这里不理解，也问题不大！
z <- (x.Age - 45) / 15 - (x.Age-45) ^ 2 / 100 + 2 * x.Gender
tps <- exp(z) / (1+exp(z))

#z <- (x.Age - 45) / 15 - (x.Age-45) ^ 2 / 100 + 2 * x.Gender####
#这行是假设得真实得模型是这样得
#所以log(p(吸烟)/(1-p(吸烟)))=(x.Age - 45) / 15 - (x.Age-45) ^ 2 / 100 + 2 * x.Gender
#log(p(吸烟)/(1-p(吸烟)))=z
#换算就是p(吸烟)=exp(z) / (1+exp(z))
#即tps <- exp(z) / (1+exp(z))

Smoke <- as.numeric(runif(1000) < tps)
z.y <- x.Gender + 0.3*x.Age + 5*Smoke - 20
y <- exp(z.y) / (1+exp(z.y))
CVD <- as.numeric(runif(1000) < y)
x.Age.mask <- rbinom(1000, 1, 0.2) # 随机产生几个缺失值
x.Age <- ifelse(x.Age.mask==1, NA, x.Age)
# 原始数据长这样：
data <- data.frame(x.Age, x.Gender, Smoke, CVD)
head(data)
#tableone计算SMD####
library(tableone)
table2 <- CreateTableOne(vars = c('x.Age', 'x.Gender', 'CVD'),
                         data = data,
                         factorVars = c('x.Gender', 'CVD'),
                         strata = 'Smoke',
                         smd=TRUE)
table2 <- print(table2,smd=TRUE,
                showAllLevels = TRUE,
                noSpaces = TRUE,
                printToggle = FALSE)#不直接打印，返回结果赋值给table2
#这串代码存在的意义
#CreateTableOne() 生成的是一个复杂的列表对象（TableOne 类）
#不是直接可读的表格
#CreateTableOne(smd=TRUE)
#计算 SMD 并存储在对象中
#print(smd=TRUE)
#显示 SMD 列
table2

#结果显示吸烟和不吸烟组之间（年龄）分布差异显著
#结果显示吸烟和不吸烟组之间（性别）分布差异显著
#结果显示吸烟和不吸烟组之间（cvd患病率）差异显著


#如果要探究吸烟对cvd患病率得影响
#在组间性别和年龄差异这么显著得情况下
#需要消除这些协变量的基线差异

#matchIt包进行PSM####

#distance:指定PS的计算方法，默认是logit，即logistic回归
#distance.options:当你选择好了方法之后，不同的方法会有不同的额外选项

library(MatchIt)
# 这里为了方便演示直接删掉了缺失值
data.complete <- na.omit(data)
# 因变量是处理因素，自变量是需要平衡的协变量
m.out <- matchit(Smoke~x.Age+x.Gender,
                 data = data.complete,
                 distance = "logit" # 选择logistic回归
)
m.out

#获得算法估计的PS值####
eps <- m.out$distance
length(eps)#检查 eps 的长度（即样本量）
#输出 831：表示数据中共有 831 个个体 的倾向得分被计算
head(eps)#显示 eps 的前6个值


# 1         2         3         4         5         6 
# 0.2583040 0.2545807 0.1847661 0.1818430 0.1200378 0.2774451 

#倾向得分的含义：####
#ID=1的个体有 25.8%的概率 属于处理组（如吸烟），ID=5的个体仅有 12.0%的概率
#值越接近1，越可能属于处理组；越接近0，越可能属于对照组

#后续匹配的用途####
#得分将用于 匹配处理组和对照组的个体（如1:1最近邻匹配）
#匹配时，会优先选择 倾向得分最接近的个体对

#逻辑回归 是直接建模二分类变量（如吸烟与否）概率的经典方法，其输出值（预测概率）即为PS的估计值

#画一个tps和估计ps的散点图来检验拟合####
library(ggplot2)
# 去掉缺失值
tps.comp <- tps[complete.cases(data)]
Smoke.comp <- as.factor(Smoke[complete.cases(data)])
df <- data.frame(True=tps.comp, Estimated=eps, Smoke=Smoke.comp)
#complete.cases(data)
#返回一个逻辑向量，标记数据框 data 中 没有缺失值的行
#tps.comp
#从真实倾向得分向量 tps 中提取 对应无缺失值行的值
#Smoke.comp
#从吸烟状态向量 Smoke 中提取无缺失值行的值，并转换为因子（as.factor）
#便于后续按吸烟状态分组着色
ggplot(df, aes(x=True, y=Estimated, colour=Smoke)) +
  geom_point() +
  geom_abline(intercept=0,slope=1, colour="#990000", linetype="dashed") +
  expand_limits(x=c(0,1),y=c(0,1))
#intercept=0：截距为0
#slope=1：斜率为1

#完善拟合模型####
# 对x.Age平方
m.out <- matchit(Smoke~I(x.Age^2)+x.Age +x.Gender,
                 data=data.complete)
eps <- m.out$distance
tps.comp <- tps[complete.cases(data)]
Smoke.comp <- as.factor(Smoke[complete.cases(data)])
df <- data.frame(True=tps.comp, Estimated=eps, Smoke=Smoke.comp)
ggplot(df, aes(x=True, y=Estimated, colour=Smoke)) +
  geom_point() +
  geom_abline(intercept=0,slope=1, colour="#990000", linetype="dashed") +
  expand_limits(x=c(0,1),y=c(0,1))

#完全可以用glm自己计算ps####
tmp <- glm(Smoke~I(x.Age^2)+x.Age +x.Gender, data=data.complete,
           family = binomial())
tmp.df <- data.frame(estimated = tmp$fitted.values,
                     true = tps.comp,
                     Smoke=Smoke.comp)
#fitted.values指的是模型中得预测值
ggplot(tmp.df, aes(true, estimated))+
  geom_point(aes(color=Smoke))+
  geom_abline(intercept=0,slope=1, colour="#990000", linetype="dashed") +
  expand_limits(x=c(0,1),y=c(0,1))

#(分类和回归树)及(神经网络)方法####
# cart
m.out <- matchit(Smoke~x.Age+x.Gender,
                 data=data.complete,
                 distance='rpart')
m.out

# nnet
m.out <- matchit(Smoke~x.Age+x.Gender,
                 data=data.complete,
                 distance='nnet',
                 distance.options=list(size=16))
m.out

#使用随机森林计算PS####
# 使用随机森林构建模型
library(randomForest)
data.complete$Smoke <- factor(data.complete$Smoke)
rf.out <- randomForest(Smoke~x.Age+x.Gender, data=data.complete)
rf.out
#提取预测类别为1（有CVD）的概率
eps <- rf.out$votes[,2] # Estimated PS
matchit(formula=Smoke~x.Age+x.Gender,
        data=data.complete,
        distance=eps, # 自己估计的eps
        method='nearest',
        replace=TRUE,
        discard='both',
        ratio=2)
#主要匹配方法选择####
# (1)匹配方法的选择（method）
# (2)采样手段（有无放回）
# (3)相似度的度量（卡钳值或其他）
# (4)匹配比例（1:1或1：多）

#method####
# (1)默认的匹配方法是最近邻匹配nearest，其他方法还有
# (2)"exact" (exact matching),
# (3)"full" (optimal full matching),
# (4)"optimal" (optimal pair matching),
# (5)"cardinality"(cardinality and template matching),
# (6)"subclass" (subclassification),
# (7)"genetic" (genetic matching),
# (8)"cem" (coarsened exact matching)

# caliper:卡钳值####
# 也就是配对标准，两组的概率值（PS）差距在这个标准内才会配对
# 这里的卡钳值是PS标准差的倍数，默认是不设置卡钳值
# std.caliper参数默认是TRUE，如果设置FALSE，你设置的卡钳值就直接是PS的倍数

# replace:能否重复匹配####
# 默认是FALSE
# 意思是假如干预组的1号匹配到了对照组的A，那A就不能再和其他的干预组进行匹配了

# ratio:设置匹配比例####
# 干预组:对照组到底是1比几，默认为1:1
# ratio=2即是干预组：对照组是1:2。所以一般要求数据的对照组数量多于干预组才行
# 如果对照组比干预组多出很多，完全可以设置1:n进行匹配，这样还能损失更少的样本信息
# 但是一般也不会超过1:4


# reestimate:是否丢弃没匹配上的样本####
# 如果是TRUE，丢掉没匹配上的样本，PS会使用剩下的样本重新计算PS
# 如果是FALSE或者不写就不会重新计算PS

#有放回的，1:2的，最近邻匹配####
m.out <- matchit(Smoke~x.Age+x.Gender,
                 data=data.complete,
                 distance='logit',
                 method='nearest',
                 replace=TRUE,
                 ratio=2)
m.out

#match.matrix获取配好的对子####

head(m.out$match.matrix)

#m.out$discarded查看某个样本是否被丢弃####

table(m.out$discarded)


#匹配后数据的平衡性检验####

m.out <- matchit(Smoke~x.Age+x.Gender,
                 data=data.complete,
                 distance='logit',
                 method='nearest',
                 replace=FALSE,
                 ratio = 1)

#summary(m.out,standardize = TRUE)####
summary(m.out,standardize = TRUE)
#standardize = TRUE 时，summary() 会显示 标准化后的均值差（Standardized Mean Difference, SMD） 
#和其他标准化平衡性指标（如方差比、eCDF 等）


#SMD的全称与含义####
#衡量处理组（Treated）和对照组（Control）之间协变量（如年龄、性别）差异的标准化指标
#计算公式为
#SMD = (Mean Treated−Mean Control)/SD Pooled
#SMD < 0.1 表示匹配后两组差异可忽略
#与P值不同，SMD 不受样本量影响，更适合评估匹配效果

#Var. Ratio####
#方差比（两组方差的比值，理想值接近1）
#VR>2.0或者VR<0.5说明很不均衡（越接近1越均衡）


#tableone的局限性####
#具体原因大家自己读文献
#Zhang Z, Kim HJ, Lonjon G, et al. Balance diagnostics after propensity score matching. Ann Transl Med 2019;7:16.

#cobalt包进行平衡性检验####
library(cobalt)
# m.threshold表示SMD的阈值，小于这个阈值的协变量是平衡的
bal.tab(m.out, m.threshold = 0.1, un = TRUE)
# v.threshold表示VR
bal.tab(m.out, v.threshold = 2)

#统计检验衡量均衡性####
mdata <- match.data(m.out)
head(mdata)
#t检验####
t.test(x.Age ~ Smoke, data = mdata)
#卡方检验####
chisq.test(mdata$x.Gender, mdata$Smoke,correct = F)


#不平衡怎么办####
#换算法，换公式（增加二次项、交互项等）####
# 增加二次项，结果依然不平衡
m.out <- matchit(Smoke~I(x.Age^2)+x.Age+x.Gender,
                 data=data.complete,
                 distance='logit',
                 method='nearest',
                 replace=FALSE,
                 ratio=1)
m.out
summary(m.out)

# 还是不平衡
m.out <- matchit(Smoke~I(x.Age^2)+x.Age+x.Gender,
                 data=data.complete,
                 distance='logit',
                 method='genetic',
                 pop.size=100)
bal.tab(m.out, m.threshold=0.1)

#精确匹配####
m.out <- matchit(Smoke~I(x.Age^2)+x.Age+x.Gender,
                 data=data.complete,
                 distance='logit',
                 method='nearest',
                 exact = c('x.Gender','x.Age'), # 精准！
                 replace=FALSE,
                 ratio=1)
bal.tab(m.out, m.threshold=0.1)
# 使用 match.data() 获取匹配后的数据--------------------------------------------
matched_data <- match.data(m.out)
"
1. 权重（weights）的含义
(1) 默认情况（replace=FALSE）
匹配样本：权重为 1。
未匹配样本：权重为 0（默认被丢弃，除非设置 include.uncertain=TRUE）。
(2) 允许重复匹配时（replace=TRUE）
当设置 replace=TRUE 时，同一个对照样本可能被多次匹配到不同的处理样本（如 1 个对照匹配给 3 个处理）。
此时 weights 表示：
处理组样本：权重为 1（每个处理样本唯一）。
对照组样本：权重为 k/n，其中：
k = 该对照样本被匹配的次数。
n = 总匹配对数（如 ratio 指定的比例）。
结果：对照组权重可能小于 1（如 0.5）或大于 1（如 2.0）

"
# 使用 get_matches() 获取配对信息（推荐）(有配对列)-----------------------------
library(MatchIt)
matched_pairs <- get_matches(m.out)
"这会返回一个数据框，包含：
原始变量（Smoke, x.Age, x.Gender）。
**subclass 列**：表示配对组的ID（即你需要的“对子列”）。
**id 列**：原始数据的行号。
其他匹配相关的列（如距离、权重等）。"
# 有配对列后续的分析无论是进行逻辑回归还是cox回归都会方便很多

