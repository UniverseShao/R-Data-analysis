"综合判别改善指数,值越大，表明模型预测能力越好，如果IDI为负值，
那说明还不如原来的模型好"
"综合判别改善指数（Integrated Discrimination Index，IDI），
也适用于评价不同模型优劣的,比起NRI，IDI能够从整体角度对模型进行评价，
和NRI一起使用效果更佳."

# 28.1 二分类模型的IDI(PredictABEL)---------------------------------------------
rm(list = ls())
library(survival)

# 只使用部分数据
dat <- pbc[1:312,] 
dat <- dat[ dat$time > 2000 | (dat$time < 2000 & dat$status == 2), ]

str(dat) # 数据长这样
dim(dat) # 232 20
# 定义结局事件，0是存活，1是死亡
event <- ifelse(dat$time < 2000 & dat$status == 2, 1, 0)

# 建立2个模型
mstd <- glm(event ~ age + bili + albumin, family = binomial(), data = dat, x=TRUE)
mnew <- glm(event ~ age + bili + albumin + protime, family = binomial(), data = dat, x=TRUE)

# 取出模型预测概率
p.std <- mstd$fitted.values
p.new <- mnew$fitted.values

library(PredictABEL)  

dat$event <- event

reclassification(data = dat,
                 cOutcome = 21, # 结果变量在哪一列
                 predrisk1 = p.std,
                 predrisk2 = p.new,
                 cutoff = c(0,0.3,0.7,1)
)

# 随机建立一个测试集
# 取前100行作为测试集，这个方法是不正规的哈
testset <- dat[1:100,]

# 计算测试集的概率
p.std_test <- predict(mstd, newdata = testset,type = "response")
p.new_test <- predict(mnew, newdata = testset,type = "response")
reclassification(data = testset,
                 cOutcome = 21, # 结果变量在哪一列
                 predrisk1 = p.std_test,
                 predrisk2 = p.new_test,
                 cutoff = c(0,0.3,0.7,1)
)
"分类NRI（Categorical NRI）
适用于风险被划分为 离散类别（如低、中、高风险）的情况
NRI(Categorical) = 0.0019 [ -0.0551 - 0.0589 ] ; p=0.948
值接近0且P值>0.05，说明新模型在分类风险上 无显著改善"

"连续NRI（Continuous NRI）
适用于风险以 连续概率（如0%~100%）表示的情况
NRI(Continuous) = 0.0391 [ -0.2238 - 0.3021 ] ; p=0.770
值较小且P值>0.05，说明新模型在连续概率调整上 无显著优势"

# 28.2 生存资料的IDI------------------------------------------------------------
rm(list = ls())
library(survival)
library(survIDINRI)

# 使用部分数据
dat <- pbc[1:312,]
dat$status <- ifelse(dat$status==2, 1, 0) # 0表示活着，1表示死亡

str(dat)
##### 28.2.1构建变量矩阵--------------------------------------------------------
# 两个只由预测变量组成的矩阵
z.std <- as.matrix(subset(dat, select = c(age, bili, albumin)))
z.new <- as.matrix(subset(dat, select = c(age, bili, albumin, protime)))

##### 28.2.2IDI.INF()函数计算IDI------------------------------------------------
res <- IDI.INF(indata = dat[,c(2,3)],#输入数据，需包含两列：时间（如生存时间）和事件状态（如死亡）
               covs0 = z.std,
               covs1 = z.new,
               t0 = 2000, # 时间点
               npert = 500, # 重抽样次数
               seed1 = 1234 # 设定重抽样的种子数
)
IDI.INF.OUT(res) # 提取结果
##### 28.2.3结果解释------------------------------------------------------------

##     Est.  Lower Upper p-value
## M1 0.020 -0.004 0.055   0.104
## M2 0.202 -0.064 0.382   0.084
## M3 0.011 -0.003 0.033   0.088

"m1：IDI的值，可信区间，P值
m2：NRI的值，可信区间，P值,NRI是连续型NRI
m3：风险分数的中位数提升（median improvement of risk score），可信区间，p值。"

"Continuous NRI=(事件组中概率上升的比例−非事件组中概率上升的比例)/2"
##### 28.2.4结果可视化----------------------------------------------------------
IDI.INF.GRAPH(res)

"由于这个函数计算IDI时不支持使用概率，
只能使用原始数据，所以理论上这个函数无法计算测试集（验证集，外部验证集）的IDI,
而且也不支持计算机器学习模型（比如随机生存森林、生存支持向量机等）的IDI"

