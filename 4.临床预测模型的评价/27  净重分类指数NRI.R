"两者加起来是-150，说明实际上新模型比旧模型还多分错了150个人，新模型是更差的"
# 27.1 二分类模型的NRI----------------------------------------------------------
##### 27.1.1 nricens------------------------------------------------------------
rm(list = ls())
library(nricens)
library(survival)
# 只使用部分数据
dat <- pbc[1:312,] 
dat <- dat[dat$time > 2000 | (dat$time < 2000 & dat$status == 2), ]
str(dat) # 数据长这样
# 定义结局事件，0是存活，1是死亡
event <- ifelse(dat$time < 2000 & dat$status == 2, 1, 0)
###### 27.1.1.1两个只由预测变量组成的矩阵---------------------------------------
z.std <- as.matrix(subset(dat, select = c(age, bili, albumin)))
z.new <- as.matrix(subset(dat, select = c(age, bili, albumin, protime)))
###### 27.1.1.2建立2个模型用于比较----------------------------------------------
mstd <- glm(event ~ age + bili + albumin, family = binomial(), 
            data = dat, x=TRUE)
mnew <- glm(event ~ age + bili + albumin + protime, family = binomial(), 
            data = dat, x=TRUE)
###### 27.1.1.3取出模型预测概率-------------------------------------------------
p.std <- mstd$fitted.values
p.new <- mnew$fitted.values
###### 27.1.1.4这3种方法算出来都是一样的结果------------------------------------
# 两个模型
nribin(mdl.std = mstd, mdl.new = mnew, 
       cut = c(0.3,0.7), 
       niter = 500, 
       updown = 'category')
# 结果变量 + 两个只有预测变量的矩阵
nribin(event = event, z.std = z.std, z.new = z.new, 
       cut = c(0.3,0.7), 
       niter = 500, 
       updown = 'category')
# 结果变量 + 两个模型得到的预测概率
nribin(event = event, p.std = p.std, p.new = p.new, 
       cut = c(0.3,0.7), 
       niter = 500, 
       updown = 'category')
###### 27.1.1.5结果解释---------------------------------------------------------
"cut是判断风险高低的阈值，我们使用了0.3,0.7
代表0-0.3是低风险，0.3-0.7是中风险，0.7-1是高风险
niter是使用bootstrap法进行重抽样的次数，默认是1000
updown参数，当设置为category时，表示使用低、中、高风险这种方式"

"case组（n=88）：

重新分对的：3个
重新分错的：1个
净重分类：3-1=2
NRI+净重分类的比例是：2/88=0.022727273

control组（n=144）：

重新分对的：2个
重新分错的：5个
净重分类：2-5=-3
NRI-净重分类的比例是：-3/144=-0.020833333
相加NRI=2/88 + (-3/144) = 0.001893939 绝对NRI=（2-3）/232=-0.000431"


###### 27.1.1.6测试集-----------------------------------------------------------
# 取前100行作为测试集，这个方法是不正规的哈
testset <- dat[1:100,]

# 计算测试集的概率
p.std_test <- predict(mstd, newdata = testset,type = "response")
p.new_test <- predict(mnew, newdata = testset,type = "response")
event_test <- ifelse(testset$time < 2000 & testset$status == 2, 1, 0)
## 结果变量 + 两个模型得到的预测概率
nribin(event = event_test, p.std = p.std_test, p.new = p.new_test, 
       cut = c(0.3,0.7), 
       niter = 500, 
       updown = 'category')


##### 27.1.2 PredictABEL--------------------------------------------------------

#PredictABEL只能计算二分类模型的NRI，除此之外，它还会自动给出IDI
library(PredictABEL)  
# 取出模型预测概率，这个包只能用预测概率计算
p.std = mstd$fitted.values
p.new = mnew$fitted.values 
dat$event <- event
###### 27.1.2.1计算NRI----------------------------------------------------------
reclassification(data = dat,
                 cOutcome = 21, # 结果变量在哪一列
                 predrisk1 = p.std,
                 predrisk2 = p.new,
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

# 27.2 生存模型的NRI------------------------------------------------------------

##### 27.2.1 nricens------------------------------------------------------------
rm(list = ls())
library(nricens)
library(survival)

dat <- pbc[1:312,]
dat$status <- ifelse(dat$status==2, 1, 0) # 0表示活着，1表示死亡
###### 27.2.1.1变量矩阵---------------------------------------------------------
# 两个只由预测变量组成的矩阵
z.std <- as.matrix(subset(dat, select = c(age, bili, albumin)))
z.new <- as.matrix(subset(dat, select = c(age, bili, albumin, protime)))
###### 27.2.1.2cox模型----------------------------------------------------------
# 建立2个cox模型，建立其他模型也可以，只要能计算概率就行
mstd <- coxph(Surv(time,status) ~ age + bili + albumin, data = dat, x=TRUE)
mnew <- coxph(Surv(time,status) ~ age + bili + albumin + protime, data = dat, x=TRUE)
###### 27.2.1.3计算特定时间生存概率---------------------------------------------
# 计算在2000天的模型预测的生存概率
p.std <- get.risk.coxph(mstd, t0=2000)
p.new <- get.risk.coxph(mnew, t0=2000)
"生存模型的NRI只能选择计算某一个时间点的NRI"
###### 27.2.1.4计算NRI----------------------------------------------------------
set.seed(123)
nricens(mdl.std= mstd, mdl.new = mnew, 
        t0 = 2000, 
        cut = c(0.3, 0.7),
        niter = 1000, 
        updown = 'category')

###### 27.2.1.5测试集-----------------------------------------------------------
# 取前100行作为测试集，这个方法并不正规哈
testset <- dat[1:100,]

# 获取测试集在2000天时的生存概率
p.std_test <- 1-c((summary(survfit(mstd, newdata=testset), times=2000)$surv))
p.new_test <- 1-c((summary(survfit(mnew, newdata=testset), times=2000)$surv))
nricens(time = testset$time, event = testset$status,
        p.std = p.std_test, p.new = p.new_test,
        t0 = 2000, 
        cut = c(0.3, 0.7),
        niter = 1000, 
        updown = 'category')

##### 27.2.2 survNRI------------------------------------------------------------
rm(list = ls())
library(survNRI)
library(survival)

# 使用部分数据
dat <- pbc[1:312,]
dat$status <- ifelse(dat$status==2, 1, 0) # 0表示活着，1表示死亡
res <- survNRI(time  = "time", event = "status", 
               model1 = c("age", "bili", "albumin"), # 模型1的自变量
               model2 = c("age", "bili", "albumin", "protime"), # 模型2的自变量
               data = dat, 
               predict.time = 2000, # 预测的时间点
               method = "all", # 所有方法都计算
               bootMethod = "normal",  
               bootstraps = 500, # 重抽样次数
               alpha = .05)
print.survNRI(res)
###### 27.2.2.1结果解释---------------------------------------------------------
##  method     |  event NRI              non-event NRI             NRI 

##   KM        |  0.204 (-0.034,0.443)   0.319 (0.132,0.710)   0.523 (-0.046,0.888)   
##   IPW       |  0.224 (-0.014,0.463)   0.327 (0.141,0.738)   0.552 (-0.036,0.915)   
##   SmoothIPW |  0.196 (-0.041,0.434)   0.314 (0.129,0.696)   0.511 (-0.047,0.873)   
##   SEM       |  0.075 (-0.116,0.266)   0.263 (0.084,0.518)   0.338 (-0.047,0.649)   
##   Combined  |  0.196 (-0.041,0.434)   0.314 (0.129,0.696)   0.511 (-0.048,0.873)   

