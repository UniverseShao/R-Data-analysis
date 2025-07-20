# 1.对数线性模型和卡方检验------------------------------------------------------

# 不考虑护理地点的影响，就是2*2列联表
M <- matrix(c(20,373,6,316),nrow = 2, byrow = T,
            dimnames = list(`产前护理量`=c("少","多"),
                            `存活情况`=c("死","活")))
M

# 进行卡方检验
chisq.test(M,correct = F)


# 诊所甲
M1 <- matrix(c(3,176,4,293),nrow = 2, byrow = T,
             dimnames = list(`产前护理量`=c("少","多"),
                             `存活情况`=c("死","活")))
M1

# 进行卡方检验
chisq.test(M1,correct = F)

# 诊所乙
M2 <- matrix(c(17,197,2,23),nrow = 2, byrow = T,
             dimnames = list(`产前护理量`=c("少","多"),
                             `存活情况`=c("死","活")))
M2

# 产前护理量和护理地点之间的独立性检验
M3 <- matrix(c(179,297,214,25),nrow = 2, byrow = T,
             dimnames = list(`产前护理量`=c("少","多"),
                             `护理地点`=c("甲","乙")))
M3

# 2. 2*2列联表------------------------------------------------------------------ 

data17_2 <- haven::read_sav("datasets/例17-02.sav",encoding = "GBK")
#str(data17_2)
data17_2 <- haven::as_factor(data17_2)
str(data17_2)
## tibble [4 × 3] (S3: tbl_df/tbl/data.frame)
##  $ 性别: Factor w/ 2 levels "男性","女性": 1 1 2 2
##  $ 血压: Factor w/ 2 levels "正常血压","高血压": 1 2 1 2
##  $ 频数: num [1:4] 579 485 1032 483
##   ..- attr(*, "format.spss")= chr "F8.0"
data17_2


M <- matrix(data17_2$频数,nrow = 2,byrow = T,
            dimnames = list(trt = c("男性", "女性"),
                            effect = c("正常","高血压")))

M

chisq.test(M,correct = F)

fm <- loglin(M, margin=list(1,2), fit=T, param=T)
fm$lrt
fm$pearson
1 - pchisq(fm$lrt, fm$df) # 计算似然比G^2^的P值
1 - pchisq(fm$pearson, fm$df) # 计算卡方的P值


library(MASS)
# 不饱和模型（没有交互项）
f <- loglm(`频数` ~ `性别` + `血压`, data = data17_2)
f

f1 <- update(f, ~ .^2) # 直接更新模型
anova(f, f1) # 比较饱和模型和不饱和模型

# 或者重新拟合一个饱和模型
f2 <- loglm(`频数` ~ `性别` * `血压`, data = data17_2)
anova(f,f2) # 比较饱和模型和不饱和模型


f3 <- glm(`频数` ~ `性别` * `血压`, data = data17_2, family = poisson())
#f3
coef(f3) # 查看系数


library(Crosstabs.Loglinear)

LOGLINEAR(data = data17_2,
          data_type = "counts",
          variables = c("性别","血压"),
          Freq = "频数")

# 3.R*C表-----------------------------------------------------------------------

data17_3 <- haven::read_sav("datasets/例17-03.sav",encoding = "GBK")
data17_3 <- haven::as_factor(data17_3)
str(data17_3)
data17_3
f <- loglm(`频数` ~ `治疗方法`+`治疗效果`,data = data17_3)
f
f1 <- update(f, ~ .^2) # 饱和模型
anova(f,f1) # 拟合优度检验

# 4.三维列联表------------------------------------------------------------------
data17_4 <- haven::read_sav("datasets/例17-04.sav",encoding = "GBK")
data17_4 <- haven::as_factor(data17_4)
str(data17_4)
data17_4

library(MASS)

# 无交互项，只有主效应，完全独立模型
f1 <- loglm(`频数` ~ `人群分组`+`口服避孕药暴露水平`+`基因型`,data = data17_4)
# 添加1阶交互效应，即只有两个变量的交互，没有3变量交互
f2 <- update(f1, ~ .^2)
# 添加2阶交互效应，即饱和模型，既有两变量交互，又有3变量交互
f3 <- update(f1, ~ .^3)

f <- step(f3, direction = "both")
anova(f3,f)





