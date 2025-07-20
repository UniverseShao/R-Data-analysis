# 1.演示数据--------------------------------------------------------------------
rm(list = ls())
library(haven)
df <- read_sav("C:/Users/Administrator/Desktop/R脚本(SWY精心编辑版)/Medical Statistics/datasets/例22-02.sav")
names(df) <- c("年","月","门诊人次","出院人数","病床利用率","病床周转次数","平均住院天数","治愈好转率","病死率","诊断符合率","抢救成功率")
str(df)
psych::headTail(df)

# 2.判断需要提取的因子个数------------------------------------------------------
# 只用后面9列数据
df.use <- df[,-c(1,2)]

library(psych)

# 碎石图
fa.parallel(df.use, fa = "both",fm="ml")


# 进行因子分析，首先9个因子用一下看看结果再说，最大似然法，不旋转
fa.res <- fa(df.use, nfactors = 9, rotate = "none", fm="ml")
fa.res

# 选择4个因子，不旋转，最大似然法
fa.res <- fa(df.use, nfactors = 4, rotate = "none", fm="ml")
fa.res

# 选择4个因子，最大方差旋转，最大似然法
fa.res <- fa(df.use, nfactors = 4, rotate = "varimax", fm="ml")
fa.res

fa.diagram(fa.res)

factor.plot(fa.res)


