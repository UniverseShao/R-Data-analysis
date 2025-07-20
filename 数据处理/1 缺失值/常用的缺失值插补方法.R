#均值/中位数/最大值/最小值等-------------------------------------------------------
#新建一个有缺失值的数据
set.seed(12)
df <- data.frame(a = sample(c(NA,NA,1:5),10, replace = T),#抽样函数允许重复抽样
                 b = sample(c(NA,NA,1:10),10, replace = T),
                 c = sample(c(NA,1:5),10, replace = T),
                 d = sample(c(NA,1:3),10, replace = T)
)

df
table(is.na(df))
#均值插补--------------------------------------------------------------------------------------------------------------
# 用每一列的均值插补
df1 <- sapply(df, function(x){
  x[is.na(x)] <- mean(x, na.rm=T)
  x
})
df1
#中位数插补--------------------------------------------------------------------------
# 用每一列的中位数插补
df2 <- sapply(df, function(x){
  x[is.na(x)] <- median(x, na.rm=T)
  x
})

table(is.na(df2))
#Hmisc包实现--------------------------------------------------------------------
library(Hmisc)

impute(df$a, fun = mean) # median, max, min
# 支持用常数插补！
impute(df$a, fun = 2)

#其他常见R包--------------------------------------------------------------------
library(impute)

impute.knn(as.matrix(df))$data
#VIM----------------------------------------------------------------------------------
library(VIM)

kNN(df)[1:ncol(df)]

#missForest--------------------------------------------------------------------------------
library(missForest)

missForest(df)$ximp



















