#目前支持以下插补方法
#基于模型的方法

#线性回归
#稳健线性回归
#岭回归/弹性网络/lasso回归
#CART模型（决策树）
#随机森林

#多元插补

#基于最大期望值的方法
#missForest

#Donor imputation (including various donor pool specifications)

#K最近邻法
#sequential hotdeck (LOCF, NOCB)
#random hotdeck
#Predictive mean matching

#其他

#median imputation
#Proxy imputation: 使用其他列的值或使用简单的转换得到的值.
#Apply trained models for imputation purposes.

#impute_<model>(data, formula, [model-specific options])

#impute_rlm: robust linear model
#impute_en: ridge/elasticnet/lasso
#impute_cart: CART
#impute_rf: random forest
#impute_rhd: random hot deck
#impute_shd: sequential hot deck
#impute_knn: k nearest neighbours
#impute_mf: missForest
#impute_em: mv-normal
#impute_const: 用一个固定值插补
#impute_lm: linear regression
#impute_pmm: Hot-deck imputation
#impute_median: 均值插补
#impute_proxy: 自定义公式插补，可以用均值等

#data是需要插补的数据框，输出数据和输入数据结构一样，只不过缺失值被插补了
#formula指定需要插补的列
#[model-specific options]是根据所选模型不同有不同的参数。

#生成数据------------------------------------------------------------------------

library(simputation)
dat <- iris
dat[1:3, 1] <- dat[3:7, 2] <- dat[8:10, 5] <- NA
head(dat, 10)
#线性回归方法插补缺失值-------------------------------------------------------
#根据Sepal.Width和Species这两列，来预测Sepal.Length这一列的缺失值

library(simputation)
da1 <- impute_lm(dat, Sepal.Length ~ Sepal.Width + Species)
head(da1, 10)

#Sepal.Length的第3个值还是NA，这是因为Sepal.Width这一列的第3个值是NA导致的，线性回归不能插补这样的缺失值
#中位数进行插补---------------------------------------------------------------
da2 <- impute_median(da1, Sepal.Length ~ Species) # Species用来分组,相当于根据Species这一列分组求中位数然后分别插补
head(da2, 10)
#决策树方法来插补Species这一列---------------------------------------------------
da3 <- impute_cart(da2, Species ~ .)
head(da3,10)
#使用管道符连接多种插补方法-------------------------------------------------------------
library(magrittr)
da4 <- dat %>% 
  impute_lm(Sepal.Length ~ Sepal.Width + Species) %>%
  impute_median(Sepal.Length ~ Species) %>%
  impute_cart(Species ~ .)
head(da4,10)
#使用一个固定值进行插补------------------------------------------------------------
head(dat, 10)
da4 <- impute_const(dat, Sepal.Length ~ 7)#使用7插补
head(da4,10)
#复制另一列的值进行插补-----------------------------------------------------------
da4 <- impute_proxy(dat, Sepal.Length ~ Sepal.Width)
head(da4,10)
#对多列使用同一种插补方法-----------------------------------------------------------
#根据Petal.Length和Species这两列使用rlm方法插补Sepal.Length和Sepal.Width
da5 <- impute_rlm(dat, Sepal.Length + Sepal.Width ~ Petal.Length + Species)
head(da5, 10)
#支持tidyverse中的.和- -----------------------------------------------------------
da6 <- impute_lm(dat, . - Species ~ 0 + Species, add_residual = "normal") # Species用来分组
head(da6)
#在 %>% 管道中：. 代表前一步的结果（此时通常需要空格）
#在公式中：. 是特殊符号
#公式 . - Species ~ 0 + Species 的特殊性-------------------------------------------------
#按Species分组（分类变量的每个水平单独处理）
#无截距项（0 +强制模型通过原点）
#等价于均值插补的数学原理
# Sepal.Length = 0 + β * Species_setosa
# 因此插补值 = 该组的样本均值 + 随机残差

#分组插补------------------------------------------------------------------------------------------------

# 新建一个含缺失值的数据框
dat <- iris
dat[1:3,1] <- dat[3:7,2] <- NA
#复习<-的深层含义
#为什么dat[1:3,1] <- dat[3:7,2] <- NA可以直接永久修改数据框而不是临时
#因为 <- 是赋值操作而不是传递操作
#赋值是从右到左 而%>%是从左到右 如果使用%>%没有在前面使用 <- 则是临时修改
#如果是dat <- dat[1:3,1] <- dat[3:7,2] <- NA那么最后整个dat全是NA

# 根据Species分组，然后再插补
da8 <- impute_lm(dat, Sepal.Length ~ Petal.Width | Species)
head(da8, 10)
#分组插补的核心思想------------------------------------------------------------------------------------------------
#第一步分组：将数据按Species（setosa/versicolor/virginica）拆分为3个子集

#第二步插补：在每个物种组内独立建立Sepal.Length ~ Petal.Width的线性模型

#组间隔离：setosa组的模型不会用于versicolor组的插补，反之亦然

#为什么需要分组插补？

#setosa物种：花萼长度（Sepal.Length）与花瓣宽度（Petal.Width）呈强相关

#virginica物种：两者相关性弱

#全局插补问题：如果用所有数据统一建模，会忽略组间差异，导致插补偏差


#等价于和dplyr包的group_by连用
library(magrittr)
library(dplyr)
dat <- iris
dat[1:3,1] <- dat[3:7,2] <- NA
dat %>% group_by(Species) %>% 
  impute_lm(Sepal.Length ~ Petal.Width)

#使用impute_proxy指定自己的插补方法------------------------------------------------------------------------------------------------
#impute_proxy允许自定义formula
#自定义一个robust ratio imputation方法进行插补
#基于分组比例关系进行缺失值插补-----------------------------------------------------------------------------------------------
dat <- iris
dat[1:3,1] <- dat[3:7,2] <- NA
dat <- impute_proxy(dat, Sepal.Length ~ median(Sepal.Length,na.rm=TRUE)/median(Sepal.Width, na.rm=TRUE) * Sepal.Width | Species)
head(dat)
#na.rm = TRUE：自动忽略数据中的NA（缺失值），仅计算有效值
#na.rm = FALSE（默认值）：如果数据包含NA，计算结果也会返回NA
#自定义使用均值进行插补-------------------------------------------------------------------------------------------------------
dat <- iris
dat[1:3,1] <- dat[3:7,2] <- NA

dat <- impute_proxy(dat, Sepal.Length ~ mean(Sepal.Length,na.rm=TRUE))
head(dat)

#自定义分组后再使用均值进行插补-------------------------------------------------------------------------------------------------------
dat <- iris
dat[1:3,1] <- dat[3:7,2] <- NA

dat <- impute_proxy(dat, Sepal.Length ~ mean(Sepal.Length,na.rm=TRUE) | Species)
head(dat)

#使用在其他数据集中训练过的模型插补数据-------------------------------------------------------------------------------------------------------
m <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
dat <- iris
dat[1:3,1] <- dat[3:7,2] <- NA
head(dat,10)
dat <- impute(dat, Sepal.Length ~ m)
head(dat)




