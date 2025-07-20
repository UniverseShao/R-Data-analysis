#了解数据的分布状态——是正态分布数据还是偏态分布数据
#图示法和假设检验法两大类
#图示法包括直方图or核密度图和QQ图orP-P图
#常用的假设检验法主要包括Shapiro-Wilk检验（Shapiro检验）和Kolmogorov-Smirnov检验



#直方图
#直方图实现代码：
hist(x)
#注意hist(X)中的x必须是数据
#以mtcars数据库为例子，必须是mtcars$mpg才是一组数据
#对这一组数据画直方图代码就是
hist(mtcars$mpg)
#那么怎么对某一分组变量中的1或者是0组的连续变量数据画图呢？
#举个例子就是画am 是1的mpg直方图？
# 筛选出am等于1的数据
mtcars_am1 <- mtcars[mtcars$am == 1, ]
# 绘制直方图
hist(mtcars_am1$mpg)
#那同时选择那些am列值为1gear列值为4的行，选择所有列组成一个新的子集数据框，该怎么写代码
mtcars_subset <- mtcars[mtcars$am == 1 & mtcars$gear == 4, ]
#那同时选择那些am列值为1gear列值为4的行，选择mpg这一列组成新的子集数据框，该怎么写代码
mtcars_subset_mpg <- mtcars[mtcars$am == 1 & mtcars$gear == 4, "mpg"]
mtcars_subset_mpg_df <- mtcars[mtcars$am == 1 & mtcars$gear == 4, "mpg", drop = FALSE]
#注意在R中当只有一列的数据的时候R会将 这个数据变成一个向量而不是一个数据表
#所以24行虽然是按照函数公式来套的但是得出的不是数据框
# 25行加上drop = FALSE参数可以防止R在结果只有一列时将其简化为一个向量
#那同时选择那些am列值为1gear列值为4的行，选择mpg和disp这两列组成新的子集数据框，该怎么写代码
mtcars_subset_mpg_disp <- mtcars[mtcars$am == 1 & mtcars$gear == 4, c("mpg", "disp")]
#(其实你可以理解是数据表这种结构一个列的数据就是算作是一个向量)

#Q-Q图
#Q-Q图实现代码：
qqnorm(x)# x 是向量注意！
qqline(x)# 这行代码在Q-Q图上添加了一条参考线（qqline）
#如果数据来自正态分布，那么数据点应该大致沿着这条线分布




#假设检验法
#当样本量大于等于50时，采用kolmogorov-Smirnov检验
ks.test(x,"pnorm")
#当样本量小于50时，采用Shapiro-Wilk检验
shapiro.test(x)

