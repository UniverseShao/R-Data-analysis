#基本原理：卡方检验就是统计样本的实际观测值与理论推断值之间的偏离程度
#实际观测值与理论推断值之间的偏离程度就决定卡方值的大小
#如果卡方值越大，二者偏差程度越大   反之，二者偏差越小  
#若两个值完全相等时，卡方值就为0，表明理论值完全符合
#常见用途：检验两个变量之间是否有关系，比如机器学习中的特征选择，以及医学领域
#进行卡方检验的时候
#我们使用gmodels里面的CrossTable()函数非常强大，直接给出所有结果，和SPSS差不多
library(gmodels)
CrossTable(data1$treat, data1$impro, digits = 4, expected = T, chisq = T, fisher = T, mcnemar = T, format = "SPSS")
#
#
#
#
#
#
#
#
#
#
#